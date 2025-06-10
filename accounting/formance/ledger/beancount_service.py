"""
Beancount service for handling transactions and platform fees using beancount
"""
import logging
import uuid
from datetime import datetime
from decimal import Decimal
from typing import Dict, Any, Optional
from pathlib import Path
import re

from django.conf import settings
from django.contrib.auth.models import User
from beancount.loader import load_file
from beancount.query import query
from beancount.core.data import Transaction as BeancountTransaction

from ledger.accounting_provider import AccountingProvider

logger = logging.getLogger(__name__)


class BeancountService(AccountingProvider):
    """Service class for interacting with Beancount ledger files"""

    def __init__(self):
        """Initialize the Beancount service"""
        self.ledger_file = Path(settings.BEANCOUNT_LEDGER_FILE)
        self.currency = settings.BEANCOUNT_CURRENCY
        self._ensure_ledger_file_exists()

    def _ensure_ledger_file_exists(self):
        """Ensure the beancount ledger file exists with basic structure"""
        if not self.ledger_file.exists():
            # Create the directory if it doesn't exist
            self.ledger_file.parent.mkdir(parents=True, exist_ok=True)

            # Create basic beancount file structure
            initial_content = f"""; Beancount ledger file
; Currency: {self.currency}

; Account declarations
1900-01-01 open Assets:Platform:Cash
1900-01-01 open Assets:Platform:Fees
1900-01-01 open Liabilities:Users
1900-01-01 open Equity:Opening-Balances

"""
            with open(self.ledger_file, 'w') as f:
                f.write(initial_content)
            logger.info(
                f"Created new beancount ledger file: {self.ledger_file}"
            )

    def _get_user_account(self, user: User) -> str:
        """Get the beancount account name for a user"""
        # Sanitize username according to Beancount naming conventions
        # Each component must start with capital letter or number
        # and contain only letters, numbers, or dashes
        username = user.username

        # Replace invalid characters with dashes
        sanitized = re.sub(r'[^A-Za-z0-9-]', '-', username)

        # Ensure it starts with capital letter or number
        if sanitized and not (sanitized[0].isupper() or
                             sanitized[0].isdigit()):
            sanitized = sanitized.capitalize()

        # Handle edge case of empty or all-invalid username
        if not sanitized or sanitized == '-':
            sanitized = f"User{user.id}"

        return f"Liabilities:Users:{sanitized}"

    def _get_platform_account(self) -> str:
        """Get the platform fees account name"""
        return "Assets:Platform:Fees"

    def _append_transaction_to_file(self, transaction_text: str):
        """Append a transaction to the beancount file"""
        with open(self.ledger_file, 'a') as f:
            f.write(f"\n{transaction_text}\n")

    def _generate_transaction_id(self) -> str:
        """Generate a unique transaction ID"""
        return str(uuid.uuid4())

    def create_user_transaction(self, source_user: User,
                                destination_user: User,
                                amount: Decimal) -> Dict[str, Any]:
        """
        Create a transaction in Beancount with platform fee

        This creates a single beancount transaction with multiple postings:
        1. User-to-user transfer
        2. Platform fee deduction from source user

        Args:
            source_user: User sending the money
            destination_user: User receiving the money
            amount: Amount to transfer

        Returns:
            Dict containing detailed transaction information
        """
        try:
            transaction_id = self._generate_transaction_id()
            date = datetime.now().date()

            # Calculate amounts
            transfer_amount = amount
            platform_fee = Decimal(settings.PLATFORM_FEE_CENTS) / 100
            total_deducted = transfer_amount + platform_fee

            # Get account names
            source_account = self._get_user_account(source_user)
            dest_account = self._get_user_account(destination_user)
            platform_account = self._get_platform_account()

            # Ensure user accounts exist
            self._ensure_user_account_exists(source_user)
            self._ensure_user_account_exists(destination_user)

            # Create beancount transaction text
            transaction_text = f"""{date} * "Transfer from {source_user.username} to {destination_user.username}"
  id: "{transaction_id}"
  type: "user_transfer"
  source_user_id: "{source_user.id}"
  destination_user_id: "{destination_user.id}"
  {source_account}  -{total_deducted:.2f} {self.currency}
  {dest_account}     {transfer_amount:.2f} {self.currency}
  {platform_account} {platform_fee:.2f} {self.currency}"""

            # Append to file
            self._append_transaction_to_file(transaction_text)

            logger.info(
                f"Beancount transaction created: {source_user.username} -> "
                f"{destination_user.username}, amount: ${transfer_amount}, "
                f"fee: ${platform_fee}, ID: {transaction_id}"
            )

            return {
                "success": True,
                "total_deducted": str(total_deducted),
                "beancount_transaction_id": transaction_id,
                "beancount_status": "completed",
                "platform_fee_amount": str(platform_fee * 100),  # in cents
                "transfer_amount": str(amount),
            }

        except Exception as e:
            logger.error(f"Failed to create beancount transaction: {e}")
            return {
                "success": False,
                "error": str(e),
                "beancount_status": "failed",
                "platform_fee_amount": str(settings.PLATFORM_FEE_CENTS),
            }

    def _ensure_user_account_exists(self, user: User):
        """Ensure user account declaration exists in the beancount file"""
        account_name = self._get_user_account(user)

        # Read the file to check if account already exists
        try:
            with open(self.ledger_file, 'r') as f:
                content = f.read()

            if f"open {account_name}" not in content:
                # Add account declaration
                account_declaration = f"1900-01-01 open {account_name}\n"

                # Insert after other account declarations
                lines = content.split('\n')
                insert_index = 0
                for i, line in enumerate(lines):
                    if line.strip().startswith('open '):
                        insert_index = i + 1

                lines.insert(insert_index, account_declaration)

                with open(self.ledger_file, 'w') as f:
                    f.write('\n'.join(lines))

                logger.info(f"Added account declaration for {account_name}")

        except Exception as e:
            logger.error(
                f"Failed to ensure account exists for {user.username}: {e}"
            )

    def _get_account_balance(self, account_name: str) -> Optional[Decimal]:
        try:
            entries, errors, options_map = load_file(str(self.ledger_file))

            if errors:
                logger.warning(f"Beancount file has errors: {errors}")

            # Use a simpler balance query that returns just the number
            query_str = (f"SELECT account, currency, sum(position) WHERE "
                        f"account ~ '{account_name}' GROUP BY account, currency")

            result_types, result_rows = query.run_query(
                entries, options_map, query_str, numberify=True
            )

            if not result_rows or len(result_rows) == 0:
                return Decimal('0.00')

            for row in result_rows:
                if row[1] == self.currency:
                    return Decimal(str(row[2]))
        except Exception as e:
            logger.error(f"Failed to get balance for account {account_name}: {e}")
            return None

    def get_user_balance(self, user: User, asset: str = None) -> Optional[Decimal]:
        return self._get_account_balance(self._get_user_account(user))

    def get_platform_fees_total(self, asset: str = None) -> Optional[Decimal]:
        return self._get_account_balance(self._get_platform_account())

    def get_transaction_details(self,
                                transaction_id: str) -> Optional[Dict[str, Any]]:
        """
        Get transaction details from Beancount

        Args:
            transaction_id: The transaction ID to retrieve

        Returns:
            Transaction details dictionary or None if error
        """
        try:
            entries, errors, _ = load_file(str(self.ledger_file))

            if errors:
                logger.warning(f"Beancount file has errors: {errors}")

            # Find transaction by ID in metadata
            for entry in entries:
                if (isinstance(entry, BeancountTransaction) and
                    hasattr(entry, 'meta') and
                    entry.meta.get('id') == transaction_id):

                    return {
                        'id': transaction_id,
                        'date': entry.date,
                        'narration': entry.narration,
                        'postings': [
                            {
                                'account': posting.account,
                                'amount': str(posting.units.number),
                                'currency': posting.units.currency
                            }
                            for posting in entry.postings
                        ],
                        'metadata': entry.meta
                    }

            return None

        except Exception as e:
            logger.error(
                f"Failed to get transaction details for {transaction_id}: {e}"
            )
            return None

    def format_transaction_details(self, transaction_details: Any) -> str:
        """
        Format transaction details for display

        Args:
            transaction_details: Transaction details from Beancount

        Returns:
            Formatted string representation of transaction details
        """
        if not transaction_details:
            return "No transaction details available"

        output = []
        output.append("\n" + "="*50)
        output.append("BEANCOUNT TRANSACTION DETAILS")
        output.append("="*50)

        output.append(f"ID: {transaction_details.get('id', 'N/A')}")
        output.append(f"Date: {transaction_details.get('date', 'N/A')}")
        output.append(f"Description: {transaction_details.get('narration', 'N/A')}")

        postings = transaction_details.get('postings', [])
        if postings:
            output.append("\nPostings:")
            for i, posting in enumerate(postings):
                output.append(f"  {i+1}. {posting['account']}: "
                            f"{posting['amount']} {posting['currency']}")

        metadata = transaction_details.get('metadata', {})
        if metadata:
            output.append("\nMetadata:")
            for key, value in metadata.items():
                if key not in ['id']:  # Skip already displayed fields
                    output.append(f"  {key}: {value}")

        output.append("\n" + "="*50)
        return "\n".join(output)

    def revert_transaction(self, transaction_details: Any,
                           original_id: str) -> Dict[str, Any]:
        """
        Revert a transaction in Beancount by creating a reversing entry

        Args:
            transaction_details: Original transaction details
            original_id: Original transaction ID

        Returns:
            Dictionary with success status and reversal details
        """
        try:
            # Get original transaction details if not provided
            if not transaction_details:
                transaction_details = self.get_transaction_details(original_id)

            if not transaction_details:
                return {
                    'success': False,
                    'error': f'Original transaction {original_id} not found'
                }

            reversal_id = self._generate_transaction_id()
            date = datetime.now().date()

            # Create reversal transaction text
            original_postings = transaction_details.get('postings', [])
            reversal_postings = []

            for posting in original_postings:
                # Reverse the amounts
                amount = Decimal(posting['amount'])
                reversed_amount = -amount
                reversal_postings.append(
                    f"  {posting['account']}  {reversed_amount:.2f} {posting['currency']}"
                )

            transaction_text = f"""{date} * "Reversal of transaction {original_id}"
  id: "{reversal_id}"
  type: "reversal"
  original_transaction_id: "{original_id}"
{chr(10).join(reversal_postings)}"""

            # Append to file
            self._append_transaction_to_file(transaction_text)

            logger.info(
                f"Beancount reversal transaction created for {original_id}, "
                f"reversal ID: {reversal_id}"
            )

            return {
                'success': True,
                'reversal_ids': [reversal_id],
                'reversal_details': [
                    {
                        'posting_index': i + 1,
                        'account': posting['account'],
                        'reversed_amount': -Decimal(posting['amount'])
                    }
                    for i, posting in enumerate(original_postings)
                ],
                'total_reversals': len(original_postings),
                'reversal_transaction_id': reversal_id
            }

        except Exception as e:
            logger.error(f"Failed to revert transaction {original_id}: {e}")
            return {
                'success': False,
                'error': str(e)
            }

    def initialize(self) -> bool:
        """Initialize the provider connection"""
        try:
            self._ensure_ledger_file_exists()
            # Test that we can load the file
            load_file(str(self.ledger_file))
            return True
        except Exception as e:
            logger.error(f"Failed to initialize Beancount service: {e}")
            return False

    def is_healthy(self) -> bool:
        """Check if the provider service is healthy and accessible"""
        try:
            # Check if file exists and is readable
            if not self.ledger_file.exists():
                return False

            # Try to load the file to check for syntax errors
            entries, errors, _ = load_file(str(self.ledger_file))

            # Allow warnings but not critical errors
            critical_errors = [e for e in errors
                             if hasattr(e, 'level') and e.level == 'ERROR']
            return len(critical_errors) == 0

        except Exception:
            return False


# Create a singleton instance
beancount_service = BeancountService()
