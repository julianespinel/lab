"""
ERPNext service for handling transactions and platform fees
"""
import logging
import requests
from decimal import Decimal
from typing import Dict, Any, Optional
from django.conf import settings
from django.contrib.auth.models import User
from ledger.accounting_provider import AccountingProvider

logger = logging.getLogger(__name__)


class ERPNextService(AccountingProvider):
    """Service class for interacting with ERPNext"""

    def __init__(self):
        """Initialize the ERPNext service"""
        self.base_url = settings.ERPNEXT_BASE_URL
        self.api_key = settings.ERPNEXT_API_KEY
        self.api_secret = settings.ERPNEXT_API_SECRET
        self.session = requests.Session()

    def initialize(self) -> bool:
        """Initialize the provider connection"""
        try:
            # Set up authentication headers
            self.session.headers.update({
                'Authorization': f'token {self.api_key}:{self.api_secret}',
                'Content-Type': 'application/json'
            })
            return self.is_healthy()
        except Exception as e:
            logger.error(f"Failed to initialize ERPNext service: {e}")
            return False

    def is_healthy(self) -> bool:
        """Check if the ERPNext service is healthy and accessible"""
        try:
            response = self.session.get(f'{self.base_url}/api/method/ping')
            return response.status_code == 200
        except Exception as e:
            logger.error(f"ERPNext health check failed: {e}")
            return False

    def create_user_transaction(self, source_user: User,
                               destination_user: User,
                               amount: Decimal) -> Dict[str, Any]:
        """
        Create a transaction in ERPNext with platform fee

        Args:
            source_user: User sending the money
            destination_user: User receiving the money
            amount: Amount to transfer

        Returns:
            Dict containing transaction information
        """
        try:
            # Convert platform fee from cents to dollars
            platform_fee_cents = settings.PLATFORM_FEE_CENTS
            platform_fee = Decimal(platform_fee_cents) / 100
            total_deducted = amount + platform_fee

            # Create Journal Entry in ERPNext
            journal_entry_data = {
                'doctype': 'Journal Entry',
                'voucher_type': 'Journal Entry',
                'posting_date': None,  # Will use today's date
                'accounts': [
                    {
                        'account': f'Users - {source_user.username}',
                        'credit_in_account_currency': float(total_deducted),
                        'user_remark': f'Transfer to {destination_user.username}'
                    },
                    {
                        'account': f'Users - {destination_user.username}',
                        'debit_in_account_currency': float(amount),
                        'user_remark': f'Received from {source_user.username}'
                    },
                    {
                        'account': 'Platform Fees',
                        'debit_in_account_currency': float(platform_fee),
                        'user_remark': 'Platform transaction fee'
                    }
                ],
                'user_remark': f'Transfer from {source_user.username} '
                              f'to {destination_user.username}'
            }

            response = self.session.post(
                f'{self.base_url}/api/resource/Journal%20Entry',
                json=journal_entry_data
            )

            if response.status_code == 200:
                journal_entry = response.json()['data']
                return {
                    'success': True,
                    'total_deducted': str(total_deducted),
                    'erpnext_transaction_id': journal_entry.get('name'),
                    'erpnext_reference': journal_entry.get('name'),
                    'erpnext_status': 'completed',
                    'platform_fee_amount': str(platform_fee_cents),
                }
            else:
                error_msg = f"ERPNext API error: {response.status_code}"
                logger.error(f"Failed to create transaction: {error_msg}")
                return {
                    'success': False,
                    'error': error_msg,
                    'erpnext_status': 'failed',
                }

        except Exception as e:
            logger.error(f"Failed to create transaction in ERPNext: {e}")
            return {
                'success': False,
                'error': str(e),
                'erpnext_status': 'failed',
            }

    def get_user_balance(self, user: User,
                        asset: str = None) -> Optional[Decimal]:
        """
        Get user's balance from ERPNext

        Args:
            user: User to get balance for
            asset: Asset type (not used in ERPNext implementation)

        Returns:
            User's balance or None if error
        """
        try:
            account_name = f'Users - {user.username}'

            # Get account balance from ERPNext
            response = self.session.get(
                f'{self.base_url}/api/method/erpnext.accounts.utils.get_balance_on',
                params={
                    'account': account_name,
                    'date': None  # Current date
                }
            )

            if response.status_code == 200:
                balance_data = response.json()
                return Decimal(str(balance_data.get('message', 0)))
            else:
                logger.error(f"Failed to get balance for {user.username}: "
                           f"{response.status_code}")
                return None

        except Exception as e:
            logger.error(f"Failed to get balance for user {user.username}: {e}")
            return None

    def get_platform_fees_total(self,
                               asset: str = None) -> Optional[Decimal]:
        """
        Get total platform fees collected from ERPNext

        Args:
            asset: Asset type (not used in ERPNext implementation)

        Returns:
            Total fees collected or None if error
        """
        try:
            # Get platform fees account balance
            response = self.session.get(
                f'{self.base_url}/api/method/erpnext.accounts.utils.get_balance_on',
                params={
                    'account': 'Platform Fees',
                    'date': None  # Current date
                }
            )

            if response.status_code == 200:
                balance_data = response.json()
                return Decimal(str(balance_data.get('message', 0)))
            else:
                logger.error(f"Failed to get platform fees total: "
                           f"{response.status_code}")
                return None

        except Exception as e:
            logger.error(f"Failed to get platform fees total: {e}")
            return None

    def get_transaction_details(self,
                               transaction_id: str) -> Optional[Dict[str, Any]]:
        """
        Get transaction details from ERPNext

        Args:
            transaction_id: The transaction ID to retrieve

        Returns:
            Transaction details dictionary or None if error
        """
        try:
            response = self.session.get(
                f'{self.base_url}/api/resource/Journal%20Entry/{transaction_id}'
            )

            if response.status_code == 200:
                return response.json()['data']
            else:
                logger.error(f"Failed to get transaction details for "
                           f"{transaction_id}: {response.status_code}")
                return None

        except Exception as e:
            logger.error(f"Failed to get transaction details for "
                        f"{transaction_id}: {e}")
            return None

    def format_transaction_details(self, transaction_details: Any) -> str:
        """
        Format ERPNext transaction details for display

        Args:
            transaction_details: Transaction details from ERPNext

        Returns:
            Formatted string representation of transaction details
        """
        if not transaction_details:
            return "No transaction details available"

        output = []
        output.append("\n" + "="*50)
        output.append("ERPNEXT TRANSACTION DETAILS")
        output.append("="*50)

        output.append(f"\nTransaction ID: {transaction_details.get('name', 'N/A')}")
        output.append(f"Date: {transaction_details.get('posting_date', 'N/A')}")
        output.append(f"Status: {transaction_details.get('docstatus', 'N/A')}")

        accounts = transaction_details.get('accounts', [])
        if accounts:
            output.append("\nAccount Entries:")
            for i, account in enumerate(accounts):
                output.append(f"  {i+1}. {account.get('account', 'N/A')}")
                if account.get('debit_in_account_currency'):
                    output.append(f"     Debit: {account.get('debit_in_account_currency')}")
                if account.get('credit_in_account_currency'):
                    output.append(f"     Credit: {account.get('credit_in_account_currency')}")
                if account.get('user_remark'):
                    output.append(f"     Remark: {account.get('user_remark')}")

        output.append("\n" + "="*50)
        return "\n".join(output)

    def revert_transaction(self, transaction_details: Any,
                          original_id: str) -> Dict[str, Any]:
        """
        Revert a transaction in ERPNext

        Args:
            transaction_details: Original transaction details
            original_id: Original transaction ID

        Returns:
            Dictionary with success status and reversal details
        """
        try:
            # Cancel the original journal entry
            cancel_response = self.session.put(
                f'{self.base_url}/api/resource/Journal%20Entry/{original_id}',
                json={'docstatus': 2}  # 2 = Cancelled in ERPNext
            )

            if cancel_response.status_code == 200:
                return {
                    'success': True,
                    'reversal_ids': [original_id],
                    'reversal_details': [{
                        'transaction_id': original_id,
                        'status': 'cancelled',
                        'method': 'cancellation'
                    }],
                    'total_reversals': 1,
                }
            else:
                error_msg = f"Failed to cancel transaction: {cancel_response.status_code}"
                logger.error(error_msg)
                return {
                    'success': False,
                    'error': error_msg
                }

        except Exception as e:
            logger.error(f"Failed to revert transaction {original_id}: {e}")
            return {
                'success': False,
                'error': str(e)
            }


# Create a singleton instance
erpnext_service = ERPNextService()
