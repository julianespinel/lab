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

    def _get_default_company(self) -> str:
        """
        Get the default company name from ERPNext

        Returns:
            Company name or 'Default Company' as fallback
        """
        try:
            response = self.session.get(
                f'{self.base_url}/api/resource/Company',
                params={'limit': 1}
            )

            if response.status_code == 200:
                companies = response.json().get('data', [])
                if companies:
                    return companies[0].get('name', 'Default Company')

            return 'Default Company'

        except Exception as e:
            logger.error(f"Error getting default company: {e}")
            return 'Default Company'

    def _find_parent_account(self, account_type: str) -> Optional[str]:
        """
        Find an appropriate parent account for the given account type

        Args:
            account_type: Type of account (Asset, Income, etc.)

        Returns:
            Parent account name or None if not found
        """
        logger.info(f"Starting _find_parent_account for type: {account_type}")
        try:
            # Map account types to likely parent account names
            parent_mappings = {
                'Asset': ['Current Assets', 'Assets'],
                'Income': ['Income', 'Revenue', 'Direct Income'],
                'Liability': ['Current Liabilities', 'Liabilities'],
                'Expense': ['Expenses', 'Direct Expenses']
            }

            potential_parents = parent_mappings.get(account_type, ['Assets'])
            logger.info(f"Potential parent accounts: {potential_parents}")

            for parent_name in potential_parents:
                logger.info(f"Checking for parent account: {parent_name}")
                filter_str = f'[["account_name", "=", "{parent_name}"]]'
                response = self.session.get(
                    f'{self.base_url}/api/resource/Account',
                    params={'filters': filter_str}
                )
                logger.info(f"Parent check response for {parent_name}: {response.status_code}")

                if response.status_code == 200:
                    accounts = response.json().get('data', [])
                    if accounts:
                        logger.info(f"Found parent account: {parent_name}")
                        return parent_name

            # If no specific parent found, try to find any root account
            logger.info(f"No standard parent found, looking for any group account of type {account_type}")
            filter_str = (f'[["account_type", "=", "{account_type}"], '
                         f'["is_group", "=", 1]]')
            response = self.session.get(
                f'{self.base_url}/api/resource/Account',
                params={
                    'filters': filter_str,
                    'limit': 1
                }
            )
            logger.info(f"Group account search response: {response.status_code}")

            if response.status_code == 200:
                accounts = response.json().get('data', [])
                if accounts:
                    parent_name = accounts[0].get('name')
                    logger.info(f"Found group account as parent: {parent_name}")
                    return parent_name

            logger.warning(f"No parent account found for type: {account_type}")
            return None

        except Exception as e:
            logger.error(f"Error finding parent account for "
                        f"{account_type}: {e}")
            return None

    def _create_default_company(self) -> bool:
        """
        Create a default company if none exists

        Returns:
            True if company exists or was created successfully
        """
        logger.info("Starting _create_default_company")
        try:
            # Check if any company exists
            logger.info("Checking if company exists")
            response = self.session.get(
                f'{self.base_url}/api/resource/Company',
                params={'limit': 1}
            )
            logger.info(f"Company check response status: {response.status_code}")

            if response.status_code == 200:
                companies = response.json().get('data', [])
                if companies:
                    logger.info(f"Company already exists: {companies[0].get('name')}")
                    return True  # Company already exists

            # Create default company
            logger.info("Creating default company")
            company_data = {
                'doctype': 'Company',
                'company_name': 'Default Company',
                'abbr': 'DC',
                'default_currency': 'USD',
                'country': 'United States'
            }

            create_response = self.session.post(
                f'{self.base_url}/api/resource/Company',
                json=company_data
            )
            logger.info(f"Company creation response status: {create_response.status_code}")

            if create_response.status_code == 200:
                logger.info("Successfully created default company")
                return True
            else:
                logger.error(f"Failed to create company: "
                            f"{create_response.status_code}")
                try:
                    logger.error(f"Company creation error: "
                                f"{create_response.json()}")
                except Exception:
                    logger.error(f"Company creation error: "
                                f"{create_response.text}")
                return False

        except Exception as e:
            logger.error(f"Exception in _create_default_company: {e}")
            return False

    def _create_root_accounts(self) -> bool:
        """
        Create basic root accounts for the chart of accounts

        Returns:
            True if root accounts exist or were created successfully
        """
        logger.info("Starting _create_root_accounts")
        try:
            company_name = self._get_default_company()
            logger.info(f"Using company name: {company_name}")

            # Define root accounts to create
            root_accounts = [
                {
                    'account_name': 'Assets',
                    'account_type': 'Asset',
                    'is_group': 1,
                    'root_type': 'Asset'
                },
                {
                    'account_name': 'Current Assets',
                    'account_type': 'Asset',
                    'is_group': 1,
                    'parent_account': 'Assets'
                },
                {
                    'account_name': 'Income',
                    'account_type': 'Income',
                    'is_group': 1,
                    'root_type': 'Income'
                },
                {
                    'account_name': 'Liabilities',
                    'account_type': 'Liability',
                    'is_group': 1,
                    'root_type': 'Liability'
                },
                {
                    'account_name': 'Expenses',
                    'account_type': 'Expense',
                    'is_group': 1,
                    'root_type': 'Expense'
                }
            ]

            logger.info(f"Creating {len(root_accounts)} root accounts")
            for i, account_info in enumerate(root_accounts):
                account_name = account_info['account_name']
                logger.info(f"Processing root account {i+1}/{len(root_accounts)}: {account_name}")

                # Check if account already exists
                filter_str = f'[["account_name", "=", "{account_name}"]]'
                logger.info(f"Checking if {account_name} exists")
                response = self.session.get(
                    f'{self.base_url}/api/resource/Account',
                    params={'filters': filter_str}
                )
                logger.info(f"Account check response for {account_name}: {response.status_code}")

                if response.status_code == 200:
                    accounts = response.json().get('data', [])
                    if accounts:
                        logger.info(f"Account {account_name} already exists")
                        continue  # Account already exists

                # Create the account
                logger.info(f"Creating root account: {account_name}")
                account_data = {
                    'doctype': 'Account',
                    'account_name': account_info['account_name'],
                    'account_type': account_info['account_type'],
                    'is_group': account_info['is_group'],
                    'company': company_name
                }

                if 'parent_account' in account_info:
                    account_data['parent_account'] = account_info['parent_account']
                if 'root_type' in account_info:
                    account_data['root_type'] = account_info['root_type']

                create_response = self.session.post(
                    f'{self.base_url}/api/resource/Account',
                    json=account_data
                )
                logger.info(f"Root account creation response for {account_name}: {create_response.status_code}")

                if create_response.status_code == 200:
                    logger.info(f"Successfully created root account: {account_name}")
                else:
                    logger.error(f"Failed to create root account "
                               f"{account_name}: "
                               f"{create_response.status_code}")
                    try:
                        logger.error(f"Root account creation error: "
                                    f"{create_response.json()}")
                    except Exception:
                        logger.error(f"Root account creation error: "
                                    f"{create_response.text}")
                    return False

            logger.info("Successfully created all root accounts")
            return True

        except Exception as e:
            logger.error(f"Exception in _create_root_accounts: {e}")
            return False

    def _bootstrap_chart_of_accounts(self) -> bool:
        """
        Bootstrap the basic chart of accounts structure

        Returns:
            True if successful, False otherwise
        """
        logger.info("Starting _bootstrap_chart_of_accounts")
        try:
            # Create default company first
            logger.info("Step 1: Creating default company")
            if not self._create_default_company():
                logger.error("Failed to create default company")
                return False
            logger.info("Step 1 completed: Default company ready")

            # Create root accounts
            logger.info("Step 2: Creating root accounts")
            if not self._create_root_accounts():
                logger.error("Failed to create root accounts")
                return False
            logger.info("Step 2 completed: Root accounts ready")

            logger.info("Chart of accounts bootstrap completed successfully")
            return True

        except Exception as e:
            logger.error(f"Exception in _bootstrap_chart_of_accounts: {e}")
            return False

    def _create_account_if_not_exists(self, account_name: str,
                                      account_type: str = "Asset") -> bool:
        """
        Create an account in ERPNext if it doesn't exist

        Args:
            account_name: Name of the account to create
            account_type: Type of account (Asset, Liability, etc.)

        Returns:
            True if account exists or was created successfully
        """
        logger.info(f"Starting _create_account_if_not_exists for: {account_name} ({account_type})")
        try:
            # First ensure basic chart of accounts exists
            logger.info("Ensuring chart of accounts is bootstrapped")
            if not self._bootstrap_chart_of_accounts():
                logger.error("Failed to bootstrap chart of accounts")
                return False
            logger.info("Chart of accounts bootstrap check completed")

            # Check if account already exists
            logger.info(f"Checking if account {account_name} exists")
            filter_str = f'[["account_name", "=", "{account_name}"]]'
            response = self.session.get(
                f'{self.base_url}/api/resource/Account',
                params={'filters': filter_str}
            )
            logger.info(f"Account existence check response: {response.status_code}")

            if response.status_code == 200:
                accounts = response.json().get('data', [])
                if accounts:
                    logger.info(f"Account {account_name} already exists")
                    return True  # Account already exists

            # Find appropriate parent account
            logger.info(f"Finding parent account for type: {account_type}")
            parent_account = self._find_parent_account(account_type)
            if not parent_account:
                logger.error(f"Could not find parent account for "
                           f"type {account_type}")
                return False
            logger.info(f"Found parent account: {parent_account}")

            # Account doesn't exist, create it
            logger.info(f"Creating account: {account_name}")
            account_data = {
                'doctype': 'Account',
                'account_name': account_name,
                'account_type': account_type,
                'parent_account': parent_account,
                'is_group': 0,
                'company': self._get_default_company()
            }
            logger.info(f"Account data prepared: {account_data}")

            create_response = self.session.post(
                f'{self.base_url}/api/resource/Account',
                json=account_data
            )
            logger.info(f"Account creation response status: {create_response.status_code}")

            if create_response.status_code == 200:
                logger.info(f"Successfully created account: {account_name}")
                return True
            else:
                logger.error(f"Failed to create account {account_name}: "
                             f"{create_response.status_code}")
                try:
                    logger.error(f"ERPNext account creation error response: "
                                 f"{create_response.json()}")
                except Exception:
                    logger.error(f"ERPNext account creation error response: "
                                 f"{create_response.text}")
                return False

        except Exception as e:
            logger.error(f"Exception in _create_account_if_not_exists for {account_name}: {e}")
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
        logger.info(f"Starting create_user_transaction: {source_user.username} -> {destination_user.username}, amount: {amount}")
        try:
            # Convert platform fee from cents to dollars
            platform_fee_cents = settings.PLATFORM_FEE_CENTS
            platform_fee = Decimal(platform_fee_cents) / 100
            total_deducted = amount + platform_fee
            logger.info(f"Transaction details - Amount: {amount}, Fee: {platform_fee}, Total: {total_deducted}")

            # Ensure accounts exist before creating journal entry
            source_account = f'Users - {source_user.username}'
            dest_account = f'Users - {destination_user.username}'
            logger.info(f"Account names - Source: {source_account}, Dest: {dest_account}")

            logger.info("Step 1: Creating source user account")
            if not self._create_account_if_not_exists(source_account):
                logger.error(f"Failed to create source account: {source_account}")
                return {
                    'success': False,
                    'error': f'Failed to create account for {source_user.username}',
                    'erpnext_status': 'failed',
                }
            logger.info("Step 1 completed: Source account ready")

            logger.info("Step 2: Creating destination user account")
            if not self._create_account_if_not_exists(dest_account):
                logger.error(f"Failed to create destination account: {dest_account}")
                return {
                    'success': False,
                    'error': f'Failed to create account for {destination_user.username}',
                    'erpnext_status': 'failed',
                }
            logger.info("Step 2 completed: Destination account ready")

            logger.info("Step 3: Creating Platform Fees account")
            if not self._create_account_if_not_exists('Platform Fees', 'Income'):
                logger.error("Failed to create Platform Fees account")
                return {
                    'success': False,
                    'error': 'Failed to create Platform Fees account',
                    'erpnext_status': 'failed',
                }
            logger.info("Step 3 completed: Platform Fees account ready")

            # Create Journal Entry in ERPNext
            logger.info("Step 4: Creating journal entry")
            journal_entry_data = {
                'doctype': 'Journal Entry',
                'voucher_type': 'Journal Entry',
                'posting_date': None,  # Will use today's date
                'accounts': [
                    {
                        'account': source_account,
                        'credit_in_account_currency': float(total_deducted),
                        'user_remark': f'Transfer to {destination_user.username}'
                    },
                    {
                        'account': dest_account,
                        'debit_in_account_currency': float(amount),
                        'user_remark': f'Received from {source_user.username}'
                    },
                    {
                        'account': 'Platform Fees',
                        'debit_in_account_currency': float(platform_fee),
                        'user_remark': 'Platform transaction fee'
                    }
                ],
                'user_remark': (f'Transfer from {source_user.username} '
                               f'to {destination_user.username}')
            }
            logger.info(f"Journal entry data prepared: {journal_entry_data}")

            response = self.session.post(
                f'{self.base_url}/api/resource/Journal Entry',
                json=journal_entry_data
            )
            logger.info(f"Journal entry creation response status: {response.status_code}")

            logger.debug(f"ERPNext response: {response.json()}")

            if response.status_code == 200:
                journal_entry = response.json()['data']
                logger.info(f"Successfully created journal entry: {journal_entry.get('name')}")
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
                logger.error(f"Failed to create journal entry: {error_msg}")
                try:
                    logger.error(f"ERPNext transaction error response: "
                                 f"{response.json()}")
                except Exception:
                    logger.error(f"ERPNext transaction error response: "
                                 f"{response.text}")
                return {
                    'success': False,
                    'error': error_msg,
                    'erpnext_status': 'failed',
                }

        except Exception as e:
            logger.error(f"Exception in create_user_transaction: {e}")
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
                f'{self.base_url}/api/method/'
                f'erpnext.accounts.utils.get_balance_on',
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
                try:
                    logger.error(f"ERPNext balance error response: "
                                 f"{response.json()}")
                except Exception:
                    logger.error(f"ERPNext balance error response: "
                                 f"{response.text}")
                return None

        except Exception as e:
            logger.error(f"Failed to get balance for user "
                        f"{user.username}: {e}")
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
                f'{self.base_url}/api/method/'
                f'erpnext.accounts.utils.get_balance_on',
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
                try:
                    logger.error(f"ERPNext fees error response: "
                                 f"{response.json()}")
                except Exception:
                    logger.error(f"ERPNext fees error response: "
                                 f"{response.text}")
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
                f'{self.base_url}/api/resource/Journal Entry/{transaction_id}'
            )

            if response.status_code == 200:
                return response.json()['data']
            else:
                logger.error(f"Failed to get transaction details for "
                             f"{transaction_id}: {response.status_code}")
                try:
                    logger.error(f"ERPNext transaction details error response: "
                                 f"{response.json()}")
                except Exception:
                    logger.error(f"ERPNext transaction details error response: "
                                 f"{response.text}")
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

        output.append(f"\nTransaction ID: "
                     f"{transaction_details.get('name', 'N/A')}")
        output.append(f"Date: "
                     f"{transaction_details.get('posting_date', 'N/A')}")
        output.append(f"Status: "
                     f"{transaction_details.get('docstatus', 'N/A')}")

        accounts = transaction_details.get('accounts', [])
        if accounts:
            output.append("\nAccount Entries:")
            for i, account in enumerate(accounts):
                output.append(f"  {i+1}. {account.get('account', 'N/A')}")
                if account.get('debit_in_account_currency'):
                    debit = account.get('debit_in_account_currency')
                    output.append(f"     Debit: {debit}")
                if account.get('credit_in_account_currency'):
                    credit = account.get('credit_in_account_currency')
                    output.append(f"     Credit: {credit}")
                if account.get('user_remark'):
                    output.append(f"     Remark: "
                                 f"{account.get('user_remark')}")

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
                f'{self.base_url}/api/resource/Journal Entry/{original_id}',
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
                error_msg = (f"Failed to cancel transaction: "
                            f"{cancel_response.status_code}")
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
