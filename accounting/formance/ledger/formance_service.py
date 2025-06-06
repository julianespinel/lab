"""
Formance service for handling transactions and platform fees
"""
import logging
from datetime import datetime
from decimal import Decimal
from typing import Dict, Any, Optional, List
from django.conf import settings
from django.contrib.auth.models import User
from formance_sdk_python import SDK
from formance_sdk_python.models import shared
from formance_sdk_python.models.shared import V2Ledger, V2Transaction
from ledger.constants import USD_CENTS_ASSET

logger = logging.getLogger(__name__)


class FormanceService:
    """Service class for interacting with Formance SDK"""

    def __init__(self):
        """Initialize the Formance SDK client"""
        self.sdk = None
        self.ledgers_cache: Dict[str, Dict[str, V2Ledger]] = {}
        self._initialize_client()

    def _initialize_client(self):
        """Initialize the Formance SDK client with credentials"""
        try:
            self.sdk = SDK(
                server_url=settings.FORMANCE_SERVER_URL,
                security=shared.Security(
                    client_id=settings.FORMANCE_CLIENT_ID,
                    client_secret=settings.FORMANCE_CLIENT_SECRET,
                ),
            )
            logger.info("Formance SDK client initialized successfully")

            # List and cache ledgers after successful initialization
            self._list_and_cache_ledgers()
            self._create_main_ledger_if_not_exists()

        except Exception as e:
            logger.error(f"Failed to initialize Formance SDK: {e}")
            raise

    def _create_main_ledger_if_not_exists(self):
        """Create the main ledger if it doesn't exist"""
        if settings.FORMANCE_LEDGER_NAME in self.ledgers_cache:
            logger.info(f"Ledger {settings.FORMANCE_LEDGER_NAME} already exists")
            return

        request = {
            "ledger": settings.FORMANCE_LEDGER_NAME,
            "v2_create_ledger_request": {},
        }
        response = self.sdk.ledger.v2.create_ledger(request=request)
        if 200 <= response.status_code < 300:
            self.ledgers_cache[settings.FORMANCE_LEDGER_NAME] = {}
        else:
            logger.error(f"Failed to create main ledger: {response.status_code}")
            raise Exception(f"Failed to create main ledger: {response.status_code}")

    def _list_and_cache_ledgers(self):
        """List all ledgers and cache them in a map"""
        try:
            all_ledgers = self._fetch_all_ledgers()
            self._cache_ledgers(all_ledgers)
        except Exception as e:
            print(f"Exception occurred: {e}")
            print(f"Exception type: {type(e)}")
            logger.error(f"Failed to list and cache ledgers: {e}")
            # Initialize empty cache on error
            self.ledgers_cache = {}

    def _fetch_all_ledgers(self) -> List[V2Ledger]:
        """Fetch all ledgers across multiple pages"""
        all_ledgers: List[V2Ledger] = []
        cursor_param = None
        page_count = 0

        # Fetch all pages of ledgers
        while True:
            page_count += 1
            page_ledgers = self._fetch_ledgers_page(page_count, cursor_param)

            if not page_ledgers:
                break

            all_ledgers.extend(page_ledgers['ledgers'])

            # Check if there are more pages
            if page_ledgers['has_more'] and page_ledgers['next_cursor']:
                cursor_param = page_ledgers['next_cursor']
                print("More pages available, continuing...")
            else:
                print("No more pages, finished fetching all ledgers")
                break

        print(f"Total ledgers collected: {len(all_ledgers)} "
              f"across {page_count} pages")
        return all_ledgers

    def _fetch_ledgers_page(self, page_num: int,
                            cursor: Optional[str] = None) -> Dict[str, Any]:
        """Fetch a single page of ledgers"""
        print(f"Fetching ledgers page {page_num}...")

        # Build request with cursor if we have one
        request_params = {}
        if cursor:
            request_params['cursor'] = cursor

        res = self.sdk.ledger.v2.list_ledgers(request=request_params)
        assert res.v2_ledger_list_response is not None

        # Handle response
        cursor_info = res.v2_ledger_list_response.cursor
        print(f"Page {page_num} response cursor info:")
        print(f"  has_more: {cursor_info.has_more}")
        print(f"  page_size: {cursor_info.page_size}")
        print(f"  data count: {len(cursor_info.data)}")

        # Extract ledgers from cursor data
        if cursor_info and hasattr(cursor_info, 'data'):
            page_ledgers: List[V2Ledger] = cursor_info.data
            print(f"Added {len(page_ledgers)} ledgers from page {page_num}")

            return {
                'ledgers': page_ledgers,
                'has_more': cursor_info.has_more,
                'next_cursor': cursor_info.next,
            }
        else:
            print(f"No cursor data found on page {page_num}")
            return {
                'ledgers': [],
                'has_more': False,
                'next_cursor': None,
            }

    def _cache_ledgers(self, ledgers: List[V2Ledger]):
        """Cache a list of ledgers"""
        for ledger in ledgers:
            if ledger.name:
                self.ledgers_cache[ledger.name] = ledger
                print(f"Cached ledger: {ledger.name}")

        # Print summary
        ledger_names = list(self.ledgers_cache.keys())
        logger.info(f"Cached {len(self.ledgers_cache)} ledgers")
        print(f"All cached ledgers: {ledger_names}")

    def get_cached_ledgers(self) -> Dict[str, Dict[str, Any]]:
        """
        Get cached ledgers

        Returns:
            Dict mapping ledger names to their information
        """
        return self.ledgers_cache.copy()

    def refresh_ledgers_cache(self):
        """Refresh the ledgers cache by fetching latest data"""
        self._list_and_cache_ledgers()

    def create_user_transaction(self, source_user: User, destination_user: User,
                              amount: Decimal) -> Dict[str, Any]:
        """
        Create a transaction in Formance with platform fee

        This creates two transactions:
        1. User-to-user transfer
        2. Platform fee deduction from source user

        Args:
            source_user: User sending the money
            destination_user: User receiving the money
            amount: Amount to transfer

        Returns:
            Dict containing detailed transaction information for database storage
        """
        try:
            # Convert amount to string for Formance
            amount_str = str(amount * 100)  # Convert to cents
            platform_fee_str = str(settings.FORMANCE_PLATFORM_FEE_CENTS)

            # Create the main transfer transaction
            user_transfer = self._create_ledger_transaction(
                source_account=f"users:{source_user.username}",
                destination_account=f"users:{destination_user.username}",
                amount=amount_str,
                asset=USD_CENTS_ASSET,
                reference=f"transfer_{source_user.id}_{destination_user.id}_{amount_str}",
                metadata={
                    "type": "user_transfer",
                    "source_user_id": str(source_user.id),
                    "destination_user_id": str(destination_user.id),
                    "source_username": source_user.username,
                    "destination_username": destination_user.username,
                    "amount": amount_str,
                }
            )

            # Create platform fee transaction
            fee_transfer = self._create_ledger_transaction(
                source_account=f"users:{source_user.username}",
                destination_account="platform:fees",
                amount=platform_fee_str,
                asset=USD_CENTS_ASSET,
                reference=f"fee_{source_user.id}_{destination_user.id}_{platform_fee_str}",
                metadata={
                    "type": "platform_fee",
                    "related_transfer": str(user_transfer.id),
                    "source_user_id": str(source_user.id),
                    "amount": platform_fee_str,
                }
            )

            # Determine overall status
            user_success = user_transfer.id > 0
            fee_success = fee_transfer.id > 0

            if user_success and fee_success:
                overall_status = "completed"
            elif user_success or fee_success:
                overall_status = "partial"
            else:
                overall_status = "failed"

            return {
                "success": user_success and fee_success,
                "user_transfer": user_transfer,
                "fee_transfer": fee_transfer,
                "total_deducted": str(Decimal(amount_str) + Decimal(platform_fee_str)),
                # Additional fields for database storage
                "formance_transaction_id": user_transfer.id,
                "formance_reference": user_transfer.reference,
                "platform_fee_transaction_id": fee_transfer.id,
                "platform_fee_reference": fee_transfer.reference,
                "formance_status": overall_status,
                "platform_fee_amount": platform_fee_str,
            }

        except Exception as e:
            logger.error(f"Failed to create transaction: {e}")
            return {
                "success": False,
                "error": str(e),
                "formance_status": "failed",
                "platform_fee_amount": str(settings.FORMANCE_PLATFORM_FEE_CENTS),
            }

    def _create_ledger_transaction(self, source_account: str,
                                 destination_account: str, amount: str,
                                 asset: str, reference: str,
                                 metadata: Dict[str, str]) -> V2Transaction:
        """
        Create a single transaction in Formance ledger

        Args:
            source_account: Source account identifier
            destination_account: Destination account identifier
            amount: Amount to transfer
            asset: Asset type (e.g., USD_CENTS_ASSET)
            reference: Transaction reference
            metadata: Additional transaction metadata

        Returns:
            Transaction response from Formance
        """
        try:
            timestamp = datetime.now().strftime("%Y%m%d%H%M%S")
            request = {
                "ledger": settings.FORMANCE_LEDGER_NAME,
                "v2_post_transaction": {
                    "reference": f'{reference}_{timestamp}',
                    "metadata": metadata,
                    "postings": [
                        {
                            "amount": int(Decimal(amount)),
                            "asset": asset,
                            "source": source_account,
                            "destination": destination_account,
                        }
                    ],
                },
            }
            response = self.sdk.ledger.v2.create_transaction(request=request)
            assert response.v2_create_transaction_response is not None
            return response.v2_create_transaction_response.data

        except Exception as e:
            logger.error(f"Failed to create ledger transaction request: {request}, error: {e}")
            raise

    def get_user_balance(self, user: User, asset: str = USD_CENTS_ASSET) -> Optional[Decimal]:
        """
        Get user's balance from Formance ledger

        Args:
            user: User to get balance for
            asset: Asset type (default: USD_CENTS_ASSET)

        Returns:
            User's balance or None if error
        """
        try:
            account_address = f"users:{user.username}"
            request = {
                "ledger": settings.FORMANCE_LEDGER_NAME,
                "address": account_address,
            }

            response = self.sdk.ledger.v1.get_account(request=request)

            if not response:
                raise Exception(f"No response from Formance for user {user}")

            account_data = response.account_response.data
            balances = account_data.balances
            usd_balance = balances[asset]
            # Convert from cents to dollars
            return Decimal(usd_balance) / 100

        except Exception as e:
            logger.error(f"Failed to get balance for user {user.username}: {e}")
            return None

    def get_platform_fees_total(self, asset: str = USD_CENTS_ASSET) -> Optional[Decimal]:
        """
        Get total platform fees collected

        Args:
            asset: Asset type (default: USD_CENTS_ASSET)

        Returns:
            Total fees collected or None if error
        """
        try:
            request = {
                "ledger": settings.FORMANCE_LEDGER_NAME,
                "address": "platform:fees",
            }
            response = self.sdk.ledger.v1.get_account(request=request)

            if not response:
                raise Exception(f"No response from Formance for platform fees")

            account_data = response.account_response.data
            balances = account_data.balances
            usd_balance = balances[asset]
            # Convert from cents to dollars
            return Decimal(usd_balance) / 100

        except Exception as e:
            logger.error(f"Failed to get platform fees total: {e}")
            return None

    def get_transaction_details(self, transaction_id: str) -> dict:
        """
        Get transaction details from Formance

        Args:
            transaction_id: The transaction ID to retrieve

        Returns:
            Transaction details dictionary
        """
        try:
            response = self.sdk.ledger.v2.get_transaction(
                request={
                    "ledger": settings.FORMANCE_LEDGER_NAME,
                    "id": int(transaction_id)
                }
            )

            if response and hasattr(response, 'v2_get_transaction_response'):
                return response.v2_get_transaction_response.data

            return None

        except Exception as e:
            logger.error(f"Failed to get transaction details for {transaction_id}: {e}")
            raise

    def format_transaction_details(self, transaction_details) -> str:
        """
        Format transaction details for display

        Args:
            transaction_details: Transaction details from Formance

        Returns:
            Formatted string representation of transaction details
        """
        output = []
        output.append("\n" + "="*50)
        output.append("TRANSACTION DETAILS")
        output.append("="*50)

        # Extract postings (transaction movements)
        postings = getattr(transaction_details, 'postings', [])

        for i, posting in enumerate(postings):
            output.append(f"\nPosting {i+1}:")
            output.append(f"  Source: {posting.source}")
            output.append(f"  Destination: {posting.destination}")
            # Convert from cents to dollars for display
            amount_dollars = Decimal(posting.amount) / 100
            output.append(f"  Amount: ${amount_dollars}")
            output.append(f"  Asset: {posting.asset}")

        # Show metadata if available
        metadata = getattr(transaction_details, 'metadata', {})
        if metadata:
            output.append("\nMetadata:")
            for key, value in metadata.items():
                output.append(f"  {key}: {value}")

        output.append("\n" + "="*50)
        return "\n".join(output)

    def revert_transaction(self, transaction_details, original_id: str) -> dict:
        """
        Revert a transaction using Formance SDK's built-in revert functionality

        Args:
            transaction_details: Original transaction details (not used with SDK method)
            original_id: Original transaction ID

        Returns:
            Dictionary with success status and reversal transaction details
        """
        try:
            # Use Formance SDK's built-in revert transaction method
            response = self.sdk.ledger.v2.revert_transaction(
                request={
                    "ledger": settings.FORMANCE_LEDGER_NAME,
                    "id": int(original_id)
                }
            )

            if response and hasattr(response, 'v2_revert_transaction_response'):
                reversal_data = response.v2_revert_transaction_response.data

                # Extract reversal details from the response
                reversal_details = []
                postings = getattr(reversal_data, 'postings', [])

                for i, posting in enumerate(postings):
                    reversal_details.append({
                        'posting_index': i + 1,
                        'from': posting.source,
                        'to': posting.destination,
                        'amount': Decimal(posting.amount) / 100
                    })

                return {
                    'success': True,
                    'reversal_ids': [str(reversal_data.id)] if hasattr(reversal_data, 'id') else [],
                    'reversal_details': reversal_details,
                    'total_reversals': len(postings),
                    'reversal_transaction': reversal_data
                }
            else:
                return {
                    'success': False,
                    'error': 'No response data from revert transaction'
                }

        except Exception as e:
            logger.error(f"Failed to revert transaction {original_id}: {e}")
            return {
                'success': False,
                'error': str(e)
            }


# Create a singleton instance
formance_service = FormanceService()
