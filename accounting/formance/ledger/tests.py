from django.test import TestCase
from django.contrib.auth.models import User
from decimal import Decimal
from unittest.mock import Mock, patch
from .formance_service import FormanceService


class FormanceServiceTestCase(TestCase):
    """Test cases for Formance service integration"""

    def setUp(self):
        """Set up test users"""
        self.user1 = User.objects.create_user(
            username='alice',
            email='alice@example.com',
            password='testpass123'
        )
        self.user2 = User.objects.create_user(
            username='bob',
            email='bob@example.com',
            password='testpass123'
        )

    @patch('ledger.formance_service.SDK')
    def test_formance_service_initialization(self, mock_sdk):
        """Test that FormanceService initializes correctly"""
        service = FormanceService()
        self.assertIsNotNone(service.sdk)
        mock_sdk.assert_called_once()

    @patch('ledger.formance_service.formance_service.sdk')
    def test_create_user_transaction_success(self, mock_sdk):
        """Test successful user transaction creation"""
        # Mock the SDK response
        mock_response = Mock()
        mock_response.v2_create_transaction_response = Mock()
        mock_response.v2_create_transaction_response.data = Mock()
        mock_response.v2_create_transaction_response.data.id = "txn_123"
        mock_response.v2_create_transaction_response.data.timestamp = "2023-01-01T00:00:00Z"

        mock_sdk.ledger.v2.create_transaction.return_value = mock_response

        # Test transaction creation
        result = FormanceService().create_user_transaction(
            source_user=self.user1,
            destination_user=self.user2,
            amount=Decimal('50.00')
        )

        # Verify results
        self.assertTrue(result['success'])
        self.assertIn('user_transfer', result)
        self.assertIn('fee_transfer', result)
        self.assertEqual(result['total_deducted'], '50.10')

        # Verify SDK was called twice (user transfer + fee)
        self.assertEqual(mock_sdk.ledger.v2.create_transaction.call_count, 2)

    @patch('ledger.formance_service.formance_service.sdk')
    def test_create_user_transaction_failure(self, mock_sdk):
        """Test transaction creation failure handling"""
        # Mock SDK to raise an exception
        mock_sdk.ledger.v2.create_transaction.side_effect = Exception("API Error")

        # Test transaction creation
        result = FormanceService().create_user_transaction(
            source_user=self.user1,
            destination_user=self.user2,
            amount=Decimal('50.00')
        )

        # Verify failure handling
        self.assertFalse(result['success'])
        self.assertIn('error', result)

    @patch('ledger.formance_service.formance_service.sdk')
    def test_get_user_balance(self, mock_sdk):
        """Test getting user balance from Formance"""
        # Mock the SDK response
        mock_response = Mock()
        mock_response.v2_get_account_response = Mock()
        mock_response.v2_get_account_response.data = Mock()
        mock_response.v2_get_account_response.data.balances = {'USD': 10000}  # $100.00 in cents

        mock_sdk.ledger.v2.get_account.return_value = mock_response

        # Test balance retrieval
        balance = FormanceService().get_user_balance(self.user1)

        # Verify balance conversion from cents
        self.assertEqual(balance, Decimal('100.00'))

        # Verify correct account address was used
        mock_sdk.ledger.v2.get_account.assert_called_once()
        call_args = mock_sdk.ledger.v2.get_account.call_args[1]['request']
        self.assertEqual(call_args['address'], 'users:alice')

    @patch('ledger.formance_service.formance_service.sdk')
    def test_get_platform_fees_total(self, mock_sdk):
        """Test getting platform fees total"""
        # Mock the SDK response
        mock_response = Mock()
        mock_response.v2_get_account_response = Mock()
        mock_response.v2_get_account_response.data = Mock()
        mock_response.v2_get_account_response.data.balances = {'USD': 500}  # $5.00 in cents

        mock_sdk.ledger.v2.get_account.return_value = mock_response

        # Test fees total retrieval
        fees_total = FormanceService().get_platform_fees_total()

        # Verify fees total conversion from cents
        self.assertEqual(fees_total, Decimal('5.00'))

        # Verify correct account address was used
        mock_sdk.ledger.v2.get_account.assert_called_once()
        call_args = mock_sdk.ledger.v2.get_account.call_args[1]['request']
        self.assertEqual(call_args['address'], 'platform:fees')
