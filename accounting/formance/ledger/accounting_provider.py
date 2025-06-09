"""
Abstract base class for accounting service providers
"""
from abc import ABC, abstractmethod
from decimal import Decimal
from typing import Dict, Any, Optional
from django.contrib.auth.models import User


class AccountingProvider(ABC):
    """Abstract base class defining the interface for accounting providers"""

    @abstractmethod
    def create_user_transaction(self, source_user: User,
                                destination_user: User,
                                amount: Decimal) -> Dict[str, Any]:
        """
        Create a transaction between users with platform fee

        Args:
            source_user: User sending the money
            destination_user: User receiving the money
            amount: Amount to transfer

        Returns:
            Dict containing transaction information with at least:
            - success: bool
            - error: str (if success is False)
            - total_deducted: str (total amount deducted from source)
        """
        pass

    @abstractmethod
    def get_user_balance(self, user: User,
                        asset: str = None) -> Optional[Decimal]:
        """
        Get user's balance

        Args:
            user: User to get balance for
            asset: Asset type (provider-specific, optional)

        Returns:
            User's balance or None if error
        """
        pass

    @abstractmethod
    def get_platform_fees_total(self,
                               asset: str = None) -> Optional[Decimal]:
        """
        Get total platform fees collected

        Args:
            asset: Asset type (provider-specific, optional)

        Returns:
            Total fees collected or None if error
        """
        pass

    @abstractmethod
    def get_transaction_details(self,
                               transaction_id: str) -> Optional[Dict[str, Any]]:
        """
        Get transaction details

        Args:
            transaction_id: The transaction ID to retrieve

        Returns:
            Transaction details dictionary or None if error
        """
        pass

    @abstractmethod
    def format_transaction_details(self, transaction_details: Any) -> str:
        """
        Format transaction details for display

        Args:
            transaction_details: Transaction details from the provider

        Returns:
            Formatted string representation of transaction details
        """
        pass

    @abstractmethod
    def revert_transaction(self, transaction_details: Any,
                          original_id: str) -> Dict[str, Any]:
        """
        Revert a transaction

        Args:
            transaction_details: Original transaction details
            original_id: Original transaction ID

        Returns:
            Dictionary with success status and reversal details
        """
        pass

    @abstractmethod
    def initialize(self) -> bool:
        """
        Initialize the provider connection

        Returns:
            True if successful, False otherwise
        """
        pass

    @abstractmethod
    def is_healthy(self) -> bool:
        """
        Check if the provider service is healthy and accessible

        Returns:
            True if healthy, False otherwise
        """
        pass
