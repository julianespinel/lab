"""
Factory for creating and managing accounting service providers
"""
import logging
from typing import Optional
from django.conf import settings
from ledger.accounting_provider import AccountingProvider
from ledger.formance_service import formance_service
from ledger.beancount_service import beancount_service

logger = logging.getLogger(__name__)


class AccountingFactory:
    """Factory for creating and managing accounting providers"""

    _providers = {
        'formance': formance_service,
        'beancount': beancount_service,
    }

    _current_provider: Optional[AccountingProvider] = None

    @classmethod
    def get_provider(cls, provider_name: Optional[str] = None) -> AccountingProvider:
        """
        Get the accounting provider instance

        Args:
            provider_name: Name of the provider ('formance', 'erpnext', etc.)
                          If None, uses the configured default provider

        Returns:
            AccountingProvider instance

        Raises:
            ValueError: If provider is not supported
        """
        if provider_name is None:
            provider_name = settings.ACCOUNTING_PROVIDER

        provider_name = provider_name.lower()

        if provider_name not in cls._providers:
            available = ', '.join(cls._providers.keys())
            raise ValueError(
                f"Unsupported accounting provider: {provider_name}. "
                f"Available providers: {available}"
            )

        provider = cls._providers[provider_name]

        # Initialize provider if not already done
        if not provider.is_healthy():
            if not provider.initialize():
                logger.error(f"Failed to initialize {provider_name} provider")
                raise Exception(f"Failed to initialize {provider_name} provider")

        cls._current_provider = provider
        return provider

    @classmethod
    def get_current_provider(cls) -> Optional[AccountingProvider]:
        """Get the currently active provider"""
        return cls._current_provider

    @classmethod
    def register_provider(cls, name: str, provider: AccountingProvider):
        """
        Register a new accounting provider

        Args:
            name: Provider name
            provider: Provider instance
        """
        cls._providers[name.lower()] = provider
        logger.info(f"Registered accounting provider: {name}")

    @classmethod
    def list_providers(cls) -> list:
        """List all available provider names"""
        return list(cls._providers.keys())

    @classmethod
    def health_check_all(cls) -> dict:
        """
        Check health status of all registered providers

        Returns:
            Dict mapping provider names to their health status
        """
        health_status = {}
        for name, provider in cls._providers.items():
            try:
                health_status[name] = provider.is_healthy()
            except Exception as e:
                logger.error(f"Health check failed for {name}: {e}")
                health_status[name] = False

        return health_status


# Convenience function to get the current accounting provider
def get_accounting_provider(provider_name: Optional[str] = None) -> AccountingProvider:
    """
    Get the accounting provider instance

    Args:
        provider_name: Name of the provider ('formance', 'erpnext', etc.)
                      If None, uses the configured default provider

    Returns:
        AccountingProvider instance
    """
    return AccountingFactory.get_provider(provider_name)
