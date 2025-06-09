from django.db import models
from django.contrib.auth.models import User
from django.core.validators import MinValueValidator
from decimal import Decimal
from django.conf import settings


class Account(models.Model):
    """Simple account with name and currency"""
    name = models.CharField(max_length=100)
    currency = models.CharField(
        max_length=3,
        default='USD',
        help_text="3-letter currency code (e.g., USD, EUR)"
    )
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)

    def __str__(self):
        return f"{self.name} ({self.currency})"

    @classmethod
    def get_default_account(cls):
        """Get or create the default account"""
        account, created = cls.objects.get_or_create(
            name='Default Account',
            defaults={'currency': 'USD'}
        )
        return account

    class Meta:
        ordering = ['name']


class Transaction(models.Model):
    """Transaction between two users"""
    source_user = models.ForeignKey(
        User,
        on_delete=models.CASCADE,
        related_name='sent_transactions'
    )
    destination_user = models.ForeignKey(
        User,
        on_delete=models.CASCADE,
        related_name='received_transactions'
    )
    account = models.ForeignKey(
        Account,
        on_delete=models.PROTECT,
        null=True,
        blank=True,
        help_text="Account for this transaction "
                  "(uses default if not specified)"
    )
    amount = models.DecimalField(
        max_digits=15,
        decimal_places=2,
        validators=[MinValueValidator(Decimal('0.01'))]
    )
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)

    def save(self, *args, **kwargs):
        """Assign default account if none specified"""
        if not self.account:
            self.account = Account.get_default_account()
        super().save(*args, **kwargs)

    def __str__(self):
        return f"{self.source_user} → {self.destination_user}: ${self.amount}"

    @property
    def total_with_platform_fee(self):
        """Calculate total including platform fee"""
        amount_cents = self.amount * 100
        platform_fee_cents = settings.PLATFORM_FEE_CENTS
        return (amount_cents + platform_fee_cents) / 100

    class Meta:
        ordering = ['-created_at']
