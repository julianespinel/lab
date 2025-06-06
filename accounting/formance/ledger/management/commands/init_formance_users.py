"""
Management command to initialize users with starting balances in Formance
"""
from django.core.management.base import BaseCommand
from django.contrib.auth.models import User
from ledger.formance_service import formance_service
from ledger.constants import USD_CENTS_ASSET
from decimal import Decimal


class Command(BaseCommand):
    help = '''
    Initialize users with starting balances in Formance ledger.
    To execute: python manage.py init_formance_users --amount 10000
    '''

    def add_arguments(self, parser):
        parser.add_argument(
            '--amount',
            type=str,
            default='10000',
            help='Starting balance amount in cents (default: 10000 -> $100.00)'
        )

    def handle(self, *args, **options):
        starting_balance = Decimal(options['amount'])

        self.stdout.write(
            self.style.SUCCESS(
                f'Initializing users with ${starting_balance} starting balance...'
            )
        )

        users = User.objects.all()

        for user in users:
            try:
                # Create initial balance transaction from world to user
                result = formance_service._create_ledger_transaction(
                    source_account="world",
                    destination_account=f"users:{user.username}",
                    amount=str(starting_balance),
                    asset=USD_CENTS_ASSET,
                    reference=f"initial_balance_{user.id}_{starting_balance}",
                    metadata={
                        "type": "initial_balance",
                        "user_id": str(user.id),
                        "username": user.username,
                    }
                )

                self.stdout.write(
                    self.style.SUCCESS(
                        f'✓ Initialized {user.username} with ${starting_balance}'
                    )
                )

            except Exception as e:
                self.stdout.write(
                    self.style.ERROR(
                        f'✗ Failed to initialize {user.username}: {e}'
                    )
                )

        self.stdout.write(
            self.style.SUCCESS(
                'User initialization complete!'
            )
        )
