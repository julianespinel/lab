"""
Management command to revert a transaction in Formance by its ID
"""
from django.core.management.base import BaseCommand, CommandError
from ledger.formance_service import formance_service
import logging

logger = logging.getLogger(__name__)


class Command(BaseCommand):
    help = '''
    Revert a transaction in Formance ledger by its transaction ID.
    Uses Formance SDK's built-in revert functionality.
    To execute: python manage.py revert_formance_transaction --transaction-id <ID>
    '''

    def add_arguments(self, parser):
        parser.add_argument(
            '--transaction-id',
            type=str,
            required=True,
            help='The Formance transaction ID to revert'
        )
        parser.add_argument(
            '--dry-run',
            action='store_true',
            help='Show what would be reverted without actually reverting'
        )

    def handle(self, *args, **options):
        transaction_id = options['transaction_id']
        dry_run = options.get('dry_run', False)

        prefix = "[DRY RUN] " if dry_run else ""
        self.stdout.write(
            self.style.WARNING(
                f'{prefix}Attempting to revert transaction: {transaction_id}'
            )
        )

        try:
            # Get transaction details from Formance
            transaction_details = formance_service.get_transaction_details(transaction_id)

            if not transaction_details:
                raise CommandError(f'Transaction {transaction_id} not found')

            # Display transaction details
            details_output = formance_service.format_transaction_details(transaction_details)
            self.stdout.write(details_output)

            if dry_run:
                self.stdout.write(
                    self.style.SUCCESS(
                        '[DRY RUN] Transaction details retrieved successfully. '
                        'No actual reversal performed.'
                    )
                )
                return

            # Create reversal transactions
            reversal_result = formance_service.revert_transaction(
                transaction_details, transaction_id
            )

            if reversal_result['success']:
                self.stdout.write(
                    self.style.SUCCESS(
                        f'✓ Successfully reverted transaction {transaction_id}'
                    )
                )
                if reversal_result.get('reversal_ids'):
                    for reversal_id in reversal_result['reversal_ids']:
                        self.stdout.write(f'  - Reversal transaction ID: {reversal_id}')

                # Display reversal details
                if reversal_result.get('reversal_details'):
                    for detail in reversal_result['reversal_details']:
                        self.stdout.write(
                            self.style.SUCCESS(
                                f'  ✓ Created reversal posting {detail["posting_index"]}: '
                                f'{detail["from"]} → {detail["to"]} '
                                f'(${detail["amount"]})'
                            )
                        )
            else:
                error_msg = reversal_result.get("error", "Unknown error")
                self.stdout.write(
                    self.style.ERROR(
                        f'✗ Failed to revert transaction {transaction_id}: '
                        f'{error_msg}'
                    )
                )

        except Exception as e:
            logger.error(f"Error reverting transaction {transaction_id}: {e}")
            raise CommandError(f'Failed to revert transaction: {e}')
