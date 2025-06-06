from django.shortcuts import render, redirect
from django.contrib.auth.decorators import login_required
from django.contrib import messages
from .models import Transaction
from .forms import SendTransactionForm
from .formance_service import formance_service
import logging

logger = logging.getLogger(__name__)


def _create_local_transaction(form, source_user):
    """Create and save transaction to local database"""
    transaction = form.save(commit=False)
    transaction.source_user = source_user
    transaction.save()
    return transaction


def _sync_transaction_to_formance(transaction, destination_user, amount):
    """Sync transaction to Formance and return result"""
    try:
        return formance_service.create_user_transaction(
            source_user=transaction.source_user,
            destination_user=destination_user,
            amount=amount
        )
    except Exception as e:
        logger.error(
            f"Exception during Formance sync for transaction {transaction.pk}: {e}"
        )
        return {
            'success': False,
            'error': str(e)
        }


def _handle_formance_success(request, transaction, formance_result,
                           destination_user, amount):
    """Handle successful Formance sync"""
    total_deducted = formance_result['total_deducted']

    messages.success(
        request,
        f'Transaction of ${amount} sent to '
        f'{destination_user.username} successfully! '
        f'Total deducted: ${total_deducted} '
        f'(includes $0.10 platform fee)'
    )

    formance_ref = formance_result.get('formance_reference', 'N/A')
    logger.info(
        f"Dual storage transaction completed: "
        f"User {request.user.username} sent ${amount} "
        f"to {destination_user.username}. "
        f"Local DB ID: {transaction.pk}, "
        f"Formance ref: {formance_ref}"
    )


def _handle_formance_failure(request, transaction, formance_result):
    """Handle failed Formance sync"""
    error_msg = formance_result.get("error", "Unknown error")

    messages.warning(
        request,
        f'Transaction saved locally but failed in Formance: '
        f'{error_msg}. '
        f'Your transaction is recorded and will be reconciled.'
    )

    logger.error(
        f"Formance sync failed for local transaction {transaction.pk}: "
        f"{error_msg}"
    )


def _handle_formance_exception(request, transaction):
    """Handle exceptions during Formance sync"""
    messages.warning(
        request,
        'Transaction saved locally but Formance sync failed. '
        'Your transaction is recorded and will be reconciled.'
    )


@login_required
def index(request):
    """Ledger index page with transaction form and history"""

    if request.method == 'POST':
        form = SendTransactionForm(request.POST, current_user=request.user)
        if form.is_valid():
            # Get form data
            destination_user = form.cleaned_data['destination_user']
            amount = form.cleaned_data['amount'] * 100  # Convert to cents

            # Step 1: Save to local database first (for audit trail)
            transaction = _create_local_transaction(form, request.user)

            # Step 2: Sync to Formance
            formance_result = _sync_transaction_to_formance(
                transaction, destination_user, amount
            )

            # Step 3: Handle result
            if formance_result['success']:
                _handle_formance_success(
                    request, transaction, formance_result,
                    destination_user, amount
                )
            else:
                _handle_formance_failure(request, transaction, formance_result)

            return redirect('ledger:index')
    else:
        form = SendTransactionForm(current_user=request.user)

    # Get user's sent and received transactions from local database
    sent_transactions = Transaction.objects.filter(
        source_user=request.user
    ).select_related('destination_user').order_by('-created_at')

    received_transactions = Transaction.objects.filter(
        destination_user=request.user
    ).select_related('source_user').order_by('-created_at')

    # Get user's balance from Formance (real-time)
    user_balance = formance_service.get_user_balance(request.user)
    # user_volumes = formance_service.get_volumes_with_balances(
    #     account_address=f'users:{request.user.username}'
    # )

    # Get platform fees total (for admin visibility)
    platform_fees_total = None
    if request.user.is_staff:
        platform_fees_total = formance_service.get_platform_fees_total()

    context = {
        'form': form,
        'sent_transactions': sent_transactions,
        'received_transactions': received_transactions,
        'user_balance': user_balance,
        'platform_fees_total': platform_fees_total,
    }

    return render(request, 'ledger/index.html', context)
