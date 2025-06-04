from django.shortcuts import render, redirect
from django.contrib.auth.decorators import login_required
from django.contrib import messages
from .models import Transaction
from .forms import SendTransactionForm


@login_required
def index(request):
    """Ledger index page with transaction form and history"""

    if request.method == 'POST':
        form = SendTransactionForm(request.POST, current_user=request.user)
        if form.is_valid():
            transaction = form.save(commit=False)
            transaction.source_user = request.user
            transaction.save()
            messages.success(
                request,
                f'Transaction of ${transaction.amount} sent to '
                f'{transaction.destination_user.username} successfully!'
            )
            return redirect('ledger:index')
    else:
        form = SendTransactionForm(current_user=request.user)

    # Get user's sent and received transactions
    sent_transactions = Transaction.objects.filter(
        source_user=request.user
    ).select_related('destination_user').order_by('-created_at')

    received_transactions = Transaction.objects.filter(
        destination_user=request.user
    ).select_related('source_user').order_by('-created_at')

    context = {
        'form': form,
        'sent_transactions': sent_transactions,
        'received_transactions': received_transactions,
    }

    return render(request, 'ledger/index.html', context)
