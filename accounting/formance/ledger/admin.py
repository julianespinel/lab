from django.contrib import admin
from .models import Account, Transaction


@admin.register(Account)
class AccountAdmin(admin.ModelAdmin):
    list_display = ['name', 'currency', 'created_at']
    list_filter = ['currency', 'created_at']
    search_fields = ['name']
    readonly_fields = ['created_at', 'updated_at']


@admin.register(Transaction)
class TransactionAdmin(admin.ModelAdmin):
    list_display = [
        'source_user', 'destination_user', 'account',
        'amount', 'created_at'
    ]
    list_filter = ['created_at', 'account']
    search_fields = [
        'source_user__username', 'destination_user__username',
        'account__name'
    ]
    readonly_fields = ['created_at', 'updated_at']
