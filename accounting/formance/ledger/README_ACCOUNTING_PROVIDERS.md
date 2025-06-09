# Accounting Provider Interface

This project implements a provider pattern for accounting services, allowing you to easily switch between different accounting backends like Formance and ERPNext.

## Architecture

The system uses an abstract base class `AccountingProvider` that defines the interface for all accounting providers. This enables:

- **Loose coupling**: Your views and business logic don't depend on specific accounting implementations
- **Easy switching**: Change providers by updating a single configuration setting
- **Extensibility**: Add new providers by implementing the interface
- **Testability**: Mock providers easily for testing

## Available Providers

### 1. Formance Provider (`FormanceService`)
- High-performance ledger system
- Built-in transaction reversal
- Real-time balance tracking
- Platform fee handling

### 2. ERPNext Provider (`ERPNextService`)
- Full ERP integration
- Journal entry based transactions
- Account-based balance tracking
- Cancellation-based reversals

## Configuration

Add to your Django settings:

```python
# Choose your accounting provider
ACCOUNTING_PROVIDER = 'formance'  # or 'erpnext'

# Platform fee configuration (applies to all providers)
PLATFORM_FEE_CENTS = 10  # $0.10 fee (always specify in cents)

# Formance settings
FORMANCE_SERVER_URL = 'http://localhost:8080'
FORMANCE_CLIENT_ID = 'your_client_id'
FORMANCE_CLIENT_SECRET = 'your_client_secret'
FORMANCE_LEDGER_NAME = 'main'

# ERPNext settings
ERPNEXT_BASE_URL = 'https://your-erpnext-instance.com'
ERPNEXT_API_KEY = 'your_api_key'
ERPNEXT_API_SECRET = 'your_api_secret'
```

## Usage

### In your code:

```python
from ledger.accounting_factory import get_accounting_provider

# Get the configured provider
provider = get_accounting_provider()

# Use the provider
result = provider.create_user_transaction(
    source_user=user1,
    destination_user=user2,
    amount=Decimal('10.00')
)

balance = provider.get_user_balance(user1)
```

### Management commands:

```bash
# List available providers
python manage.py accounting_provider list

# Check health of all providers
python manage.py accounting_provider health

# Test a specific provider
python manage.py accounting_provider test --provider formance
```

## Adding New Providers

1. Create a new service class that inherits from `AccountingProvider`
2. Implement all abstract methods
3. Register it in `AccountingFactory._providers`

Example:

```python
from ledger.accounting_provider import AccountingProvider

class MyCustomService(AccountingProvider):
    def create_user_transaction(self, source_user, destination_user, amount):
        # Your implementation
        pass

    def get_user_balance(self, user, asset=None):
        # Your implementation
        pass

    # ... implement other required methods

# Register the provider
from ledger.accounting_factory import AccountingFactory
AccountingFactory.register_provider('mycustom', MyCustomService())
```

## Interface Methods

All providers must implement these methods:

- `create_user_transaction(source_user, destination_user, amount)` - Create a transaction
- `get_user_balance(user, asset=None)` - Get user's balance
- `get_platform_fees_total(asset=None)` - Get total platform fees
- `get_transaction_details(transaction_id)` - Get transaction details
- `format_transaction_details(transaction_details)` - Format for display
- `revert_transaction(transaction_details, original_id)` - Revert a transaction
- `initialize()` - Initialize the provider
- `is_healthy()` - Health check
