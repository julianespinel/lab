# Formance lab

## Goal

Understand how Formance ledger works.

## How?

1. Create a simple Django app to send and receive transactions between users.
2. For each transaction, capture a fee for the platform.
3. Capture all the accounting data in Formance.

## Install

1. Install formance CLI: https://docs.formance.com/getting-started/fctl-quick-start
2. Create a sandbox in Formance: https://docs.formance.com/guides/newSandbox
   1. Enable the Ledger module
3. Setup the SDK: https://docs.formance.com/getting-started/sdk/
   1. Replace the client ID and client secret in the file `settings.py`

## Run

1. Start the server: `make dev`
2. Go to http://127.0.0.1:8000/
3. Register at least two users
4. Login as one user
5. Start making transactions
6. Sign into https://www.formance.com/ to see your transactions
