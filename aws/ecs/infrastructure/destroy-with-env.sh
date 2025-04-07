#!/bin/bash
set -e

# Check if .env file exists
if [ ! -f .env ]; then
  echo "Error: .env file not found!"
  echo "Please create a .env file based on .env.example"
  exit 1
fi

# Display information about the destruction
echo "Preparing to destroy infrastructure created with environment variables from .env file..."

# Display account and region info by running a test synthesis
echo "Checking deployment target..."
npm run synth | grep "Deploying to"

# List the stacks that will be destroyed
echo -e "\nStack that will be destroyed:"
npx cdk ls

# Confirm destruction - require extra confirmation for safety
echo -e "\n⚠️  WARNING: This will destroy all resources in the stack! ⚠️"
echo "This action cannot be undone and may result in data loss."
read -p "Are you ABSOLUTELY SURE you want to destroy the infrastructure? Type 'destroy' to confirm: " confirmation
if [[ $confirmation != "destroy" ]]; then
  echo "Destruction canceled"
  exit 0
fi

# Extra confirmation for production environments
if grep -q "prod\|production" .env; then
  echo -e "\n🚨 PRODUCTION ENVIRONMENT DETECTED! 🚨"
  read -p "You are about to destroy PRODUCTION infrastructure. Type the AWS account ID to confirm: " account_id

  # Get the actual account ID from the .env file
  actual_account=$(grep AWS_ACCOUNT .env | cut -d '=' -f2)

  if [[ $account_id != $actual_account ]]; then
    echo "Account ID doesn't match. Destruction canceled"
    exit 0
  fi
fi

# All confirmations passed, proceed with destruction
echo -e "\nDestroying CDK stack..."
npx cdk destroy --force

echo "Infrastructure destruction completed!"
