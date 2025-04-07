#!/bin/bash
set -e

# Check if .env file exists
if [ ! -f .env ]; then
  echo "Error: .env file not found!"
  echo "Please create a .env file based on .env.example"
  exit 1
fi

# Display information about the deployment
echo "Deploying with environment variables from .env file..."
echo "Building CDK application..."
npm run build

# Display account and region info by running a test synthesis
echo "Checking deployment target..."
npm run synth | grep "Deploying to"

# Confirm deployment
read -p "Do you want to continue with the deployment? (y/n) " -n 1 -r
echo    # Move to a new line
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
  echo "Deployment canceled"
  exit 0
fi

# Deploy the CDK stack
echo "Deploying CDK stack..."
npm run deploy -- --require-approval never

echo "Deployment completed!"
