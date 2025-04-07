#!/bin/bash
set -e

echo "Building the CDK project..."
npm run build

echo "Synthesizing CloudFormation template..."
npm run synth

echo "Deploying the CDK stack..."
npm run deploy

echo "Deployment completed!"
