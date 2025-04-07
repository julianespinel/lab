# Express App AWS Infrastructure

This directory contains AWS CDK code to deploy the Express application with a PostgreSQL database on AWS.

## Architecture

The infrastructure consists of:

1. **VPC** with public, private, and isolated subnets
2. **RDS PostgreSQL Database** in isolated subnets (no public internet access)
3. **ECS Fargate Cluster, Service, and Task** to run the Express application
4. **Application Load Balancer** to route traffic to the ECS service
5. **Security Groups** to control network access
6. **Secret Manager** to store database credentials

## Deployment Architecture

![Architecture Diagram](./architecture.png)

- The Express application runs in ECS Fargate tasks in private subnets with NAT Gateway access for outbound internet traffic
- The PostgreSQL database runs in isolated subnets with no internet access
- Traffic enters through the Application Load Balancer in public subnets
- Database credentials are stored in AWS Secrets Manager

## Prerequisites

- AWS CLI installed and configured
- AWS CDK installed
- Node.js and npm installed

## Configuration with Environment Variables

The deployment can be customized using environment variables in a `.env` file. Create one by copying the example:

```bash
cp .env.example .env
```

Then edit the `.env` file with your AWS account details and application configuration:

```
AWS_ACCOUNT=123456789012  # Your AWS account ID
AWS_REGION=us-east-1      # AWS region to deploy to

# Database configuration
DB_NAME=authors_db        # Database name
DB_USERNAME=postgres      # Database username

# Application configuration
APP_PORT=3000             # Port the application runs on
MIN_CAPACITY=2            # Minimum number of ECS tasks
MAX_CAPACITY=10           # Maximum number of ECS tasks
CPU_SCALING_TARGET_PERCENT=70  # CPU percentage to trigger scaling
```

## Deployment Steps

1. Install dependencies:

```bash
npm install
```

2. Configure your deployment with environment variables:

```bash
cp .env.example .env
# Edit .env with your settings
```

3. Bootstrap your AWS environment (if you haven't already):

```bash
npm run bootstrap
```

4. Deploy using the convenience script:

```bash
npm run deploy:env
# or
./deploy-with-env.sh
```

Or manually:

```bash
npm run build
npm run deploy
```

## Destroying the Infrastructure

When you're done with the infrastructure and want to clean up all AWS resources, you can destroy it:

```bash
npm run destroy:env
# or
./destroy-with-env.sh
```

This script includes safety confirmations to prevent accidental destruction, especially in production environments. You'll need to type "destroy" to confirm, and if the environment is detected as production, you'll need to additionally enter the AWS account ID.

Alternatively, you can destroy manually:

```bash
npm run destroy
```

## Useful Commands

- `npm run build`: Compiles TypeScript to JavaScript
- `npm run watch`: Watches for changes and compiles
- `npm run diff`: Shows differences between current state and deployed stack
- `npm run synth`: Synthesizes and prints the CloudFormation template
- `npm run deploy`: Deploys the stack to your AWS account
- `npm run destroy`: Destroys the stack
- `npm run deploy:env`: Deploys using values from .env file
- `npm run destroy:env`: Destroys infrastructure using values from .env file

## Security Note

- The RDS instance is configured with deletion protection off for development purposes. For production, set `deletionProtection: true` and `removalPolicy: cdk.RemovalPolicy.RETAIN`.
- The database is in isolated subnets and can only be accessed by the ECS tasks.
- Secrets are managed by AWS Secrets Manager.
