import * as cdk from 'aws-cdk-lib';
import * as ec2 from 'aws-cdk-lib/aws-ec2';
import * as ecs from 'aws-cdk-lib/aws-ecs';
import * as ecr from 'aws-cdk-lib/aws-ecr';
import * as ecsPatterns from 'aws-cdk-lib/aws-ecs-patterns';
import * as rds from 'aws-cdk-lib/aws-rds';
import * as iam from 'aws-cdk-lib/aws-iam';
import * as logs from 'aws-cdk-lib/aws-logs';
import * as secretsmanager from 'aws-cdk-lib/aws-secretsmanager';
import { Construct } from 'constructs';

// Interface for application configuration
export interface AppConfig {
  dbName: string;
  dbUsername: string;
  appPort: number;
  minCapacity: number;
  maxCapacity: number;
  cpuScalingTargetPercent: number;
}

// Interface for stack properties with appConfig
export interface EcsRdsStackProps extends cdk.StackProps {
  appConfig: AppConfig;
}

export class EcsRdsStack extends cdk.Stack {
  constructor(scope: Construct, id: string, props: EcsRdsStackProps) {
    super(scope, id, props);

    // Extract configuration from props
    const {
      dbName,
      dbUsername,
      appPort,
      minCapacity,
      maxCapacity,
      cpuScalingTargetPercent
    } = props.appConfig;

    // Create a VPC with private and public subnets
    const vpc = new ec2.Vpc(this, 'AppVpc', {
      maxAzs: 2,
      natGateways: 1,
      subnetConfiguration: [
        {
          cidrMask: 24,
          name: 'public',
          subnetType: ec2.SubnetType.PUBLIC,
        },
        {
          cidrMask: 24,
          name: 'private',
          subnetType: ec2.SubnetType.PRIVATE_WITH_EGRESS,
        },
        {
          cidrMask: 24,
          name: 'isolated',
          subnetType: ec2.SubnetType.PRIVATE_ISOLATED,
        },
      ],
    });

    // Create database credentials in Secrets Manager
    const databaseCredentials = new secretsmanager.Secret(this, 'DBCredentials', {
      secretName: 'express-app-db-credentials',
      generateSecretString: {
        secretStringTemplate: JSON.stringify({ username: dbUsername }),
        generateStringKey: 'password',
        excludeCharacters: '/@"',
      },
    });

    // Create the RDS instance in the isolated subnets (no public access)
    const dbSecurityGroup = new ec2.SecurityGroup(this, 'DBSecurityGroup', {
      vpc,
      description: 'Security group for RDS Database',
      allowAllOutbound: true,
    });

    const database = new rds.DatabaseInstance(this, 'Database', {
      engine: rds.DatabaseInstanceEngine.postgres({
        version: rds.PostgresEngineVersion.VER_14,
      }),
      instanceType: ec2.InstanceType.of(
        ec2.InstanceClass.BURSTABLE3,
        ec2.InstanceSize.SMALL
      ),
      vpc,
      vpcSubnets: {
        subnetType: ec2.SubnetType.PRIVATE_ISOLATED,
      },
      securityGroups: [dbSecurityGroup],
      credentials: rds.Credentials.fromSecret(databaseCredentials),
      multiAz: false,
      allocatedStorage: 20,
      databaseName: dbName,
      deletionProtection: false, // Set to true for production
      removalPolicy: cdk.RemovalPolicy.DESTROY, // Change to RETAIN for production
      backupRetention: cdk.Duration.days(7),
    });

    // Create an ECS cluster
    const cluster = new ecs.Cluster(this, 'ExpressAppCluster', {
      vpc,
      enableFargateCapacityProviders: true,
    });

    // Create a security group for the ECS tasks
    const ecsSecurityGroup = new ec2.SecurityGroup(this, 'ECSSecurityGroup', {
      vpc,
      description: 'Security group for ECS Tasks',
      allowAllOutbound: true,
    });

    // Allow the ECS tasks to connect to the database
    dbSecurityGroup.addIngressRule(
      ecsSecurityGroup,
      ec2.Port.tcp(5432),
      'Allow ECS tasks to connect to RDS'
    );

    // Create a task definition for the Express app
    const taskDefinition = new ecs.FargateTaskDefinition(this, 'ExpressAppTaskDef', {
      memoryLimitMiB: 512,
      cpu: 256,
    });

    // Add policy to allow reading database credentials
    taskDefinition.addToTaskRolePolicy(
      new iam.PolicyStatement({
        actions: ['secretsmanager:GetSecretValue'],
        resources: [databaseCredentials.secretArn],
      })
    );

    // Create a log group for the container
    const logGroup = new logs.LogGroup(this, 'ExpressAppLogGroup', {
      logGroupName: '/ecs/express-app',
      retention: logs.RetentionDays.ONE_WEEK,
      removalPolicy: cdk.RemovalPolicy.DESTROY,
    });

    // Add container to task definition
    const container = taskDefinition.addContainer('express-app', {
      // Path to Dockerfile, exclude infrastructure directory to avoid recursive packaging
      image: ecs.ContainerImage.fromAsset('..', {
        exclude: ['infrastructure', 'cdk.out']
      }),
      logging: ecs.LogDrivers.awsLogs({
        streamPrefix: 'express-app',
        logGroup,
      }),
      environment: {
        PORT: appPort.toString(),
        DB_HOST: database.dbInstanceEndpointAddress,
        DB_PORT: '5432',
        DB_NAME: dbName,
      },
      secrets: {
        DB_USER: ecs.Secret.fromSecretsManager(databaseCredentials, 'username'),
        DB_PASSWORD: ecs.Secret.fromSecretsManager(databaseCredentials, 'password'),
      },
    });

    // Add container port mappings
    container.addPortMappings({
      containerPort: appPort,
    });

    // Create a load-balanced Fargate service
    const service = new ecsPatterns.ApplicationLoadBalancedFargateService(this, 'ExpressAppService', {
      cluster,
      taskDefinition,
      securityGroups: [ecsSecurityGroup],
      desiredCount: minCapacity,
      publicLoadBalancer: true,
      assignPublicIp: false,
      taskSubnets: {
        subnetType: ec2.SubnetType.PRIVATE_WITH_EGRESS,
      },
    });

    // Configure health check for the service
    service.targetGroup.configureHealthCheck({
      path: '/healthcheck',
      timeout: cdk.Duration.seconds(10),
      interval: cdk.Duration.seconds(30),
      healthyHttpCodes: '200',
    });

    // Auto scaling configuration
    const scaling = service.service.autoScaleTaskCount({
      minCapacity,
      maxCapacity,
    });

    // Scale based on CPU utilization
    scaling.scaleOnCpuUtilization('CpuScaling', {
      targetUtilizationPercent: cpuScalingTargetPercent,
      scaleInCooldown: cdk.Duration.seconds(60),
      scaleOutCooldown: cdk.Duration.seconds(60),
    });

    // Output the endpoint URL
    new cdk.CfnOutput(this, 'LoadBalancerDNS', {
      value: service.loadBalancer.loadBalancerDnsName,
    });

    new cdk.CfnOutput(this, 'DBEndpoint', {
      value: database.dbInstanceEndpointAddress,
    });
  }
}
