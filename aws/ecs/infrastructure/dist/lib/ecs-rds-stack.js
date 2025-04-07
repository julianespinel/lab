"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.EcsRdsStack = void 0;
const cdk = require("aws-cdk-lib");
const ec2 = require("aws-cdk-lib/aws-ec2");
const ecs = require("aws-cdk-lib/aws-ecs");
const ecsPatterns = require("aws-cdk-lib/aws-ecs-patterns");
const rds = require("aws-cdk-lib/aws-rds");
const iam = require("aws-cdk-lib/aws-iam");
const logs = require("aws-cdk-lib/aws-logs");
const secretsmanager = require("aws-cdk-lib/aws-secretsmanager");
class EcsRdsStack extends cdk.Stack {
    constructor(scope, id, props) {
        super(scope, id, props);
        // Extract configuration from props
        const { dbName, dbUsername, appPort, minCapacity, maxCapacity, cpuScalingTargetPercent } = props.appConfig;
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
            instanceType: ec2.InstanceType.of(ec2.InstanceClass.BURSTABLE3, ec2.InstanceSize.SMALL),
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
        dbSecurityGroup.addIngressRule(ecsSecurityGroup, ec2.Port.tcp(5432), 'Allow ECS tasks to connect to RDS');
        // Create a task definition for the Express app
        const taskDefinition = new ecs.FargateTaskDefinition(this, 'ExpressAppTaskDef', {
            memoryLimitMiB: 512,
            cpu: 256,
        });
        // Add policy to allow reading database credentials
        taskDefinition.addToTaskRolePolicy(new iam.PolicyStatement({
            actions: ['secretsmanager:GetSecretValue'],
            resources: [databaseCredentials.secretArn],
        }));
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
exports.EcsRdsStack = EcsRdsStack;
//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJmaWxlIjoiZWNzLXJkcy1zdGFjay5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzIjpbIi4uLy4uL2xpYi9lY3MtcmRzLXN0YWNrLnRzIl0sIm5hbWVzIjpbXSwibWFwcGluZ3MiOiI7OztBQUFBLG1DQUFtQztBQUNuQywyQ0FBMkM7QUFDM0MsMkNBQTJDO0FBRTNDLDREQUE0RDtBQUM1RCwyQ0FBMkM7QUFDM0MsMkNBQTJDO0FBQzNDLDZDQUE2QztBQUM3QyxpRUFBaUU7QUFrQmpFLE1BQWEsV0FBWSxTQUFRLEdBQUcsQ0FBQyxLQUFLO0lBQ3hDLFlBQVksS0FBZ0IsRUFBRSxFQUFVLEVBQUUsS0FBdUI7UUFDL0QsS0FBSyxDQUFDLEtBQUssRUFBRSxFQUFFLEVBQUUsS0FBSyxDQUFDLENBQUM7UUFFeEIsbUNBQW1DO1FBQ25DLE1BQU0sRUFDSixNQUFNLEVBQ04sVUFBVSxFQUNWLE9BQU8sRUFDUCxXQUFXLEVBQ1gsV0FBVyxFQUNYLHVCQUF1QixFQUN4QixHQUFHLEtBQUssQ0FBQyxTQUFTLENBQUM7UUFFcEIsK0NBQStDO1FBQy9DLE1BQU0sR0FBRyxHQUFHLElBQUksR0FBRyxDQUFDLEdBQUcsQ0FBQyxJQUFJLEVBQUUsUUFBUSxFQUFFO1lBQ3RDLE1BQU0sRUFBRSxDQUFDO1lBQ1QsV0FBVyxFQUFFLENBQUM7WUFDZCxtQkFBbUIsRUFBRTtnQkFDbkI7b0JBQ0UsUUFBUSxFQUFFLEVBQUU7b0JBQ1osSUFBSSxFQUFFLFFBQVE7b0JBQ2QsVUFBVSxFQUFFLEdBQUcsQ0FBQyxVQUFVLENBQUMsTUFBTTtpQkFDbEM7Z0JBQ0Q7b0JBQ0UsUUFBUSxFQUFFLEVBQUU7b0JBQ1osSUFBSSxFQUFFLFNBQVM7b0JBQ2YsVUFBVSxFQUFFLEdBQUcsQ0FBQyxVQUFVLENBQUMsbUJBQW1CO2lCQUMvQztnQkFDRDtvQkFDRSxRQUFRLEVBQUUsRUFBRTtvQkFDWixJQUFJLEVBQUUsVUFBVTtvQkFDaEIsVUFBVSxFQUFFLEdBQUcsQ0FBQyxVQUFVLENBQUMsZ0JBQWdCO2lCQUM1QzthQUNGO1NBQ0YsQ0FBQyxDQUFDO1FBRUgsaURBQWlEO1FBQ2pELE1BQU0sbUJBQW1CLEdBQUcsSUFBSSxjQUFjLENBQUMsTUFBTSxDQUFDLElBQUksRUFBRSxlQUFlLEVBQUU7WUFDM0UsVUFBVSxFQUFFLDRCQUE0QjtZQUN4QyxvQkFBb0IsRUFBRTtnQkFDcEIsb0JBQW9CLEVBQUUsSUFBSSxDQUFDLFNBQVMsQ0FBQyxFQUFFLFFBQVEsRUFBRSxVQUFVLEVBQUUsQ0FBQztnQkFDOUQsaUJBQWlCLEVBQUUsVUFBVTtnQkFDN0IsaUJBQWlCLEVBQUUsS0FBSzthQUN6QjtTQUNGLENBQUMsQ0FBQztRQUVILHFFQUFxRTtRQUNyRSxNQUFNLGVBQWUsR0FBRyxJQUFJLEdBQUcsQ0FBQyxhQUFhLENBQUMsSUFBSSxFQUFFLGlCQUFpQixFQUFFO1lBQ3JFLEdBQUc7WUFDSCxXQUFXLEVBQUUsaUNBQWlDO1lBQzlDLGdCQUFnQixFQUFFLElBQUk7U0FDdkIsQ0FBQyxDQUFDO1FBRUgsTUFBTSxRQUFRLEdBQUcsSUFBSSxHQUFHLENBQUMsZ0JBQWdCLENBQUMsSUFBSSxFQUFFLFVBQVUsRUFBRTtZQUMxRCxNQUFNLEVBQUUsR0FBRyxDQUFDLHNCQUFzQixDQUFDLFFBQVEsQ0FBQztnQkFDMUMsT0FBTyxFQUFFLEdBQUcsQ0FBQyxxQkFBcUIsQ0FBQyxNQUFNO2FBQzFDLENBQUM7WUFDRixZQUFZLEVBQUUsR0FBRyxDQUFDLFlBQVksQ0FBQyxFQUFFLENBQy9CLEdBQUcsQ0FBQyxhQUFhLENBQUMsVUFBVSxFQUM1QixHQUFHLENBQUMsWUFBWSxDQUFDLEtBQUssQ0FDdkI7WUFDRCxHQUFHO1lBQ0gsVUFBVSxFQUFFO2dCQUNWLFVBQVUsRUFBRSxHQUFHLENBQUMsVUFBVSxDQUFDLGdCQUFnQjthQUM1QztZQUNELGNBQWMsRUFBRSxDQUFDLGVBQWUsQ0FBQztZQUNqQyxXQUFXLEVBQUUsR0FBRyxDQUFDLFdBQVcsQ0FBQyxVQUFVLENBQUMsbUJBQW1CLENBQUM7WUFDNUQsT0FBTyxFQUFFLEtBQUs7WUFDZCxnQkFBZ0IsRUFBRSxFQUFFO1lBQ3BCLFlBQVksRUFBRSxNQUFNO1lBQ3BCLGtCQUFrQixFQUFFLEtBQUssRUFBRSw2QkFBNkI7WUFDeEQsYUFBYSxFQUFFLEdBQUcsQ0FBQyxhQUFhLENBQUMsT0FBTyxFQUFFLGtDQUFrQztZQUM1RSxlQUFlLEVBQUUsR0FBRyxDQUFDLFFBQVEsQ0FBQyxJQUFJLENBQUMsQ0FBQyxDQUFDO1NBQ3RDLENBQUMsQ0FBQztRQUVILHdCQUF3QjtRQUN4QixNQUFNLE9BQU8sR0FBRyxJQUFJLEdBQUcsQ0FBQyxPQUFPLENBQUMsSUFBSSxFQUFFLG1CQUFtQixFQUFFO1lBQ3pELEdBQUc7WUFDSCw4QkFBOEIsRUFBRSxJQUFJO1NBQ3JDLENBQUMsQ0FBQztRQUVILDRDQUE0QztRQUM1QyxNQUFNLGdCQUFnQixHQUFHLElBQUksR0FBRyxDQUFDLGFBQWEsQ0FBQyxJQUFJLEVBQUUsa0JBQWtCLEVBQUU7WUFDdkUsR0FBRztZQUNILFdBQVcsRUFBRSw4QkFBOEI7WUFDM0MsZ0JBQWdCLEVBQUUsSUFBSTtTQUN2QixDQUFDLENBQUM7UUFFSCxpREFBaUQ7UUFDakQsZUFBZSxDQUFDLGNBQWMsQ0FDNUIsZ0JBQWdCLEVBQ2hCLEdBQUcsQ0FBQyxJQUFJLENBQUMsR0FBRyxDQUFDLElBQUksQ0FBQyxFQUNsQixtQ0FBbUMsQ0FDcEMsQ0FBQztRQUVGLCtDQUErQztRQUMvQyxNQUFNLGNBQWMsR0FBRyxJQUFJLEdBQUcsQ0FBQyxxQkFBcUIsQ0FBQyxJQUFJLEVBQUUsbUJBQW1CLEVBQUU7WUFDOUUsY0FBYyxFQUFFLEdBQUc7WUFDbkIsR0FBRyxFQUFFLEdBQUc7U0FDVCxDQUFDLENBQUM7UUFFSCxtREFBbUQ7UUFDbkQsY0FBYyxDQUFDLG1CQUFtQixDQUNoQyxJQUFJLEdBQUcsQ0FBQyxlQUFlLENBQUM7WUFDdEIsT0FBTyxFQUFFLENBQUMsK0JBQStCLENBQUM7WUFDMUMsU0FBUyxFQUFFLENBQUMsbUJBQW1CLENBQUMsU0FBUyxDQUFDO1NBQzNDLENBQUMsQ0FDSCxDQUFDO1FBRUYsdUNBQXVDO1FBQ3ZDLE1BQU0sUUFBUSxHQUFHLElBQUksSUFBSSxDQUFDLFFBQVEsQ0FBQyxJQUFJLEVBQUUsb0JBQW9CLEVBQUU7WUFDN0QsWUFBWSxFQUFFLGtCQUFrQjtZQUNoQyxTQUFTLEVBQUUsSUFBSSxDQUFDLGFBQWEsQ0FBQyxRQUFRO1lBQ3RDLGFBQWEsRUFBRSxHQUFHLENBQUMsYUFBYSxDQUFDLE9BQU87U0FDekMsQ0FBQyxDQUFDO1FBRUgsbUNBQW1DO1FBQ25DLE1BQU0sU0FBUyxHQUFHLGNBQWMsQ0FBQyxZQUFZLENBQUMsYUFBYSxFQUFFO1lBQzNELG9GQUFvRjtZQUNwRixLQUFLLEVBQUUsR0FBRyxDQUFDLGNBQWMsQ0FBQyxTQUFTLENBQUMsSUFBSSxFQUFFO2dCQUN4QyxPQUFPLEVBQUUsQ0FBQyxnQkFBZ0IsRUFBRSxTQUFTLENBQUM7YUFDdkMsQ0FBQztZQUNGLE9BQU8sRUFBRSxHQUFHLENBQUMsVUFBVSxDQUFDLE9BQU8sQ0FBQztnQkFDOUIsWUFBWSxFQUFFLGFBQWE7Z0JBQzNCLFFBQVE7YUFDVCxDQUFDO1lBQ0YsV0FBVyxFQUFFO2dCQUNYLElBQUksRUFBRSxPQUFPLENBQUMsUUFBUSxFQUFFO2dCQUN4QixPQUFPLEVBQUUsUUFBUSxDQUFDLHlCQUF5QjtnQkFDM0MsT0FBTyxFQUFFLE1BQU07Z0JBQ2YsT0FBTyxFQUFFLE1BQU07YUFDaEI7WUFDRCxPQUFPLEVBQUU7Z0JBQ1AsT0FBTyxFQUFFLEdBQUcsQ0FBQyxNQUFNLENBQUMsa0JBQWtCLENBQUMsbUJBQW1CLEVBQUUsVUFBVSxDQUFDO2dCQUN2RSxXQUFXLEVBQUUsR0FBRyxDQUFDLE1BQU0sQ0FBQyxrQkFBa0IsQ0FBQyxtQkFBbUIsRUFBRSxVQUFVLENBQUM7YUFDNUU7U0FDRixDQUFDLENBQUM7UUFFSCw4QkFBOEI7UUFDOUIsU0FBUyxDQUFDLGVBQWUsQ0FBQztZQUN4QixhQUFhLEVBQUUsT0FBTztTQUN2QixDQUFDLENBQUM7UUFFSCx5Q0FBeUM7UUFDekMsTUFBTSxPQUFPLEdBQUcsSUFBSSxXQUFXLENBQUMscUNBQXFDLENBQUMsSUFBSSxFQUFFLG1CQUFtQixFQUFFO1lBQy9GLE9BQU87WUFDUCxjQUFjO1lBQ2QsY0FBYyxFQUFFLENBQUMsZ0JBQWdCLENBQUM7WUFDbEMsWUFBWSxFQUFFLFdBQVc7WUFDekIsa0JBQWtCLEVBQUUsSUFBSTtZQUN4QixjQUFjLEVBQUUsS0FBSztZQUNyQixXQUFXLEVBQUU7Z0JBQ1gsVUFBVSxFQUFFLEdBQUcsQ0FBQyxVQUFVLENBQUMsbUJBQW1CO2FBQy9DO1NBQ0YsQ0FBQyxDQUFDO1FBRUgseUNBQXlDO1FBQ3pDLE9BQU8sQ0FBQyxXQUFXLENBQUMsb0JBQW9CLENBQUM7WUFDdkMsSUFBSSxFQUFFLGNBQWM7WUFDcEIsT0FBTyxFQUFFLEdBQUcsQ0FBQyxRQUFRLENBQUMsT0FBTyxDQUFDLEVBQUUsQ0FBQztZQUNqQyxRQUFRLEVBQUUsR0FBRyxDQUFDLFFBQVEsQ0FBQyxPQUFPLENBQUMsRUFBRSxDQUFDO1lBQ2xDLGdCQUFnQixFQUFFLEtBQUs7U0FDeEIsQ0FBQyxDQUFDO1FBRUgsNkJBQTZCO1FBQzdCLE1BQU0sT0FBTyxHQUFHLE9BQU8sQ0FBQyxPQUFPLENBQUMsa0JBQWtCLENBQUM7WUFDakQsV0FBVztZQUNYLFdBQVc7U0FDWixDQUFDLENBQUM7UUFFSCxpQ0FBaUM7UUFDakMsT0FBTyxDQUFDLHFCQUFxQixDQUFDLFlBQVksRUFBRTtZQUMxQyx3QkFBd0IsRUFBRSx1QkFBdUI7WUFDakQsZUFBZSxFQUFFLEdBQUcsQ0FBQyxRQUFRLENBQUMsT0FBTyxDQUFDLEVBQUUsQ0FBQztZQUN6QyxnQkFBZ0IsRUFBRSxHQUFHLENBQUMsUUFBUSxDQUFDLE9BQU8sQ0FBQyxFQUFFLENBQUM7U0FDM0MsQ0FBQyxDQUFDO1FBRUgsMEJBQTBCO1FBQzFCLElBQUksR0FBRyxDQUFDLFNBQVMsQ0FBQyxJQUFJLEVBQUUsaUJBQWlCLEVBQUU7WUFDekMsS0FBSyxFQUFFLE9BQU8sQ0FBQyxZQUFZLENBQUMsbUJBQW1CO1NBQ2hELENBQUMsQ0FBQztRQUVILElBQUksR0FBRyxDQUFDLFNBQVMsQ0FBQyxJQUFJLEVBQUUsWUFBWSxFQUFFO1lBQ3BDLEtBQUssRUFBRSxRQUFRLENBQUMseUJBQXlCO1NBQzFDLENBQUMsQ0FBQztJQUNMLENBQUM7Q0FDRjtBQTNMRCxrQ0EyTEMiLCJzb3VyY2VzQ29udGVudCI6WyJpbXBvcnQgKiBhcyBjZGsgZnJvbSAnYXdzLWNkay1saWInO1xuaW1wb3J0ICogYXMgZWMyIGZyb20gJ2F3cy1jZGstbGliL2F3cy1lYzInO1xuaW1wb3J0ICogYXMgZWNzIGZyb20gJ2F3cy1jZGstbGliL2F3cy1lY3MnO1xuaW1wb3J0ICogYXMgZWNyIGZyb20gJ2F3cy1jZGstbGliL2F3cy1lY3InO1xuaW1wb3J0ICogYXMgZWNzUGF0dGVybnMgZnJvbSAnYXdzLWNkay1saWIvYXdzLWVjcy1wYXR0ZXJucyc7XG5pbXBvcnQgKiBhcyByZHMgZnJvbSAnYXdzLWNkay1saWIvYXdzLXJkcyc7XG5pbXBvcnQgKiBhcyBpYW0gZnJvbSAnYXdzLWNkay1saWIvYXdzLWlhbSc7XG5pbXBvcnQgKiBhcyBsb2dzIGZyb20gJ2F3cy1jZGstbGliL2F3cy1sb2dzJztcbmltcG9ydCAqIGFzIHNlY3JldHNtYW5hZ2VyIGZyb20gJ2F3cy1jZGstbGliL2F3cy1zZWNyZXRzbWFuYWdlcic7XG5pbXBvcnQgeyBDb25zdHJ1Y3QgfSBmcm9tICdjb25zdHJ1Y3RzJztcblxuLy8gSW50ZXJmYWNlIGZvciBhcHBsaWNhdGlvbiBjb25maWd1cmF0aW9uXG5leHBvcnQgaW50ZXJmYWNlIEFwcENvbmZpZyB7XG4gIGRiTmFtZTogc3RyaW5nO1xuICBkYlVzZXJuYW1lOiBzdHJpbmc7XG4gIGFwcFBvcnQ6IG51bWJlcjtcbiAgbWluQ2FwYWNpdHk6IG51bWJlcjtcbiAgbWF4Q2FwYWNpdHk6IG51bWJlcjtcbiAgY3B1U2NhbGluZ1RhcmdldFBlcmNlbnQ6IG51bWJlcjtcbn1cblxuLy8gSW50ZXJmYWNlIGZvciBzdGFjayBwcm9wZXJ0aWVzIHdpdGggYXBwQ29uZmlnXG5leHBvcnQgaW50ZXJmYWNlIEVjc1Jkc1N0YWNrUHJvcHMgZXh0ZW5kcyBjZGsuU3RhY2tQcm9wcyB7XG4gIGFwcENvbmZpZzogQXBwQ29uZmlnO1xufVxuXG5leHBvcnQgY2xhc3MgRWNzUmRzU3RhY2sgZXh0ZW5kcyBjZGsuU3RhY2sge1xuICBjb25zdHJ1Y3RvcihzY29wZTogQ29uc3RydWN0LCBpZDogc3RyaW5nLCBwcm9wczogRWNzUmRzU3RhY2tQcm9wcykge1xuICAgIHN1cGVyKHNjb3BlLCBpZCwgcHJvcHMpO1xuXG4gICAgLy8gRXh0cmFjdCBjb25maWd1cmF0aW9uIGZyb20gcHJvcHNcbiAgICBjb25zdCB7XG4gICAgICBkYk5hbWUsXG4gICAgICBkYlVzZXJuYW1lLFxuICAgICAgYXBwUG9ydCxcbiAgICAgIG1pbkNhcGFjaXR5LFxuICAgICAgbWF4Q2FwYWNpdHksXG4gICAgICBjcHVTY2FsaW5nVGFyZ2V0UGVyY2VudFxuICAgIH0gPSBwcm9wcy5hcHBDb25maWc7XG5cbiAgICAvLyBDcmVhdGUgYSBWUEMgd2l0aCBwcml2YXRlIGFuZCBwdWJsaWMgc3VibmV0c1xuICAgIGNvbnN0IHZwYyA9IG5ldyBlYzIuVnBjKHRoaXMsICdBcHBWcGMnLCB7XG4gICAgICBtYXhBenM6IDIsXG4gICAgICBuYXRHYXRld2F5czogMSxcbiAgICAgIHN1Ym5ldENvbmZpZ3VyYXRpb246IFtcbiAgICAgICAge1xuICAgICAgICAgIGNpZHJNYXNrOiAyNCxcbiAgICAgICAgICBuYW1lOiAncHVibGljJyxcbiAgICAgICAgICBzdWJuZXRUeXBlOiBlYzIuU3VibmV0VHlwZS5QVUJMSUMsXG4gICAgICAgIH0sXG4gICAgICAgIHtcbiAgICAgICAgICBjaWRyTWFzazogMjQsXG4gICAgICAgICAgbmFtZTogJ3ByaXZhdGUnLFxuICAgICAgICAgIHN1Ym5ldFR5cGU6IGVjMi5TdWJuZXRUeXBlLlBSSVZBVEVfV0lUSF9FR1JFU1MsXG4gICAgICAgIH0sXG4gICAgICAgIHtcbiAgICAgICAgICBjaWRyTWFzazogMjQsXG4gICAgICAgICAgbmFtZTogJ2lzb2xhdGVkJyxcbiAgICAgICAgICBzdWJuZXRUeXBlOiBlYzIuU3VibmV0VHlwZS5QUklWQVRFX0lTT0xBVEVELFxuICAgICAgICB9LFxuICAgICAgXSxcbiAgICB9KTtcblxuICAgIC8vIENyZWF0ZSBkYXRhYmFzZSBjcmVkZW50aWFscyBpbiBTZWNyZXRzIE1hbmFnZXJcbiAgICBjb25zdCBkYXRhYmFzZUNyZWRlbnRpYWxzID0gbmV3IHNlY3JldHNtYW5hZ2VyLlNlY3JldCh0aGlzLCAnREJDcmVkZW50aWFscycsIHtcbiAgICAgIHNlY3JldE5hbWU6ICdleHByZXNzLWFwcC1kYi1jcmVkZW50aWFscycsXG4gICAgICBnZW5lcmF0ZVNlY3JldFN0cmluZzoge1xuICAgICAgICBzZWNyZXRTdHJpbmdUZW1wbGF0ZTogSlNPTi5zdHJpbmdpZnkoeyB1c2VybmFtZTogZGJVc2VybmFtZSB9KSxcbiAgICAgICAgZ2VuZXJhdGVTdHJpbmdLZXk6ICdwYXNzd29yZCcsXG4gICAgICAgIGV4Y2x1ZGVDaGFyYWN0ZXJzOiAnL0BcIicsXG4gICAgICB9LFxuICAgIH0pO1xuXG4gICAgLy8gQ3JlYXRlIHRoZSBSRFMgaW5zdGFuY2UgaW4gdGhlIGlzb2xhdGVkIHN1Ym5ldHMgKG5vIHB1YmxpYyBhY2Nlc3MpXG4gICAgY29uc3QgZGJTZWN1cml0eUdyb3VwID0gbmV3IGVjMi5TZWN1cml0eUdyb3VwKHRoaXMsICdEQlNlY3VyaXR5R3JvdXAnLCB7XG4gICAgICB2cGMsXG4gICAgICBkZXNjcmlwdGlvbjogJ1NlY3VyaXR5IGdyb3VwIGZvciBSRFMgRGF0YWJhc2UnLFxuICAgICAgYWxsb3dBbGxPdXRib3VuZDogdHJ1ZSxcbiAgICB9KTtcblxuICAgIGNvbnN0IGRhdGFiYXNlID0gbmV3IHJkcy5EYXRhYmFzZUluc3RhbmNlKHRoaXMsICdEYXRhYmFzZScsIHtcbiAgICAgIGVuZ2luZTogcmRzLkRhdGFiYXNlSW5zdGFuY2VFbmdpbmUucG9zdGdyZXMoe1xuICAgICAgICB2ZXJzaW9uOiByZHMuUG9zdGdyZXNFbmdpbmVWZXJzaW9uLlZFUl8xNCxcbiAgICAgIH0pLFxuICAgICAgaW5zdGFuY2VUeXBlOiBlYzIuSW5zdGFuY2VUeXBlLm9mKFxuICAgICAgICBlYzIuSW5zdGFuY2VDbGFzcy5CVVJTVEFCTEUzLFxuICAgICAgICBlYzIuSW5zdGFuY2VTaXplLlNNQUxMXG4gICAgICApLFxuICAgICAgdnBjLFxuICAgICAgdnBjU3VibmV0czoge1xuICAgICAgICBzdWJuZXRUeXBlOiBlYzIuU3VibmV0VHlwZS5QUklWQVRFX0lTT0xBVEVELFxuICAgICAgfSxcbiAgICAgIHNlY3VyaXR5R3JvdXBzOiBbZGJTZWN1cml0eUdyb3VwXSxcbiAgICAgIGNyZWRlbnRpYWxzOiByZHMuQ3JlZGVudGlhbHMuZnJvbVNlY3JldChkYXRhYmFzZUNyZWRlbnRpYWxzKSxcbiAgICAgIG11bHRpQXo6IGZhbHNlLFxuICAgICAgYWxsb2NhdGVkU3RvcmFnZTogMjAsXG4gICAgICBkYXRhYmFzZU5hbWU6IGRiTmFtZSxcbiAgICAgIGRlbGV0aW9uUHJvdGVjdGlvbjogZmFsc2UsIC8vIFNldCB0byB0cnVlIGZvciBwcm9kdWN0aW9uXG4gICAgICByZW1vdmFsUG9saWN5OiBjZGsuUmVtb3ZhbFBvbGljeS5ERVNUUk9ZLCAvLyBDaGFuZ2UgdG8gUkVUQUlOIGZvciBwcm9kdWN0aW9uXG4gICAgICBiYWNrdXBSZXRlbnRpb246IGNkay5EdXJhdGlvbi5kYXlzKDcpLFxuICAgIH0pO1xuXG4gICAgLy8gQ3JlYXRlIGFuIEVDUyBjbHVzdGVyXG4gICAgY29uc3QgY2x1c3RlciA9IG5ldyBlY3MuQ2x1c3Rlcih0aGlzLCAnRXhwcmVzc0FwcENsdXN0ZXInLCB7XG4gICAgICB2cGMsXG4gICAgICBlbmFibGVGYXJnYXRlQ2FwYWNpdHlQcm92aWRlcnM6IHRydWUsXG4gICAgfSk7XG5cbiAgICAvLyBDcmVhdGUgYSBzZWN1cml0eSBncm91cCBmb3IgdGhlIEVDUyB0YXNrc1xuICAgIGNvbnN0IGVjc1NlY3VyaXR5R3JvdXAgPSBuZXcgZWMyLlNlY3VyaXR5R3JvdXAodGhpcywgJ0VDU1NlY3VyaXR5R3JvdXAnLCB7XG4gICAgICB2cGMsXG4gICAgICBkZXNjcmlwdGlvbjogJ1NlY3VyaXR5IGdyb3VwIGZvciBFQ1MgVGFza3MnLFxuICAgICAgYWxsb3dBbGxPdXRib3VuZDogdHJ1ZSxcbiAgICB9KTtcblxuICAgIC8vIEFsbG93IHRoZSBFQ1MgdGFza3MgdG8gY29ubmVjdCB0byB0aGUgZGF0YWJhc2VcbiAgICBkYlNlY3VyaXR5R3JvdXAuYWRkSW5ncmVzc1J1bGUoXG4gICAgICBlY3NTZWN1cml0eUdyb3VwLFxuICAgICAgZWMyLlBvcnQudGNwKDU0MzIpLFxuICAgICAgJ0FsbG93IEVDUyB0YXNrcyB0byBjb25uZWN0IHRvIFJEUydcbiAgICApO1xuXG4gICAgLy8gQ3JlYXRlIGEgdGFzayBkZWZpbml0aW9uIGZvciB0aGUgRXhwcmVzcyBhcHBcbiAgICBjb25zdCB0YXNrRGVmaW5pdGlvbiA9IG5ldyBlY3MuRmFyZ2F0ZVRhc2tEZWZpbml0aW9uKHRoaXMsICdFeHByZXNzQXBwVGFza0RlZicsIHtcbiAgICAgIG1lbW9yeUxpbWl0TWlCOiA1MTIsXG4gICAgICBjcHU6IDI1NixcbiAgICB9KTtcblxuICAgIC8vIEFkZCBwb2xpY3kgdG8gYWxsb3cgcmVhZGluZyBkYXRhYmFzZSBjcmVkZW50aWFsc1xuICAgIHRhc2tEZWZpbml0aW9uLmFkZFRvVGFza1JvbGVQb2xpY3koXG4gICAgICBuZXcgaWFtLlBvbGljeVN0YXRlbWVudCh7XG4gICAgICAgIGFjdGlvbnM6IFsnc2VjcmV0c21hbmFnZXI6R2V0U2VjcmV0VmFsdWUnXSxcbiAgICAgICAgcmVzb3VyY2VzOiBbZGF0YWJhc2VDcmVkZW50aWFscy5zZWNyZXRBcm5dLFxuICAgICAgfSlcbiAgICApO1xuXG4gICAgLy8gQ3JlYXRlIGEgbG9nIGdyb3VwIGZvciB0aGUgY29udGFpbmVyXG4gICAgY29uc3QgbG9nR3JvdXAgPSBuZXcgbG9ncy5Mb2dHcm91cCh0aGlzLCAnRXhwcmVzc0FwcExvZ0dyb3VwJywge1xuICAgICAgbG9nR3JvdXBOYW1lOiAnL2Vjcy9leHByZXNzLWFwcCcsXG4gICAgICByZXRlbnRpb246IGxvZ3MuUmV0ZW50aW9uRGF5cy5PTkVfV0VFSyxcbiAgICAgIHJlbW92YWxQb2xpY3k6IGNkay5SZW1vdmFsUG9saWN5LkRFU1RST1ksXG4gICAgfSk7XG5cbiAgICAvLyBBZGQgY29udGFpbmVyIHRvIHRhc2sgZGVmaW5pdGlvblxuICAgIGNvbnN0IGNvbnRhaW5lciA9IHRhc2tEZWZpbml0aW9uLmFkZENvbnRhaW5lcignZXhwcmVzcy1hcHAnLCB7XG4gICAgICAvLyBQYXRoIHRvIERvY2tlcmZpbGUsIGV4Y2x1ZGUgaW5mcmFzdHJ1Y3R1cmUgZGlyZWN0b3J5IHRvIGF2b2lkIHJlY3Vyc2l2ZSBwYWNrYWdpbmdcbiAgICAgIGltYWdlOiBlY3MuQ29udGFpbmVySW1hZ2UuZnJvbUFzc2V0KCcuLicsIHtcbiAgICAgICAgZXhjbHVkZTogWydpbmZyYXN0cnVjdHVyZScsICdjZGsub3V0J11cbiAgICAgIH0pLFxuICAgICAgbG9nZ2luZzogZWNzLkxvZ0RyaXZlcnMuYXdzTG9ncyh7XG4gICAgICAgIHN0cmVhbVByZWZpeDogJ2V4cHJlc3MtYXBwJyxcbiAgICAgICAgbG9nR3JvdXAsXG4gICAgICB9KSxcbiAgICAgIGVudmlyb25tZW50OiB7XG4gICAgICAgIFBPUlQ6IGFwcFBvcnQudG9TdHJpbmcoKSxcbiAgICAgICAgREJfSE9TVDogZGF0YWJhc2UuZGJJbnN0YW5jZUVuZHBvaW50QWRkcmVzcyxcbiAgICAgICAgREJfUE9SVDogJzU0MzInLFxuICAgICAgICBEQl9OQU1FOiBkYk5hbWUsXG4gICAgICB9LFxuICAgICAgc2VjcmV0czoge1xuICAgICAgICBEQl9VU0VSOiBlY3MuU2VjcmV0LmZyb21TZWNyZXRzTWFuYWdlcihkYXRhYmFzZUNyZWRlbnRpYWxzLCAndXNlcm5hbWUnKSxcbiAgICAgICAgREJfUEFTU1dPUkQ6IGVjcy5TZWNyZXQuZnJvbVNlY3JldHNNYW5hZ2VyKGRhdGFiYXNlQ3JlZGVudGlhbHMsICdwYXNzd29yZCcpLFxuICAgICAgfSxcbiAgICB9KTtcblxuICAgIC8vIEFkZCBjb250YWluZXIgcG9ydCBtYXBwaW5nc1xuICAgIGNvbnRhaW5lci5hZGRQb3J0TWFwcGluZ3Moe1xuICAgICAgY29udGFpbmVyUG9ydDogYXBwUG9ydCxcbiAgICB9KTtcblxuICAgIC8vIENyZWF0ZSBhIGxvYWQtYmFsYW5jZWQgRmFyZ2F0ZSBzZXJ2aWNlXG4gICAgY29uc3Qgc2VydmljZSA9IG5ldyBlY3NQYXR0ZXJucy5BcHBsaWNhdGlvbkxvYWRCYWxhbmNlZEZhcmdhdGVTZXJ2aWNlKHRoaXMsICdFeHByZXNzQXBwU2VydmljZScsIHtcbiAgICAgIGNsdXN0ZXIsXG4gICAgICB0YXNrRGVmaW5pdGlvbixcbiAgICAgIHNlY3VyaXR5R3JvdXBzOiBbZWNzU2VjdXJpdHlHcm91cF0sXG4gICAgICBkZXNpcmVkQ291bnQ6IG1pbkNhcGFjaXR5LFxuICAgICAgcHVibGljTG9hZEJhbGFuY2VyOiB0cnVlLFxuICAgICAgYXNzaWduUHVibGljSXA6IGZhbHNlLFxuICAgICAgdGFza1N1Ym5ldHM6IHtcbiAgICAgICAgc3VibmV0VHlwZTogZWMyLlN1Ym5ldFR5cGUuUFJJVkFURV9XSVRIX0VHUkVTUyxcbiAgICAgIH0sXG4gICAgfSk7XG5cbiAgICAvLyBDb25maWd1cmUgaGVhbHRoIGNoZWNrIGZvciB0aGUgc2VydmljZVxuICAgIHNlcnZpY2UudGFyZ2V0R3JvdXAuY29uZmlndXJlSGVhbHRoQ2hlY2soe1xuICAgICAgcGF0aDogJy9oZWFsdGhjaGVjaycsXG4gICAgICB0aW1lb3V0OiBjZGsuRHVyYXRpb24uc2Vjb25kcygxMCksXG4gICAgICBpbnRlcnZhbDogY2RrLkR1cmF0aW9uLnNlY29uZHMoMzApLFxuICAgICAgaGVhbHRoeUh0dHBDb2RlczogJzIwMCcsXG4gICAgfSk7XG5cbiAgICAvLyBBdXRvIHNjYWxpbmcgY29uZmlndXJhdGlvblxuICAgIGNvbnN0IHNjYWxpbmcgPSBzZXJ2aWNlLnNlcnZpY2UuYXV0b1NjYWxlVGFza0NvdW50KHtcbiAgICAgIG1pbkNhcGFjaXR5LFxuICAgICAgbWF4Q2FwYWNpdHksXG4gICAgfSk7XG5cbiAgICAvLyBTY2FsZSBiYXNlZCBvbiBDUFUgdXRpbGl6YXRpb25cbiAgICBzY2FsaW5nLnNjYWxlT25DcHVVdGlsaXphdGlvbignQ3B1U2NhbGluZycsIHtcbiAgICAgIHRhcmdldFV0aWxpemF0aW9uUGVyY2VudDogY3B1U2NhbGluZ1RhcmdldFBlcmNlbnQsXG4gICAgICBzY2FsZUluQ29vbGRvd246IGNkay5EdXJhdGlvbi5zZWNvbmRzKDYwKSxcbiAgICAgIHNjYWxlT3V0Q29vbGRvd246IGNkay5EdXJhdGlvbi5zZWNvbmRzKDYwKSxcbiAgICB9KTtcblxuICAgIC8vIE91dHB1dCB0aGUgZW5kcG9pbnQgVVJMXG4gICAgbmV3IGNkay5DZm5PdXRwdXQodGhpcywgJ0xvYWRCYWxhbmNlckROUycsIHtcbiAgICAgIHZhbHVlOiBzZXJ2aWNlLmxvYWRCYWxhbmNlci5sb2FkQmFsYW5jZXJEbnNOYW1lLFxuICAgIH0pO1xuXG4gICAgbmV3IGNkay5DZm5PdXRwdXQodGhpcywgJ0RCRW5kcG9pbnQnLCB7XG4gICAgICB2YWx1ZTogZGF0YWJhc2UuZGJJbnN0YW5jZUVuZHBvaW50QWRkcmVzcyxcbiAgICB9KTtcbiAgfVxufVxuIl19