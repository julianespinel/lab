import * as cdk from 'aws-cdk-lib';
import { Construct } from 'constructs';
export interface AppConfig {
    dbName: string;
    dbUsername: string;
    appPort: number;
    minCapacity: number;
    maxCapacity: number;
    cpuScalingTargetPercent: number;
}
export interface EcsRdsStackProps extends cdk.StackProps {
    appConfig: AppConfig;
}
export declare class EcsRdsStack extends cdk.Stack {
    constructor(scope: Construct, id: string, props: EcsRdsStackProps);
}
