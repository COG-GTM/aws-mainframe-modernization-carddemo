import { CfnOutput, Stack, type StackProps } from 'aws-cdk-lib';
import * as ec2 from 'aws-cdk-lib/aws-ec2';
import * as logs from 'aws-cdk-lib/aws-logs';
import type { Construct } from 'constructs';
import { type CardDemoConfig, resourceName } from './config';

export interface NetworkStackProps extends StackProps {
  readonly config: CardDemoConfig;
}

/**
 * VPC with public (ALB, NAT), private (Fargate services/batch, schema bootstrap Lambda) and isolated (Aurora)
 * subnets, VPC endpoints so tasks reach S3/SQS/ECR/Secrets Manager/Logs/Step Functions without NAT, and all
 * security groups (kept here so cross-stack ingress rules cannot create dependency cycles).
 */
export class NetworkStack extends Stack {
  readonly vpc: ec2.Vpc;
  readonly albSg: ec2.SecurityGroup;
  readonly servicesSg: ec2.SecurityGroup;
  readonly batchSg: ec2.SecurityGroup;
  readonly dbSg: ec2.SecurityGroup;
  readonly bootstrapSg: ec2.SecurityGroup;
  /** Subnets for Fargate tasks and VPC Lambdas (private with egress, or isolated when natGateways = 0). */
  readonly appSubnets: ec2.SubnetSelection;

  constructor(scope: Construct, id: string, props: NetworkStackProps) {
    super(scope, id, props);
    const cfg = props.config;
    const appSubnetType = cfg.natGateways > 0 ? ec2.SubnetType.PRIVATE_WITH_EGRESS : ec2.SubnetType.PRIVATE_ISOLATED;

    this.vpc = new ec2.Vpc(this, 'Vpc', {
      vpcName: resourceName(cfg, 'vpc'),
      ipAddresses: ec2.IpAddresses.cidr(cfg.vpcCidr),
      availabilityZones: cfg.availabilityZones,
      natGateways: cfg.natGateways,
      subnetConfiguration: [
        { name: 'public', subnetType: ec2.SubnetType.PUBLIC, cidrMask: 24 },
        { name: 'private', subnetType: appSubnetType, cidrMask: 22 },
        { name: 'isolated', subnetType: ec2.SubnetType.PRIVATE_ISOLATED, cidrMask: 24 },
      ],
      gatewayEndpoints: { S3: { service: ec2.GatewayVpcEndpointAwsService.S3 } },
      flowLogs: {
        rejected: {
          destination: ec2.FlowLogDestination.toCloudWatchLogs(
            new logs.LogGroup(this, 'FlowLogs', {
              logGroupName: `/carddemo/${cfg.envName}/vpc-flow-logs`,
              retention: cfg.logRetentionDays as logs.RetentionDays,
            }),
          ),
          trafficType: ec2.FlowLogTrafficType.REJECT,
        },
      },
    });
    this.appSubnets = { subnetGroupName: 'private' };

    const interfaceEndpoints: Record<string, ec2.InterfaceVpcEndpointAwsService> = {
      Sqs: ec2.InterfaceVpcEndpointAwsService.SQS,
      EcrApi: ec2.InterfaceVpcEndpointAwsService.ECR,
      EcrDocker: ec2.InterfaceVpcEndpointAwsService.ECR_DOCKER,
      SecretsManager: ec2.InterfaceVpcEndpointAwsService.SECRETS_MANAGER,
      Logs: ec2.InterfaceVpcEndpointAwsService.CLOUDWATCH_LOGS,
      StepFunctions: ec2.InterfaceVpcEndpointAwsService.STEP_FUNCTIONS,
      Sts: ec2.InterfaceVpcEndpointAwsService.STS,
    };
    for (const [id, service] of Object.entries(interfaceEndpoints)) {
      this.vpc.addInterfaceEndpoint(`${id}Endpoint`, { service, subnets: this.appSubnets, privateDnsEnabled: true });
    }

    this.albSg = new ec2.SecurityGroup(this, 'AlbSg', {
      vpc: this.vpc,
      securityGroupName: resourceName(cfg, 'alb-sg'),
      description: 'CardDemo public ALB',
    });
    const albPeer = ec2.Peer.ipv4(cfg.albIngressCidr);
    this.albSg.addIngressRule(albPeer, ec2.Port.tcp(80), 'HTTP from CloudFront / clients');
    if (cfg.certificateArn) this.albSg.addIngressRule(albPeer, ec2.Port.tcp(443), 'HTTPS');

    this.servicesSg = new ec2.SecurityGroup(this, 'ServicesSg', {
      vpc: this.vpc,
      securityGroupName: resourceName(cfg, 'services-sg'),
      description: 'CardDemo Spring Boot services (Fargate)',
    });
    this.servicesSg.addIngressRule(this.albSg, ec2.Port.tcp(8080), 'ALB to services');

    this.batchSg = new ec2.SecurityGroup(this, 'BatchSg', {
      vpc: this.vpc,
      securityGroupName: resourceName(cfg, 'batch-sg'),
      description: 'CardDemo AWS Batch / ETL tasks (no ingress)',
    });

    this.bootstrapSg = new ec2.SecurityGroup(this, 'BootstrapSg', {
      vpc: this.vpc,
      securityGroupName: resourceName(cfg, 'schema-bootstrap-sg'),
      description: 'CardDemo schema bootstrap Lambda (no ingress)',
    });

    this.dbSg = new ec2.SecurityGroup(this, 'DbSg', {
      vpc: this.vpc,
      securityGroupName: resourceName(cfg, 'aurora-sg'),
      description: 'CardDemo Aurora PostgreSQL',
      allowAllOutbound: false,
    });
    this.dbSg.addIngressRule(this.servicesSg, ec2.Port.tcp(5432), 'services');
    this.dbSg.addIngressRule(this.batchSg, ec2.Port.tcp(5432), 'batch + ETL');
    this.dbSg.addIngressRule(this.bootstrapSg, ec2.Port.tcp(5432), 'schema bootstrap');

    new CfnOutput(this, 'VpcId', { value: this.vpc.vpcId });
  }
}
