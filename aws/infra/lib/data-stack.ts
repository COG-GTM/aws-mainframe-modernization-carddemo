import * as crypto from 'node:crypto';
import * as fs from 'node:fs';
import * as path from 'node:path';
import { Annotations, CfnOutput, CustomResource, Duration, RemovalPolicy, Stack, type StackProps } from 'aws-cdk-lib';
import type * as ec2 from 'aws-cdk-lib/aws-ec2';
import * as lambda from 'aws-cdk-lib/aws-lambda';
import * as nodejs from 'aws-cdk-lib/aws-lambda-nodejs';
import * as logs from 'aws-cdk-lib/aws-logs';
import * as rds from 'aws-cdk-lib/aws-rds';
import type * as secretsmanager from 'aws-cdk-lib/aws-secretsmanager';
import * as cr from 'aws-cdk-lib/custom-resources';
import type { Construct } from 'constructs';
import { type CardDemoConfig, resourceName } from './config';

export const DB_NAME = 'carddemo';
export const DB_SCHEMA = 'carddemo';
export const DB_PORT = 5432;

export interface DataStackProps extends StackProps {
  readonly config: CardDemoConfig;
  readonly vpc: ec2.IVpc;
  readonly dbSg: ec2.ISecurityGroup;
  readonly bootstrapSg: ec2.ISecurityGroup;
  readonly appSubnets: ec2.SubnetSelection;
}

/**
 * Aurora PostgreSQL Serverless v2 (replaces the VSAM KSDS files and DB2 tables), admin credentials in Secrets
 * Manager, and a custom resource that applies `schemaFile` (default `aws/db/schema.sql`) on every deploy where
 * the file content changes. The SQL must be idempotent (`CREATE ... IF NOT EXISTS`).
 */
export class DataStack extends Stack {
  readonly cluster: rds.DatabaseCluster;
  readonly secret: secretsmanager.ISecret;
  readonly schemaApplied: boolean;

  constructor(scope: Construct, id: string, props: DataStackProps) {
    super(scope, id, props);
    const cfg = props.config;

    this.cluster = new rds.DatabaseCluster(this, 'Aurora', {
      clusterIdentifier: resourceName(cfg, 'aurora'),
      engine: rds.DatabaseClusterEngine.auroraPostgres({ version: rds.AuroraPostgresEngineVersion.VER_16_6 }),
      credentials: rds.Credentials.fromGeneratedSecret('carddemo_admin', {
        secretName: `carddemo/${cfg.envName}/db-credentials`,
      }),
      defaultDatabaseName: DB_NAME,
      port: DB_PORT,
      writer: rds.ClusterInstance.serverlessV2('writer', { publiclyAccessible: false }),
      readers: cfg.auroraReader
        ? [rds.ClusterInstance.serverlessV2('reader', { scaleWithWriter: true, publiclyAccessible: false })]
        : [],
      serverlessV2MinCapacity: cfg.auroraMinAcu,
      serverlessV2MaxCapacity: cfg.auroraMaxAcu,
      vpc: props.vpc,
      vpcSubnets: { subnetGroupName: 'isolated' },
      securityGroups: [props.dbSg],
      storageEncrypted: true,
      deletionProtection: cfg.deletionProtection,
      removalPolicy: cfg.retainData ? RemovalPolicy.SNAPSHOT : RemovalPolicy.DESTROY,
      backup: { retention: Duration.days(cfg.retainData ? 14 : 1) },
      cloudwatchLogsExports: ['postgresql'],
      cloudwatchLogsRetention: cfg.logRetentionDays as logs.RetentionDays,
      iamAuthentication: true,
    });
    this.secret = this.cluster.secret!;

    this.schemaApplied = fs.existsSync(cfg.schemaFile) && fs.statSync(cfg.schemaFile).isFile();
    if (this.schemaApplied) {
      this.addSchemaBootstrap(cfg, props);
    } else {
      Annotations.of(this).addWarningV2(
        'carddemo:schema-missing',
        `Schema file ${cfg.schemaFile} not found: schema bootstrap custom resource skipped. ` +
          'Pass -c schemaFile=<path> or run the schema load manually (see aws/infra/README.md).',
      );
    }

    new CfnOutput(this, 'DbEndpoint', { value: this.cluster.clusterEndpoint.hostname });
    new CfnOutput(this, 'DbSecretArn', { value: this.secret.secretArn });
  }

  private addSchemaBootstrap(cfg: CardDemoConfig, props: DataStackProps): void {
    const sql = fs.readFileSync(cfg.schemaFile);
    const schemaHash = crypto.createHash('sha256').update(sql).digest('hex');
    const schemaFile = cfg.schemaFile;

    const fn = new nodejs.NodejsFunction(this, 'SchemaBootstrapFn', {
      functionName: resourceName(cfg, 'schema-bootstrap'),
      entry: path.join(__dirname, '..', 'lambda', 'schema-bootstrap', 'index.ts'),
      handler: 'handler',
      runtime: lambda.Runtime.NODEJS_22_X,
      architecture: lambda.Architecture.ARM_64,
      memorySize: 256,
      timeout: Duration.minutes(10),
      vpc: props.vpc,
      vpcSubnets: props.appSubnets,
      securityGroups: [props.bootstrapSg],
      environment: {
        DB_HOST: this.cluster.clusterEndpoint.hostname,
        DB_PORT: String(DB_PORT),
        DB_NAME,
        DB_SCHEMA,
        DB_SECRET_ARN: this.secret.secretArn,
        NODE_EXTRA_CA_CERTS: '/var/runtime/ca-cert.pem',
      },
      logGroup: new logs.LogGroup(this, 'SchemaBootstrapLogs', {
        retention: cfg.logRetentionDays as logs.RetentionDays,
        removalPolicy: RemovalPolicy.DESTROY,
      }),
      bundling: {
        externalModules: ['@aws-sdk/*', 'pg-native'],
        minify: true,
        commandHooks: {
          beforeBundling: () => [],
          beforeInstall: () => [],
          afterBundling: (_inputDir: string, outputDir: string) => [`cp "${schemaFile}" "${outputDir}/schema.sql"`],
        },
      },
    });
    this.secret.grantRead(fn);

    const provider = new cr.Provider(this, 'SchemaBootstrapProvider', {
      onEventHandler: fn,
      logGroup: new logs.LogGroup(this, 'SchemaBootstrapProviderLogs', {
        retention: cfg.logRetentionDays as logs.RetentionDays,
        removalPolicy: RemovalPolicy.DESTROY,
      }),
    });

    const resource = new CustomResource(this, 'SchemaBootstrap', {
      serviceToken: provider.serviceToken,
      resourceType: 'Custom::CardDemoSchema',
      properties: { SchemaHash: schemaHash, Schema: DB_SCHEMA },
    });
    resource.node.addDependency(this.cluster);
  }
}
