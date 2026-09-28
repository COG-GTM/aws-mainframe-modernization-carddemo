import { CfnOutput, Duration, RemovalPolicy, Stack, type StackProps } from 'aws-cdk-lib';
import * as s3 from 'aws-cdk-lib/aws-s3';
import type { Construct } from 'constructs';
import { type CardDemoConfig, resourceName } from './config';

export interface StorageStackProps extends StackProps {
  readonly config: CardDemoConfig;
}

/**
 * Single batch-file bucket (`S3_BUCKET`, conventions.md §3) holding every prefix of batch.md §1.2: seed/ETL
 * landing, batch inputs, rejects, reports, statements, backups, exports/imports and run return codes.
 * Lifecycle rules replace the legacy GDG `LIMIT(5)` retention.
 */
export class StorageStack extends Stack {
  readonly dataBucket: s3.Bucket;
  readonly accessLogsBucket: s3.Bucket;

  constructor(scope: Construct, id: string, props: StorageStackProps) {
    super(scope, id, props);
    const cfg = props.config;
    const removal = cfg.retainData ? RemovalPolicy.RETAIN : RemovalPolicy.DESTROY;

    this.accessLogsBucket = new s3.Bucket(this, 'AccessLogs', {
      encryption: s3.BucketEncryption.S3_MANAGED,
      blockPublicAccess: s3.BlockPublicAccess.BLOCK_ALL,
      enforceSSL: true,
      objectOwnership: s3.ObjectOwnership.BUCKET_OWNER_ENFORCED,
      lifecycleRules: [{ id: 'expire-access-logs', expiration: Duration.days(cfg.s3RetentionDays) }],
      removalPolicy: removal,
      autoDeleteObjects: !cfg.retainData,
    });

    const gdgExpiry = Duration.days(cfg.s3RetentionDays);
    const gdgPrefixes = ['input/', 'output/', 'reports/', 'export/', 'import/', 'extract/', 'runs/'];

    this.dataBucket = new s3.Bucket(this, 'DataBucket', {
      encryption: s3.BucketEncryption.S3_MANAGED,
      blockPublicAccess: s3.BlockPublicAccess.BLOCK_ALL,
      enforceSSL: true,
      versioned: true,
      objectOwnership: s3.ObjectOwnership.BUCKET_OWNER_ENFORCED,
      serverAccessLogsBucket: this.accessLogsBucket,
      serverAccessLogsPrefix: 'data-bucket/',
      eventBridgeEnabled: true,
      lifecycleRules: [
        { id: 'abort-incomplete-mpu', abortIncompleteMultipartUploadAfter: Duration.days(7) },
        { id: 'noncurrent-versions', noncurrentVersionExpiration: Duration.days(30) },
        ...gdgPrefixes.map((prefix) => ({
          id: `gdg-${prefix.replace('/', '')}`,
          prefix,
          expiration: gdgExpiry,
        })),
        {
          id: 'gdg-backup',
          prefix: 'backup/',
          transitions: [{ storageClass: s3.StorageClass.INFREQUENT_ACCESS, transitionAfter: Duration.days(30) }],
          expiration: gdgExpiry.toDays() > 30 ? gdgExpiry : Duration.days(31),
        },
        {
          id: 'statements-archive',
          prefix: 'statements/',
          transitions: [{ storageClass: s3.StorageClass.GLACIER_INSTANT_RETRIEVAL, transitionAfter: Duration.days(90) }],
        },
      ],
      removalPolicy: removal,
      autoDeleteObjects: !cfg.retainData,
    });

    new CfnOutput(this, 'DataBucketName', { value: this.dataBucket.bucketName, exportName: resourceName(cfg, 'data-bucket') });
  }
}
