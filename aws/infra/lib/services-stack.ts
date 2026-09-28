import { Annotations, ArnFormat, CfnOutput, Duration, RemovalPolicy, Stack, type StackProps } from 'aws-cdk-lib';
import * as acm from 'aws-cdk-lib/aws-certificatemanager';
import * as cloudfront from 'aws-cdk-lib/aws-cloudfront';
import * as origins from 'aws-cdk-lib/aws-cloudfront-origins';
import type * as ec2 from 'aws-cdk-lib/aws-ec2';
import * as ecr from 'aws-cdk-lib/aws-ecr';
import * as ecs from 'aws-cdk-lib/aws-ecs';
import * as elbv2 from 'aws-cdk-lib/aws-elasticloadbalancingv2';
import * as iam from 'aws-cdk-lib/aws-iam';
import type * as logs from 'aws-cdk-lib/aws-logs';
import * as s3 from 'aws-cdk-lib/aws-s3';
import * as secretsmanager from 'aws-cdk-lib/aws-secretsmanager';
import type { Construct } from 'constructs';
import { REPLY_FOR } from './catalog/queues';
import { type CardDemoConfig, resourceName } from './config';
import { DB_NAME, DB_PORT, DB_SCHEMA } from './data-stack';
import type { MessagingStack } from './messaging-stack';

export const ORIGIN_VERIFY_HEADER = 'X-CardDemo-Origin-Verify';

export interface ServicesStackProps extends StackProps {
  readonly config: CardDemoConfig;
  readonly vpc: ec2.IVpc;
  readonly appSubnets: ec2.SubnetSelection;
  readonly albSg: ec2.ISecurityGroup;
  readonly servicesSg: ec2.ISecurityGroup;
  readonly dbHost: string;
  readonly dbSecret: secretsmanager.ISecret;
  readonly dataBucket: s3.IBucket;
  readonly messaging: MessagingStack;
  readonly logGroup: logs.ILogGroup;
}

/** Service name used for the ECR repository, ECS service and log stream prefix (conventions.md §1). */
export const SERVICES_NAME = 'online-services';

/**
 * ECR repositories, ECS Fargate service for the Spring Boot online services (`aws/online-services/`, port 8080)
 * behind an ALB, and the React SPA (`aws/frontend/` static build) on S3 + CloudFront with `/api/*` routed to the ALB
 * (same origin, `API_BASE_URL=/api/v1`).
 */
export class ServicesStack extends Stack {
  readonly servicesRepo: ecr.Repository;
  readonly frontendRepo: ecr.Repository;
  readonly cluster: ecs.Cluster;
  readonly service: ecs.FargateService;
  readonly alb: elbv2.ApplicationLoadBalancer;
  readonly jwtSecret: secretsmanager.Secret;
  readonly siteBucket: s3.Bucket;
  readonly distribution: cloudfront.Distribution;

  constructor(scope: Construct, id: string, props: ServicesStackProps) {
    super(scope, id, props);
    const cfg = props.config;
    const removal = cfg.retainData ? RemovalPolicy.RETAIN : RemovalPolicy.DESTROY;

    const repo = (id: string, component: string) =>
      new ecr.Repository(this, id, {
        repositoryName: resourceName(cfg, component),
        imageScanOnPush: true,
        imageTagMutability: ecr.TagMutability.MUTABLE,
        encryption: ecr.RepositoryEncryption.AES_256,
        lifecycleRules: [{ description: 'keep last 20 images', maxImageCount: 20 }],
        removalPolicy: removal,
        emptyOnDelete: !cfg.retainData,
      });
    this.servicesRepo = repo('ServicesRepo', SERVICES_NAME);
    this.frontendRepo = repo('FrontendRepo', 'frontend');

    this.jwtSecret = new secretsmanager.Secret(this, 'JwtSecret', {
      secretName: `carddemo/${cfg.envName}/jwt-secret`,
      description: 'HMAC key for CardDemo JWTs (JWT_SECRET, conventions.md §3)',
      generateSecretString: { passwordLength: 64, excludePunctuation: true },
      removalPolicy: removal,
    });

    this.cluster = new ecs.Cluster(this, 'Cluster', {
      clusterName: resourceName(cfg, 'cluster'),
      vpc: props.vpc,
      containerInsightsV2: ecs.ContainerInsights.ENABLED,
    });

    const taskDef = new ecs.FargateTaskDefinition(this, 'ServicesTask', {
      family: resourceName(cfg, SERVICES_NAME),
      cpu: 1024,
      memoryLimitMiB: 2048,
      runtimePlatform: { cpuArchitecture: ecs.CpuArchitecture.X86_64, operatingSystemFamily: ecs.OperatingSystemFamily.LINUX },
    });
    const reportStateMachineArn = this.formatArn({
      service: 'states',
      resource: 'stateMachine',
      resourceName: resourceName(cfg, 'report'),
      arnFormat: ArnFormat.COLON_RESOURCE_NAME,
    });
    taskDef.addContainer('app', {
      containerName: SERVICES_NAME,
      image: ecs.ContainerImage.fromEcrRepository(this.servicesRepo, cfg.imageTag),
      portMappings: [{ containerPort: 8080, name: 'http' }],
      environment: {
        DB_HOST: props.dbHost,
        DB_PORT: String(DB_PORT),
        DB_NAME,
        DB_SCHEMA,
        S3_BUCKET: props.dataBucket.bucketName,
        AWS_REGION: this.region,
        SQS_QUEUE_PREFIX: props.messaging.prefix,
        SERVER_PORT: '8080',
        JWT_TTL_MINUTES: '60',
        REPORT_STATE_MACHINE_ARN: reportStateMachineArn,
        JAVA_TOOL_OPTIONS: '-XX:MaxRAMPercentage=75',
      },
      secrets: {
        DB_USER: ecs.Secret.fromSecretsManager(props.dbSecret, 'username'),
        DB_PASSWORD: ecs.Secret.fromSecretsManager(props.dbSecret, 'password'),
        JWT_SECRET: ecs.Secret.fromSecretsManager(this.jwtSecret),
      },
      logging: ecs.LogDrivers.awsLogs({ logGroup: props.logGroup, streamPrefix: SERVICES_NAME }),
      readonlyRootFilesystem: false,
    });

    this.grantServiceAccess(taskDef.taskRole, props, reportStateMachineArn);

    this.alb = new elbv2.ApplicationLoadBalancer(this, 'Alb', {
      loadBalancerName: resourceName(cfg, 'alb'),
      vpc: props.vpc,
      internetFacing: true,
      securityGroup: props.albSg,
      vpcSubnets: { subnetGroupName: 'public' },
      dropInvalidHeaderFields: true,
    });

    this.service = new ecs.FargateService(this, 'Service', {
      serviceName: resourceName(cfg, SERVICES_NAME),
      cluster: this.cluster,
      taskDefinition: taskDef,
      desiredCount: cfg.serviceDesiredCount,
      vpcSubnets: props.appSubnets,
      securityGroups: [props.servicesSg],
      assignPublicIp: false,
      circuitBreaker: { enable: true, rollback: true },
      healthCheckGracePeriod: Duration.seconds(120),
      minHealthyPercent: 100,
      maxHealthyPercent: 200,
    });
    const scaling = this.service.autoScaleTaskCount({ minCapacity: cfg.serviceDesiredCount, maxCapacity: Math.max(4, cfg.serviceDesiredCount) });
    scaling.scaleOnCpuUtilization('Cpu', { targetUtilizationPercent: 70 });

    const targetGroup = new elbv2.ApplicationTargetGroup(this, 'ServicesTg', {
      targetGroupName: resourceName(cfg, 'svc-tg'),
      vpc: props.vpc,
      port: 8080,
      protocol: elbv2.ApplicationProtocol.HTTP,
      targetType: elbv2.TargetType.IP,
      targets: [this.service],
      deregistrationDelay: Duration.seconds(30),
      healthCheck: { path: '/actuator/health', healthyHttpCodes: '200', interval: Duration.seconds(30) },
    });

    // Only requests carrying the CloudFront origin-verify header reach the service; direct ALB calls get 403.
    const originVerify = new secretsmanager.Secret(this, 'OriginVerifySecret', {
      secretName: `carddemo/${cfg.envName}/cloudfront-origin-verify`,
      description: 'Header value CloudFront adds to /api requests; the ALB rejects requests without it',
      generateSecretString: { excludePunctuation: true, passwordLength: 48 },
    });
    const originVerifyValue = originVerify.secretValue.unsafeUnwrap();
    const denyDirect = elbv2.ListenerAction.fixedResponse(403, { contentType: 'text/plain', messageBody: 'Forbidden' });
    const apiListener = cfg.certificateArn
      ? this.alb.addListener('Https', {
          port: 443,
          certificates: [acm.Certificate.fromCertificateArn(this, 'Cert', cfg.certificateArn)],
          sslPolicy: elbv2.SslPolicy.RECOMMENDED_TLS,
          defaultAction: denyDirect,
          open: false,
        })
      : this.alb.addListener('Http', { port: 80, open: false, defaultAction: denyDirect });
    if (!cfg.certificateArn) {
      Annotations.of(this).addWarningV2(
        'carddemo:no-certificate',
        'certificateArn is not set: the CloudFront-to-ALB hop for /api uses HTTP. Set certificateArn for non-dev environments.',
      );
    }
    if (cfg.certificateArn) {
      this.alb.addListener('Http', {
        port: 80,
        open: false,
        defaultAction: elbv2.ListenerAction.redirect({ protocol: 'HTTPS', port: '443', permanent: true }),
      });
    }
    apiListener.addAction('FromCloudFront', {
      priority: 10,
      conditions: [elbv2.ListenerCondition.httpHeader(ORIGIN_VERIFY_HEADER, [originVerifyValue])],
      action: elbv2.ListenerAction.forward([targetGroup]),
    });

    this.siteBucket = new s3.Bucket(this, 'SiteBucket', {
      encryption: s3.BucketEncryption.S3_MANAGED,
      blockPublicAccess: s3.BlockPublicAccess.BLOCK_ALL,
      enforceSSL: true,
      objectOwnership: s3.ObjectOwnership.BUCKET_OWNER_ENFORCED,
      removalPolicy: cfg.retainData ? RemovalPolicy.RETAIN : RemovalPolicy.DESTROY,
      autoDeleteObjects: !cfg.retainData,
    });

    const spaRewrite = new cloudfront.Function(this, 'SpaRewrite', {
      functionName: resourceName(cfg, 'spa-rewrite'),
      runtime: cloudfront.FunctionRuntime.JS_2_0,
      comment: 'Serve index.html for client-side routes (paths without a file extension)',
      code: cloudfront.FunctionCode.fromInline(
        "function handler(event) { var r = event.request; if (!/\\.[a-zA-Z0-9]+$/.test(r.uri)) { r.uri = '/index.html'; } return r; }",
      ),
    });

    const apiOrigin = new origins.LoadBalancerV2Origin(this.alb, {
      protocolPolicy: cfg.certificateArn ? cloudfront.OriginProtocolPolicy.HTTPS_ONLY : cloudfront.OriginProtocolPolicy.HTTP_ONLY,
      readTimeout: Duration.seconds(60),
      customHeaders: { [ORIGIN_VERIFY_HEADER]: originVerifyValue },
    });
    const apiBehavior: cloudfront.BehaviorOptions = {
      origin: apiOrigin,
      viewerProtocolPolicy: cloudfront.ViewerProtocolPolicy.REDIRECT_TO_HTTPS,
      allowedMethods: cloudfront.AllowedMethods.ALLOW_ALL,
      cachePolicy: cloudfront.CachePolicy.CACHING_DISABLED,
      originRequestPolicy: cloudfront.OriginRequestPolicy.ALL_VIEWER_EXCEPT_HOST_HEADER,
    };

    this.distribution = new cloudfront.Distribution(this, 'Distribution', {
      comment: `CardDemo ${cfg.envName} frontend + API`,
      defaultRootObject: 'index.html',
      priceClass: cloudfront.PriceClass.PRICE_CLASS_100,
      defaultBehavior: {
        origin: origins.S3BucketOrigin.withOriginAccessControl(this.siteBucket),
        viewerProtocolPolicy: cloudfront.ViewerProtocolPolicy.REDIRECT_TO_HTTPS,
        cachePolicy: cloudfront.CachePolicy.CACHING_OPTIMIZED,
        functionAssociations: [{ function: spaRewrite, eventType: cloudfront.FunctionEventType.VIEWER_REQUEST }],
      },
      additionalBehaviors: { '/api/*': apiBehavior, '/actuator/health': apiBehavior },
    });

    new CfnOutput(this, 'ServicesRepoUri', { value: this.servicesRepo.repositoryUri });
    new CfnOutput(this, 'FrontendRepoUri', { value: this.frontendRepo.repositoryUri });
    new CfnOutput(this, 'AlbDnsName', { value: this.alb.loadBalancerDnsName });
    new CfnOutput(this, 'SiteBucketName', { value: this.siteBucket.bucketName });
    new CfnOutput(this, 'DistributionId', { value: this.distribution.distributionId });
    new CfnOutput(this, 'AppUrl', { value: `https://${this.distribution.distributionDomainName}` });
  }

  /**
   * Least-privilege task role (messaging.md §1/§2): consume the three request queues, send only to the allowlisted
   * reply queues, the error queue and the report-request queue; read reports/statements; poll report executions.
   */
  private grantServiceAccess(role: iam.IRole, props: ServicesStackProps, reportStateMachineArn: string): void {
    const m = props.messaging;
    for (const [request, reply] of Object.entries(REPLY_FOR)) {
      m.queue(request).grantConsumeMessages(role);
      m.queue(reply).grantSendMessages(role);
    }
    m.queue('error').grantSendMessages(role);
    m.queue('report-request').grantSendMessages(role);
    props.dataBucket.grantRead(role, 'reports/*');
    props.dataBucket.grantRead(role, 'statements/*');
    const execArn = Stack.of(this).formatArn({
      service: 'states',
      resource: 'execution',
      resourceName: `${reportStateMachineArn.split(':').pop() ?? ''}:*`,
      arnFormat: ArnFormat.COLON_RESOURCE_NAME,
    });
    role.addToPrincipalPolicy(
      new iam.PolicyStatement({ actions: ['states:DescribeExecution'], resources: [execArn] }),
    );
  }
}
