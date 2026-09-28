import * as fs from 'node:fs';
import * as path from 'node:path';
import { Annotations, ArnFormat, CfnOutput, Duration, RemovalPolicy, Size, Stack, type StackProps } from 'aws-cdk-lib';
import * as batch from 'aws-cdk-lib/aws-batch';
import type * as ec2 from 'aws-cdk-lib/aws-ec2';
import * as ecr from 'aws-cdk-lib/aws-ecr';
import * as ecs from 'aws-cdk-lib/aws-ecs';
import * as events from 'aws-cdk-lib/aws-events';
import * as targets from 'aws-cdk-lib/aws-events-targets';
import * as iam from 'aws-cdk-lib/aws-iam';
import * as lambda from 'aws-cdk-lib/aws-lambda';
import { SqsEventSource } from 'aws-cdk-lib/aws-lambda-event-sources';
import * as nodejs from 'aws-cdk-lib/aws-lambda-nodejs';
import * as logs from 'aws-cdk-lib/aws-logs';
import type * as s3 from 'aws-cdk-lib/aws-s3';
import type * as secretsmanager from 'aws-cdk-lib/aws-secretsmanager';
import type * as sns from 'aws-cdk-lib/aws-sns';
import * as sfn from 'aws-cdk-lib/aws-stepfunctions';
import type { Construct } from 'constructs';
import { aslSubstitutionKeys, buildDefaultAsl, findPlaceholders } from './catalog/asl';
import { type FlowSpec, flowJobs } from './catalog/flows';
import { type JobSpec, jobDefinitionKey, jobsFor } from './catalog/jobs';
import { type CardDemoConfig, resourceName } from './config';
import { DB_NAME, DB_PORT, DB_SCHEMA } from './data-stack';
import type { MessagingStack } from './messaging-stack';

export interface BatchStackProps extends StackProps {
  readonly config: CardDemoConfig;
  readonly flows: FlowSpec[];
  readonly vpc: ec2.IVpc;
  readonly appSubnets: ec2.SubnetSelection;
  readonly batchSg: ec2.ISecurityGroup;
  readonly dbHost: string;
  readonly dbSecret: secretsmanager.ISecret;
  readonly dataBucket: s3.IBucket;
  readonly messaging: MessagingStack;
  readonly logGroup: logs.ILogGroup;
  readonly warningTopic: sns.ITopic;
}

/**
 * AWS Batch on Fargate for the JCL/COBOL batch jobs (aws/contracts/batch.md): one image (`carddemo-batch`), one
 * job definition per job with its own least-privilege job role, Step Functions flows (loaded from
 * `<stateMachineDir>/<flow>.asl.json` when the batch session provides them, otherwise generated from the
 * contract), EventBridge schedules/chaining replacing CA-7/Control-M, and the report dispatcher
 * (SQS `carddemo-report-request` → `carddemo-report` execution named after the request id).
 */
export class BatchStack extends Stack {
  readonly batchRepo: ecr.Repository;
  readonly etlRepo: ecr.Repository;
  readonly jobQueue: batch.JobQueue;
  readonly jobDefinitions: Record<string, batch.EcsJobDefinition> = {};
  readonly stateMachines: Record<string, sfn.StateMachine> = {};
  /** Flow name → `file` (loaded from stateMachineDir) or `generated`. */
  readonly definitionSources: Record<string, 'file' | 'generated'> = {};

  constructor(scope: Construct, id: string, props: BatchStackProps) {
    super(scope, id, props);
    const cfg = props.config;
    const removal = cfg.retainData ? RemovalPolicy.RETAIN : RemovalPolicy.DESTROY;

    const repo = (id: string, component: string) =>
      new ecr.Repository(this, id, {
        repositoryName: resourceName(cfg, component),
        imageScanOnPush: true,
        encryption: ecr.RepositoryEncryption.AES_256,
        lifecycleRules: [{ description: 'keep last 20 images', maxImageCount: 20 }],
        removalPolicy: removal,
        emptyOnDelete: !cfg.retainData,
      });
    this.batchRepo = repo('BatchRepo', 'batch');
    this.etlRepo = repo('EtlRepo', 'etl');

    const computeEnv = new batch.FargateComputeEnvironment(this, 'ComputeEnv', {
      computeEnvironmentName: resourceName(cfg, 'batch-fargate'),
      vpc: props.vpc,
      vpcSubnets: props.appSubnets,
      securityGroups: [props.batchSg],
      maxvCpus: cfg.batchMaxVcpus,
      replaceComputeEnvironment: false,
    });
    this.jobQueue = new batch.JobQueue(this, 'JobQueue', {
      jobQueueName: resourceName(cfg, 'batch-queue'),
      priority: 1,
      computeEnvironments: [{ computeEnvironment: computeEnv, order: 1 }],
    });

    const executionRole = new iam.Role(this, 'BatchExecutionRole', {
      roleName: resourceName(cfg, 'batch-exec'),
      assumedBy: new iam.ServicePrincipal('ecs-tasks.amazonaws.com'),
      managedPolicies: [iam.ManagedPolicy.fromAwsManagedPolicyName('service-role/AmazonECSTaskExecutionRolePolicy')],
    });

    const environment = {
      DB_HOST: props.dbHost,
      DB_PORT: String(DB_PORT),
      DB_NAME,
      DB_SCHEMA,
      S3_BUCKET: props.dataBucket.bucketName,
      AWS_REGION: this.region,
      SQS_QUEUE_PREFIX: props.messaging.prefix,
      JAVA_TOOL_OPTIONS: '-XX:MaxRAMPercentage=75',
    };
    const secrets = {
      DB_USER: batch.Secret.fromSecretsManager(props.dbSecret, 'username'),
      DB_PASSWORD: batch.Secret.fromSecretsManager(props.dbSecret, 'password'),
    };

    const jobs = jobsFor(cfg.enableAuthModule);
    for (const job of jobs) {
      this.jobDefinitions[job.name] = this.addJobDefinition(cfg, job, props, {
        image: ecs.ContainerImage.fromEcrRepository(this.batchRepo, cfg.imageTag),
        command: [...cfg.batchCommandPrefix, `--job=${job.name}`],
        executionRole,
        environment,
        secrets,
      });
    }
    this.jobDefinitions.etl = this.addJobDefinition(
      cfg,
      { name: 'etl', legacy: 'aws/etl loaders (ASCII/EBCDIC seed to Aurora)', s3Read: ['seed/', 'refdata/', 'import/'], s3Write: ['etl/'] },
      props,
      { image: ecs.ContainerImage.fromEcrRepository(this.etlRepo, cfg.imageTag), executionRole, environment, secrets },
    );

    this.addStateMachines(cfg, props, jobs);
    this.addReportDispatcher(cfg, props);

    new CfnOutput(this, 'BatchRepoUri', { value: this.batchRepo.repositoryUri });
    new CfnOutput(this, 'EtlRepoUri', { value: this.etlRepo.repositoryUri });
    new CfnOutput(this, 'JobQueueArn', { value: this.jobQueue.jobQueueArn });
    new CfnOutput(this, 'DailyCycleStateMachineArn', { value: this.stateMachines['daily-cycle'].stateMachineArn });
  }

  private addJobDefinition(
    cfg: CardDemoConfig,
    job: JobSpec,
    props: BatchStackProps,
    container: {
      image: ecs.ContainerImage;
      command?: string[];
      executionRole: iam.IRole;
      environment: Record<string, string>;
      secrets: Record<string, batch.Secret>;
    },
  ): batch.EcsJobDefinition {
    const jobRole = new iam.Role(this, `JobRole-${job.name}`, {
      assumedBy: new iam.ServicePrincipal('ecs-tasks.amazonaws.com'),
      description: `CardDemo batch job ${job.name} (${job.legacy})`,
    });
    for (const prefix of job.s3Read) props.dataBucket.grantRead(jobRole, `${prefix}*`);
    for (const prefix of [...job.s3Write, 'runs/']) props.dataBucket.grantPut(jobRole, `${prefix}*`);
    if (job.s3Write.length > 0 || job.s3Read.length > 0) {
      jobRole.addToPrincipalPolicy(
        new iam.PolicyStatement({
          actions: ['s3:ListBucket'],
          resources: [props.dataBucket.bucketArn],
          conditions: { StringLike: { 's3:prefix': [...job.s3Read, ...job.s3Write].map((p) => `${p}*`) } },
        }),
      );
    }

    return new batch.EcsJobDefinition(this, `JobDef-${job.name}`, {
      jobDefinitionName: resourceName(cfg, job.name),
      timeout: Duration.hours(4),
      retryAttempts: 1,
      propagateTags: true,
      container: new batch.EcsFargateContainerDefinition(this, `Container-${job.name}`, {
        image: container.image,
        cpu: 1,
        memory: Size.gibibytes(2),
        command: container.command,
        executionRole: container.executionRole,
        jobRole,
        environment: container.environment,
        secrets: container.secrets,
        logging: ecs.LogDriver.awsLogs({ logGroup: props.logGroup, streamPrefix: job.name }),
        assignPublicIp: false,
        fargatePlatformVersion: ecs.FargatePlatformVersion.LATEST,
      }),
    });
  }

  private addStateMachines(cfg: CardDemoConfig, props: BatchStackProps, jobs: JobSpec[]): void {
    const jobNames = jobs.map((j) => j.name);
    const substitutions: Record<string, string> = {
      Partition: this.partition,
      Region: this.region,
      AccountId: this.account,
      EnvName: cfg.envName,
      JobQueueArn: this.jobQueue.jobQueueArn,
      JobDefinitionPrefix: resourceName(cfg, ''),
      DataBucket: props.dataBucket.bucketName,
      WarningTopicArn: props.warningTopic.topicArn,
      StateMachinePrefix: resourceName(cfg, ''),
    };
    for (const job of jobNames) substitutions[jobDefinitionKey(job)] = this.jobDefinitions[job].jobDefinitionArn;
    const known = new Set(aslSubstitutionKeys(jobNames));

    for (const flow of props.flows) {
      const file = path.join(cfg.stateMachineDir, `${flow.name}.asl.json`);
      let text: string;
      if (fs.existsSync(file)) {
        text = fs.readFileSync(file, 'utf8');
        JSON.parse(text);
        this.definitionSources[flow.name] = 'file';
      } else {
        text = JSON.stringify(buildDefaultAsl(flow, cfg.batchCommandPrefix), null, 2);
        this.definitionSources[flow.name] = 'generated';
      }
      const used = findPlaceholders(text);
      const unknown = used.filter((p) => !known.has(p));
      if (unknown.length > 0) {
        Annotations.of(this).addWarningV2(
          `carddemo:asl-placeholders-${flow.name}`,
          `${flow.name}: unknown DefinitionSubstitutions ${unknown.join(', ')} (supported: ${[...known].join(', ')})`,
        );
      }
      const usedSubs = Object.fromEntries(Object.entries(substitutions).filter(([k]) => used.includes(k)));

      const sm = new sfn.StateMachine(this, `Flow-${flow.name}`, {
        stateMachineName: resourceName(cfg, flow.name),
        stateMachineType: sfn.StateMachineType.STANDARD,
        definitionBody: sfn.DefinitionBody.fromString(text),
        definitionSubstitutions: usedSubs,
        comment: flow.description,
        tracingEnabled: true,
        logs: {
          destination: new logs.LogGroup(this, `FlowLogs-${flow.name}`, {
            logGroupName: `/aws/vendedlogs/states/carddemo-${cfg.envName}-${flow.name}`,
            retention: cfg.logRetentionDays as logs.RetentionDays,
            removalPolicy: RemovalPolicy.DESTROY,
          }),
          level: sfn.LogLevel.ERROR,
          includeExecutionData: false,
        },
      });
      this.grantFlowAccess(sm, flow, jobs, props, this.definitionSources[flow.name] === 'file');
      this.stateMachines[flow.name] = sm;
    }

    for (const flow of props.flows) {
      const sm = this.stateMachines[flow.name];
      const trigger = flow.trigger;
      if (trigger.kind === 'schedule') {
        new events.Rule(this, `Schedule-${flow.name}`, {
          ruleName: resourceName(cfg, `${flow.name}-schedule`),
          description: `${flow.description} (UTC)`,
          schedule: events.Schedule.expression(cfg[trigger.cronKey]),
          targets: [new targets.SfnStateMachine(sm, { input: events.RuleTargetInput.fromObject({ trigger: 'schedule' }) })],
        });
      } else if (trigger.kind === 'after') {
        const parent = this.stateMachines[trigger.flow];
        if (!parent) throw new Error(`Flow ${flow.name} depends on unknown flow ${trigger.flow}`);
        new events.Rule(this, `After-${flow.name}`, {
          ruleName: resourceName(cfg, `${flow.name}-after-${trigger.flow}`).slice(0, 64),
          description: `Start ${flow.name} when ${trigger.flow} SUCCEEDED`,
          eventPattern: {
            source: ['aws.states'],
            detailType: ['Step Functions Execution Status Change'],
            detail: { status: ['SUCCEEDED'], stateMachineArn: [parent.stateMachineArn] },
          },
          targets: [new targets.SfnStateMachine(sm)],
        });
      }
    }
  }

  /**
   * Batch submitJob.sync needs SubmitJob/DescribeJobs/TerminateJob plus the managed EventBridge rule; each flow is
   * limited to its own job definitions. Externally supplied ASL files may start sibling flows, so they may also
   * start/describe `carddemo-<env>-*` executions.
   */
  private grantFlowAccess(sm: sfn.StateMachine, flow: FlowSpec, jobs: JobSpec[], props: BatchStackProps, external: boolean): void {
    const flowJobNames = external ? jobs.map((j) => j.name) : flowJobs(flow);
    sm.addToRolePolicy(
      new iam.PolicyStatement({
        actions: ['batch:SubmitJob'],
        resources: [this.jobQueue.jobQueueArn, ...flowJobNames.map((j) => this.jobDefinitionArnWildcard(j))],
      }),
    );
    sm.addToRolePolicy(new iam.PolicyStatement({ actions: ['batch:DescribeJobs', 'batch:TerminateJob'], resources: ['*'] }));
    sm.addToRolePolicy(
      new iam.PolicyStatement({
        actions: ['events:PutTargets', 'events:PutRule', 'events:DescribeRule'],
        resources: [
          this.formatArn({ service: 'events', resource: 'rule', resourceName: 'StepFunctionsGetEventsForBatchJobsRule' }),
        ],
      }),
    );
    props.dataBucket.grantRead(sm, 'runs/*');
    props.warningTopic.grantPublish(sm);
    if (external) {
      const prefix = resourceName(props.config, '');
      sm.addToRolePolicy(
        new iam.PolicyStatement({
          actions: ['states:StartExecution'],
          resources: [this.formatArn({ service: 'states', resource: 'stateMachine', resourceName: `${prefix}*`, arnFormat: ArnFormat.COLON_RESOURCE_NAME })],
        }),
      );
      sm.addToRolePolicy(
        new iam.PolicyStatement({
          actions: ['states:DescribeExecution', 'states:StopExecution'],
          resources: [this.formatArn({ service: 'states', resource: 'execution', resourceName: `${prefix}*`, arnFormat: ArnFormat.COLON_RESOURCE_NAME })],
        }),
      );
    }
  }

  private jobDefinitionArnWildcard(job: string): string {
    return this.formatArn({ service: 'batch', resource: 'job-definition', resourceName: `${this.jobDefinitions[job].jobDefinitionName}*`, arnFormat: ArnFormat.SLASH_RESOURCE_NAME });
  }

  private addReportDispatcher(cfg: CardDemoConfig, props: BatchStackProps): void {
    const report = this.stateMachines.report;
    if (!report) return;
    const queue = props.messaging.queue('report-request');
    const fn = new nodejs.NodejsFunction(this, 'ReportDispatcherFn', {
      functionName: resourceName(cfg, 'report-dispatcher'),
      entry: path.join(__dirname, '..', 'lambda', 'report-dispatcher', 'index.ts'),
      handler: 'handler',
      runtime: lambda.Runtime.NODEJS_22_X,
      architecture: lambda.Architecture.ARM_64,
      memorySize: 256,
      timeout: Duration.seconds(20),
      environment: { REPORT_STATE_MACHINE_ARN: report.stateMachineArn },
      logGroup: new logs.LogGroup(this, 'ReportDispatcherLogs', {
        logGroupName: `/carddemo/${cfg.envName}/report-dispatcher`,
        retention: cfg.logRetentionDays as logs.RetentionDays,
        removalPolicy: RemovalPolicy.DESTROY,
      }),
      bundling: { externalModules: ['@aws-sdk/*'], minify: true },
    });
    report.grantStartExecution(fn);
    fn.addEventSource(new SqsEventSource(queue, { batchSize: 10, reportBatchItemFailures: true }));
  }
}
