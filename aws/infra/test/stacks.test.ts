import * as fs from 'node:fs';
import * as path from 'node:path';
import { Template, Match } from 'aws-cdk-lib/assertions';
import { QUEUES } from '../lib/catalog/queues';
import { synthApp, tempDir } from './helpers';

const stacks = synthApp();
const t = {
  network: Template.fromStack(stacks.network),
  storage: Template.fromStack(stacks.storage),
  data: Template.fromStack(stacks.data),
  messaging: Template.fromStack(stacks.messaging),
  observability: Template.fromStack(stacks.observability),
  services: Template.fromStack(stacks.services),
  batch: Template.fromStack(stacks.batch),
};

describe('Network', () => {
  test('VPC with public/private/isolated subnets in 3 AZs', () => {
    t.network.resourceCountIs('AWS::EC2::VPC', 1);
    t.network.resourceCountIs('AWS::EC2::Subnet', 9);
    t.network.resourceCountIs('AWS::EC2::NatGateway', 1);
  });

  test('VPC endpoints for S3, SQS, ECR, Secrets Manager, Logs', () => {
    t.network.hasResourceProperties('AWS::EC2::VPCEndpoint', { VpcEndpointType: 'Gateway' });
    const endpoints = t.network.findResources('AWS::EC2::VPCEndpoint', { Properties: { VpcEndpointType: 'Interface' } });
    const services = JSON.stringify(Object.values(endpoints).map((r) => r.Properties.ServiceName));
    for (const svc of ['.sqs', '.ecr.api', '.ecr.dkr', '.secretsmanager', '.logs', '.states']) expect(services).toContain(svc);
  });

  test('Aurora SG only admits services, batch and bootstrap on 5432', () => {
    const ingress = t.network.findResources('AWS::EC2::SecurityGroupIngress', { Properties: { FromPort: 5432 } });
    expect(Object.keys(ingress)).toHaveLength(3);
  });
});

describe('Storage', () => {
  test('encrypted, private, versioned data bucket with lifecycle rules', () => {
    t.storage.hasResourceProperties('AWS::S3::Bucket', {
      BucketEncryption: { ServerSideEncryptionConfiguration: [{ ServerSideEncryptionByDefault: { SSEAlgorithm: 'AES256' } }] },
      PublicAccessBlockConfiguration: { BlockPublicAcls: true, BlockPublicPolicy: true, IgnorePublicAcls: true, RestrictPublicBuckets: true },
      VersioningConfiguration: { Status: 'Enabled' },
      LifecycleConfiguration: {
        Rules: Match.arrayWith([
          Match.objectLike({ Prefix: 'output/', ExpirationInDays: 90 }),
          Match.objectLike({ Prefix: 'backup/' }),
          Match.objectLike({ Prefix: 'statements/' }),
        ]),
      },
    });
  });
});

describe('Data', () => {
  test('Aurora PostgreSQL Serverless v2 with generated secret', () => {
    t.data.hasResourceProperties('AWS::RDS::DBCluster', {
      Engine: 'aurora-postgresql',
      DatabaseName: 'carddemo',
      StorageEncrypted: true,
      ServerlessV2ScalingConfiguration: { MinCapacity: 0.5, MaxCapacity: 4 },
    });
    t.data.hasResourceProperties('AWS::RDS::DBInstance', { DBInstanceClass: 'db.serverless', PubliclyAccessible: false });
    t.data.hasResourceProperties('AWS::SecretsManager::Secret', { Name: 'carddemo/dev/db-credentials' });
  });

  test('no schema bootstrap when schema.sql is absent', () => {
    t.data.resourceCountIs('Custom::CardDemoSchema', 0);
    expect(stacks.data.schemaApplied).toBe(false);
  });

  test('schema bootstrap custom resource keyed on schema hash when schema.sql exists', () => {
    const dir = tempDir('carddemo-schema-');
    const file = path.join(dir, 'schema.sql');
    fs.writeFileSync(file, 'CREATE TABLE IF NOT EXISTS account (acct_id bigint primary key);\n');
    const withSchema = synthApp({ schemaFile: file });
    const tpl = Template.fromStack(withSchema.data);
    tpl.hasResourceProperties('Custom::CardDemoSchema', { SchemaHash: Match.stringLikeRegexp('^[0-9a-f]{64}$'), Schema: 'carddemo' });
    tpl.hasResourceProperties('AWS::Lambda::Function', {
      Environment: { Variables: Match.objectLike({ DB_NAME: 'carddemo', DB_SCHEMA: 'carddemo' }) },
      VpcConfig: Match.anyValue(),
    });
  });
});

describe('Messaging', () => {
  test('every contract queue has a DLQ with maxReceiveCount 5', () => {
    t.messaging.resourceCountIs('AWS::SQS::Queue', QUEUES.length * 2);
    for (const q of QUEUES) {
      t.messaging.hasResourceProperties('AWS::SQS::Queue', {
        QueueName: `carddemo-dev-${q.name}`,
        VisibilityTimeout: 30,
        ReceiveMessageWaitTimeSeconds: 5,
        RedrivePolicy: { maxReceiveCount: 5, deadLetterTargetArn: Match.anyValue() },
        SqsManagedSseEnabled: true,
      });
      t.messaging.hasResourceProperties('AWS::SQS::Queue', { QueueName: `carddemo-dev-${q.name}-dlq`, MessageRetentionPeriod: 1209600 });
    }
  });

  test('reply queues retain messages for 60 s', () => {
    t.messaging.hasResourceProperties('AWS::SQS::Queue', { QueueName: 'carddemo-dev-pauth-reply', MessageRetentionPeriod: 60 });
  });
});

describe('Observability', () => {
  test('SNS topics, DLQ alarms, flow failure alarms, batch failure rule', () => {
    t.observability.resourceCountIs('AWS::SNS::Topic', 2);
    const alarms = t.observability.findResources('AWS::CloudWatch::Alarm');
    const names = Object.values(alarms).map((a) => a.Properties.AlarmName as string);
    for (const q of QUEUES) expect(names).toContain(`carddemo-dev-${q.name}-dlq-depth`);
    expect(names).toContain('carddemo-dev-daily-cycle-ExecutionsFailed');
    t.observability.hasResourceProperties('AWS::Events::Rule', {
      EventPattern: { source: ['aws.batch'], 'detail-type': ['Batch Job State Change'], detail: Match.objectLike({ status: ['FAILED'] }) },
    });
    t.observability.hasResourceProperties('AWS::Logs::LogGroup', { LogGroupName: '/carddemo/dev/online-services', RetentionInDays: 30 });
  });
});

describe('Services', () => {
  test('ECR repositories for services and frontend', () => {
    t.services.hasResourceProperties('AWS::ECR::Repository', { RepositoryName: 'carddemo-dev-online-services' });
    t.services.hasResourceProperties('AWS::ECR::Repository', { RepositoryName: 'carddemo-dev-frontend' });
  });

  test('Fargate task wires conventions.md env vars and secrets', () => {
    const tds = t.services.findResources('AWS::ECS::TaskDefinition');
    const [container] = Object.values(tds)[0].Properties.ContainerDefinitions;
    const envNames = container.Environment.map((e: { Name: string }) => e.Name);
    for (const v of ['DB_HOST', 'DB_PORT', 'DB_NAME', 'DB_SCHEMA', 'S3_BUCKET', 'AWS_REGION', 'SQS_QUEUE_PREFIX', 'SERVER_PORT', 'JWT_TTL_MINUTES']) {
      expect(envNames).toContain(v);
    }
    expect(container.Secrets.map((s: { Name: string }) => s.Name).sort()).toEqual(['DB_PASSWORD', 'DB_USER', 'JWT_SECRET']);
    expect(container.PortMappings[0].ContainerPort).toBe(8080);
    expect(container.Environment.find((e: { Name: string }) => e.Name === 'SQS_QUEUE_PREFIX').Value).toBe('carddemo-dev-');
  });

  test('ALB health check on /actuator/health and CloudFront routes /api/*', () => {
    t.services.hasResourceProperties('AWS::ElasticLoadBalancingV2::TargetGroup', { HealthCheckPath: '/actuator/health', Port: 8080, TargetType: 'ip' });
    t.services.hasResourceProperties('AWS::ECS::Service', {
      LaunchType: 'FARGATE',
      NetworkConfiguration: { AwsvpcConfiguration: Match.objectLike({ AssignPublicIp: 'DISABLED' }) },
    });
    t.services.hasResourceProperties('AWS::CloudFront::Distribution', {
      DistributionConfig: Match.objectLike({ CacheBehaviors: Match.arrayWith([Match.objectLike({ PathPattern: '/api/*' })]) }),
    });
  });

  test('ALB only forwards requests carrying the CloudFront origin-verify header', () => {
    t.services.hasResourceProperties('AWS::ElasticLoadBalancingV2::Listener', {
      Port: 80,
      DefaultActions: [Match.objectLike({ Type: 'fixed-response', FixedResponseConfig: Match.objectLike({ StatusCode: '403' }) })],
    });
    t.services.hasResourceProperties('AWS::ElasticLoadBalancingV2::ListenerRule', {
      Actions: [Match.objectLike({ Type: 'forward' })],
      Conditions: [Match.objectLike({ Field: 'http-header', HttpHeaderConfig: Match.objectLike({ HttpHeaderName: 'X-CardDemo-Origin-Verify' }) })],
    });
    const dist = Object.values(t.services.findResources('AWS::CloudFront::Distribution'))[0];
    const albOrigin = dist.Properties.DistributionConfig.Origins.find((o: { CustomOriginConfig?: unknown }) => o.CustomOriginConfig);
    expect(albOrigin.OriginCustomHeaders[0].HeaderName).toBe('X-CardDemo-Origin-Verify');
  });

  test('task role can only send to allowlisted reply/error/report queues', () => {
    const policies = t.services.findResources('AWS::IAM::Policy');
    const statements = Object.values(policies).flatMap((p) => p.Properties.PolicyDocument.Statement);
    const sendResources = JSON.stringify(
      statements.filter((s) => [s.Action].flat().includes('sqs:SendMessage')).map((s) => s.Resource),
    );
    for (const allowed of ['pauthreply', 'acctinquiryreply', 'dateinquiryreply', 'error', 'reportrequest']) {
      expect(sendResources.replace(/-/g, '')).toContain(allowed);
    }
    for (const denied of ['pauthrequest', 'acctinquiryrequest', 'dateinquiryrequest']) {
      expect(sendResources.replace(/-/g, '')).not.toMatch(new RegExp(`${denied}[A-F0-9]{8}"`));
    }
    const consumeResources = JSON.stringify(
      statements.filter((s) => [s.Action].flat().includes('sqs:ReceiveMessage')).map((s) => s.Resource),
    ).replace(/-/g, '');
    expect(consumeResources).toContain('pauthrequest');
    expect(consumeResources).not.toContain('pauthreply');
  });
});

describe('Batch', () => {
  test('Fargate compute environment and job queue', () => {
    t.batch.hasResourceProperties('AWS::Batch::ComputeEnvironment', {
      Type: 'managed',
      ComputeResources: Match.objectLike({ Type: 'FARGATE', MaxvCpus: 16 }),
    });
    t.batch.hasResourceProperties('AWS::Batch::JobQueue', { JobQueueName: 'carddemo-dev-batch-queue' });
  });

  test('one job definition per job, 1 vCPU / 2 GiB, --job=<name>', () => {
    t.batch.hasResourceProperties('AWS::Batch::JobDefinition', {
      JobDefinitionName: 'carddemo-dev-post-daily-transactions',
      PlatformCapabilities: ['FARGATE'],
      ContainerProperties: Match.objectLike({
        Command: ['java', '-jar', 'carddemo-batch.jar', '--job=post-daily-transactions'],
        ResourceRequirements: Match.arrayWith([{ Type: 'MEMORY', Value: '2048' }, { Type: 'VCPU', Value: '1' }]),
        Secrets: Match.arrayWith([Match.objectLike({ Name: 'DB_PASSWORD' })]),
      }),
    });
    const defs = Object.values(t.batch.findResources('AWS::Batch::JobDefinition')).map((d) => d.Properties.JobDefinitionName);
    expect(defs).not.toContain('carddemo-dev-purge-expired-authorizations');
    expect(defs).toContain('carddemo-dev-etl');
  });

  test('state machines, daily 02:00 UTC schedule, and chaining rules', () => {
    t.batch.hasResourceProperties('AWS::StepFunctions::StateMachine', { StateMachineName: 'carddemo-dev-daily-cycle' });
    t.batch.hasResourceProperties('AWS::StepFunctions::StateMachine', { StateMachineName: 'carddemo-dev-report' });
    t.batch.hasResourceProperties('AWS::Events::Rule', { ScheduleExpression: 'cron(0 2 * * ? *)' });
    t.batch.hasResourceProperties('AWS::Events::Rule', {
      EventPattern: Match.objectLike({ source: ['aws.states'], detail: Match.objectLike({ status: ['SUCCEEDED'] }) }),
    });
    expect(stacks.batch.definitionSources['daily-cycle']).toBe('generated');
  });

  test('report dispatcher consumes carddemo-report-request', () => {
    t.batch.hasResourceProperties('AWS::Lambda::EventSourceMapping', { FunctionResponseTypes: ['ReportBatchItemFailures'] });
  });

  test('job roles are scoped to their S3 prefixes', () => {
    const policies = Object.values(t.batch.findResources('AWS::IAM::Policy'));
    const postPolicy = policies.find((p) => JSON.stringify(p.Properties.Roles).includes('JobRolepostdailytransactions'));
    const text = JSON.stringify(postPolicy);
    expect(text).toContain('input/dalytran/*');
    expect(text).toContain('output/dalyrejs/*');
    expect(text).not.toContain('statements/*');
  });
});

describe('options', () => {
  test('state machine loaded from stateMachineDir when present', () => {
    const dir = tempDir('carddemo-asl-');
    const asl = {
      Comment: 'from batch session',
      StartAt: 'Post',
      States: {
        Post: {
          Type: 'Task',
          Resource: 'arn:${Partition}:states:::batch:submitJob.sync',
          Parameters: { JobName: 'post', JobQueue: '${JobQueueArn}', JobDefinition: '${JobDefinition_post_daily_transactions}' },
          End: true,
        },
      },
    };
    fs.writeFileSync(path.join(dir, 'daily-cycle.asl.json'), JSON.stringify(asl));
    const s = synthApp({ stateMachineDir: dir });
    expect(s.batch.definitionSources['daily-cycle']).toBe('file');
    expect(s.batch.definitionSources.report).toBe('generated');
    const tpl = Template.fromStack(s.batch);
    tpl.hasResourceProperties('AWS::StepFunctions::StateMachine', {
      StateMachineName: 'carddemo-dev-daily-cycle',
      DefinitionString: Match.stringLikeRegexp('from batch session'),
      DefinitionSubstitutions: Match.objectLike({ JobQueueArn: Match.anyValue(), JobDefinition_post_daily_transactions: Match.anyValue() }),
    });
  });

  test('supplied ASL with an unknown placeholder fails synth', () => {
    const dir = tempDir('asl-bad-');
    const asl = { StartAt: 'P', States: { P: { Type: 'Pass', Result: '${NoSuchThing}', End: true } } };
    fs.writeFileSync(path.join(dir, 'daily-cycle.asl.json'), JSON.stringify(asl));
    expect(() => synthApp({ stateMachineDir: dir })).toThrow(/unknown DefinitionSubstitutions NoSuchThing/);
  });

  test('auth module adds purge job; 2 AZ / no NAT variant', () => {
    const s = synthApp({ enableAuthModule: true, azCount: 2, natGateways: 0, envName: 'qa' });
    Template.fromStack(s.batch).hasResourceProperties('AWS::Batch::JobDefinition', {
      JobDefinitionName: 'carddemo-qa-purge-expired-authorizations',
    });
    const net = Template.fromStack(s.network);
    net.resourceCountIs('AWS::EC2::Subnet', 6);
    net.resourceCountIs('AWS::EC2::NatGateway', 0);
  });

  test('prod keeps data and enables deletion protection', () => {
    const s = synthApp({ envName: 'prod' });
    Template.fromStack(s.data).hasResourceProperties('AWS::RDS::DBCluster', { DeletionProtection: true });
    Template.fromStack(s.data).resourceCountIs('AWS::RDS::DBInstance', 2);
    Template.fromStack(s.storage).hasResource('AWS::S3::Bucket', { DeletionPolicy: 'Retain' });
    Template.fromStack(s.services).hasResource('AWS::S3::Bucket', { DeletionPolicy: 'Retain' });
    expect(Object.keys(Template.fromStack(s.services).findResources('Custom::S3AutoDeleteObjects'))).toHaveLength(0);
  });
});
