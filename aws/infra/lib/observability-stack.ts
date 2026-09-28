import { ArnFormat, CfnOutput, Duration, RemovalPolicy, Stack, type StackProps } from 'aws-cdk-lib';
import * as cloudwatch from 'aws-cdk-lib/aws-cloudwatch';
import * as cwActions from 'aws-cdk-lib/aws-cloudwatch-actions';
import * as events from 'aws-cdk-lib/aws-events';
import * as targets from 'aws-cdk-lib/aws-events-targets';
import * as logs from 'aws-cdk-lib/aws-logs';
import * as sns from 'aws-cdk-lib/aws-sns';
import * as subs from 'aws-cdk-lib/aws-sns-subscriptions';
import type { Construct } from 'constructs';
import type { FlowSpec } from './catalog/flows';
import { type CardDemoConfig, queuePrefix, resourceName } from './config';
import type { QueuePair } from './messaging-stack';

export interface ObservabilityStackProps extends StackProps {
  readonly config: CardDemoConfig;
  readonly queues: Record<string, QueuePair>;
  readonly flows: FlowSpec[];
}

/**
 * Central log groups, SNS topics and alarms. State machines and the Batch job queue are referenced by their
 * deterministic names (`carddemo-<env>-<name>`) so this stack can be deployed before the Batch stack (which
 * publishes RC=4 warnings to `warningTopic`) without a dependency cycle.
 */
export class ObservabilityStack extends Stack {
  readonly alarmTopic: sns.Topic;
  readonly warningTopic: sns.Topic;
  readonly servicesLogGroup: logs.LogGroup;
  readonly batchLogGroup: logs.LogGroup;
  readonly frontendLogGroup: logs.LogGroup;

  constructor(scope: Construct, id: string, props: ObservabilityStackProps) {
    super(scope, id, props);
    const cfg = props.config;
    const retention = cfg.logRetentionDays as logs.RetentionDays;
    const removalPolicy = cfg.retainData ? RemovalPolicy.RETAIN : RemovalPolicy.DESTROY;

    this.alarmTopic = new sns.Topic(this, 'AlarmTopic', { topicName: resourceName(cfg, 'alarms'), enforceSSL: true });
    this.warningTopic = new sns.Topic(this, 'WarningTopic', {
      topicName: resourceName(cfg, 'batch-warning'),
      displayName: 'CardDemo batch RC=4 warnings (batch.md §1.1)',
      enforceSSL: true,
    });
    if (cfg.alarmEmail) {
      this.alarmTopic.addSubscription(new subs.EmailSubscription(cfg.alarmEmail));
      this.warningTopic.addSubscription(new subs.EmailSubscription(cfg.alarmEmail));
    }

    const logGroup = (id: string, name: string) =>
      new logs.LogGroup(this, id, { logGroupName: `/carddemo/${cfg.envName}/${name}`, retention, removalPolicy });
    this.servicesLogGroup = logGroup('ServicesLogs', 'online-services');
    this.batchLogGroup = logGroup('BatchLogs', 'batch');
    this.frontendLogGroup = logGroup('FrontendLogs', 'frontend');

    const alarmAction = new cwActions.SnsAction(this.alarmTopic);
    const dashboardWidgets: cloudwatch.IWidget[] = [];

    for (const { spec, queue, dlq } of Object.values(props.queues)) {
      const dlqAlarm = dlq
        .metricApproximateNumberOfMessagesVisible({ period: Duration.minutes(1), statistic: 'Maximum' })
        .createAlarm(this, `${spec.name}-dlq-depth`, {
          alarmName: `${queuePrefix(cfg)}${spec.name}-dlq-depth`,
          alarmDescription: `Messages in DLQ of ${spec.name} (poison messages after 5 receives)`,
          threshold: 1,
          evaluationPeriods: 1,
          comparisonOperator: cloudwatch.ComparisonOperator.GREATER_THAN_OR_EQUAL_TO_THRESHOLD,
          treatMissingData: cloudwatch.TreatMissingData.NOT_BREACHING,
        });
      dlqAlarm.addAlarmAction(alarmAction);
      dashboardWidgets.push(
        new cloudwatch.GraphWidget({
          title: spec.name,
          left: [queue.metricApproximateNumberOfMessagesVisible(), dlq.metricApproximateNumberOfMessagesVisible()],
          width: 6,
        }),
      );
    }

    const errorQueue = props.queues.error?.queue;
    if (errorQueue) {
      errorQueue
        .metricApproximateNumberOfMessagesVisible({ period: Duration.minutes(5), statistic: 'Maximum' })
        .createAlarm(this, 'error-queue-depth', {
          alarmName: `${queuePrefix(cfg)}error-depth`,
          alarmDescription: 'Consumer errors written to carddemo-error (messaging.md §5)',
          threshold: 1,
          evaluationPeriods: 1,
          comparisonOperator: cloudwatch.ComparisonOperator.GREATER_THAN_OR_EQUAL_TO_THRESHOLD,
          treatMissingData: cloudwatch.TreatMissingData.NOT_BREACHING,
        })
        .addAlarmAction(alarmAction);
    }

    for (const flow of props.flows) {
      const stateMachineArn = this.formatArn({
        service: 'states',
        resource: 'stateMachine',
        resourceName: resourceName(cfg, flow.name),
        arnFormat: ArnFormat.COLON_RESOURCE_NAME,
      });
      const dims = { StateMachineArn: stateMachineArn };
      for (const metricName of ['ExecutionsFailed', 'ExecutionsTimedOut', 'ExecutionsAborted']) {
        new cloudwatch.Metric({ namespace: 'AWS/States', metricName, dimensionsMap: dims, period: Duration.minutes(5), statistic: 'Sum' })
          .createAlarm(this, `${flow.name}-${metricName}`, {
            alarmName: `${resourceName(cfg, flow.name)}-${metricName}`,
            alarmDescription: `Step Functions ${flow.name}: ${metricName} (${flow.description})`,
            threshold: 1,
            evaluationPeriods: 1,
            comparisonOperator: cloudwatch.ComparisonOperator.GREATER_THAN_OR_EQUAL_TO_THRESHOLD,
            treatMissingData: cloudwatch.TreatMissingData.NOT_BREACHING,
          })
          .addAlarmAction(alarmAction);
      }
    }

    new events.Rule(this, 'BatchJobFailedRule', {
      ruleName: resourceName(cfg, 'batch-job-failed'),
      description: 'AWS Batch job FAILED (RC >= 8) on the CardDemo job queue',
      eventPattern: {
        source: ['aws.batch'],
        detailType: ['Batch Job State Change'],
        detail: {
          status: ['FAILED'],
          jobQueue: [this.formatArn({ service: 'batch', resource: 'job-queue', resourceName: resourceName(cfg, 'batch-queue') })],
        },
      },
      targets: [
        new targets.SnsTopic(this.alarmTopic, {
          message: events.RuleTargetInput.fromText(
            `CardDemo batch job ${events.EventField.fromPath('$.detail.jobName')} FAILED: ` +
              `${events.EventField.fromPath('$.detail.statusReason')}`,
          ),
        }),
      ],
    });

    new cloudwatch.Dashboard(this, 'Dashboard', {
      dashboardName: resourceName(cfg, 'overview'),
      widgets: [dashboardWidgets],
    });

    new CfnOutput(this, 'AlarmTopicArn', { value: this.alarmTopic.topicArn });
  }
}
