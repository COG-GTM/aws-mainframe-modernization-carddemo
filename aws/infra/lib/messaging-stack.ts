import { CfnOutput, Duration, Stack, type StackProps } from 'aws-cdk-lib';
import * as sqs from 'aws-cdk-lib/aws-sqs';
import type { Construct } from 'constructs';
import { QUEUES, QUEUE_SETTINGS, type QueueSpec } from './catalog/queues';
import { type CardDemoConfig, queuePrefix } from './config';

export interface MessagingStackProps extends StackProps {
  readonly config: CardDemoConfig;
}

export interface QueuePair {
  readonly spec: QueueSpec;
  readonly queue: sqs.Queue;
  readonly dlq: sqs.Queue;
}

/**
 * SQS queues + DLQs from aws/contracts/messaging.md §2 (replaces IBM MQ for COPAUA0C, COACCT01, CODATE01 and the
 * CICS TD queues JOBS/CSSL). Physical name = `SQS_QUEUE_PREFIX` + logical name, prefix `carddemo-<env>-`.
 */
export class MessagingStack extends Stack {
  readonly queues: Record<string, QueuePair> = {};
  readonly prefix: string;

  constructor(scope: Construct, id: string, props: MessagingStackProps) {
    super(scope, id, props);
    const cfg = props.config;
    this.prefix = queuePrefix(cfg);

    for (const spec of QUEUES) {
      const name = `${this.prefix}${spec.name}`;
      const dlq = new sqs.Queue(this, `${spec.name}-dlq`, {
        queueName: `${name}-dlq`,
        retentionPeriod: Duration.days(QUEUE_SETTINGS.dlqRetentionDays),
        encryption: sqs.QueueEncryption.SQS_MANAGED,
        enforceSSL: true,
      });
      const queue = new sqs.Queue(this, spec.name, {
        queueName: name,
        visibilityTimeout: Duration.seconds(QUEUE_SETTINGS.visibilityTimeoutSeconds),
        receiveMessageWaitTime: Duration.seconds(QUEUE_SETTINGS.receiveWaitSeconds),
        retentionPeriod: spec.role === 'reply' ? Duration.seconds(QUEUE_SETTINGS.replyRetentionSeconds) : Duration.days(4),
        encryption: sqs.QueueEncryption.SQS_MANAGED,
        enforceSSL: true,
        deadLetterQueue: { queue: dlq, maxReceiveCount: QUEUE_SETTINGS.maxReceiveCount },
      });
      this.queues[spec.name] = { spec, queue, dlq };
    }

    new CfnOutput(this, 'SqsQueuePrefix', { value: this.prefix });
  }

  queue(name: string): sqs.Queue {
    const pair = this.queues[name];
    if (!pair) throw new Error(`Unknown queue ${name}`);
    return pair.queue;
  }
}
