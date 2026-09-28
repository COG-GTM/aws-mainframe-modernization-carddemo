import { ExecutionAlreadyExists, SFNClient, StartExecutionCommand } from '@aws-sdk/client-sfn';

/** SQS event (subset used here). */
interface SqsEvent {
  Records: { messageId: string; body: string }[];
}

interface ReportRequest {
  messageId?: string;
  reportType?: string;
  startDate?: string;
  endDate?: string;
}

const sfn = new SFNClient({});
const DATE = /^\d{4}-\d{2}-\d{2}$/;

/**
 * messaging.md §6: start `carddemo-report` with name = runId = body.messageId (the API requestId) and
 * businessDate = endDate. A duplicate delivery hits ExecutionAlreadyExists and is treated as done (dedupe).
 * Invalid bodies are reported as failures so they reach the DLQ after maxReceiveCount.
 */
export async function handler(event: SqsEvent): Promise<{ batchItemFailures: { itemIdentifier: string }[] }> {
  const failures: { itemIdentifier: string }[] = [];
  for (const record of event.Records) {
    try {
      const req = JSON.parse(record.body) as ReportRequest;
      const id = req.messageId ?? record.messageId;
      if (!/^[A-Za-z0-9_-]{1,80}$/.test(id)) throw new Error(`invalid messageId ${id}`);
      if (!req.startDate || !req.endDate || !DATE.test(req.startDate) || !DATE.test(req.endDate)) {
        throw new Error('startDate/endDate must be yyyy-MM-dd');
      }
      const input = { startDate: req.startDate, endDate: req.endDate, runId: id, businessDate: req.endDate, reportType: req.reportType };
      try {
        await sfn.send(
          new StartExecutionCommand({ stateMachineArn: process.env.REPORT_STATE_MACHINE_ARN, name: id, input: JSON.stringify(input) }),
        );
        console.log(JSON.stringify({ msg: 'report execution started', requestId: id }));
      } catch (err) {
        if (!(err instanceof ExecutionAlreadyExists)) throw err;
        console.log(JSON.stringify({ msg: 'duplicate report request ignored', requestId: id }));
      }
    } catch (err) {
      console.error(JSON.stringify({ msg: 'report dispatch failed', sqsMessageId: record.messageId, error: String(err) }));
      failures.push({ itemIdentifier: record.messageId });
    }
  }
  return { batchItemFailures: failures };
}
