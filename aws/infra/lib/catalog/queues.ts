/** SQS queue catalogue from aws/contracts/messaging.md §2. Names are logical (without `carddemo-` prefix). */
export type QueueRole = 'request' | 'reply' | 'error';

export interface QueueSpec {
  /** Logical name without the `carddemo-` prefix, e.g. `pauth-request`. */
  readonly name: string;
  readonly role: QueueRole;
  readonly replaces: string;
}

export const QUEUES: readonly QueueSpec[] = [
  { name: 'pauth-request', role: 'request', replaces: 'AWS.M2.CARDDEMO.PAUTH.REQUEST (COPAUA0C trigger queue)' },
  { name: 'pauth-reply', role: 'reply', replaces: 'AWS.M2.CARDDEMO.PAUTH.REPLY (COPAUA0C MQMD-REPLYTOQ)' },
  { name: 'acct-inquiry-request', role: 'request', replaces: 'COACCT01 trigger queue (CDRA)' },
  { name: 'acct-inquiry-reply', role: 'reply', replaces: 'CARD.DEMO.REPLY.ACCT (COACCT01)' },
  { name: 'date-inquiry-request', role: 'request', replaces: 'CODATE01 trigger queue (CDRD)' },
  { name: 'date-inquiry-reply', role: 'reply', replaces: 'CARD.DEMO.REPLY.DATE (CODATE01)' },
  { name: 'error', role: 'error', replaces: 'CARD.DEMO.ERROR + CICS TD CSSL' },
  { name: 'report-request', role: 'request', replaces: 'CICS extrapartition TD queue JOBS (CORPT00C)' },
];

/** Request queue → default reply queue (messaging.md §1 "Reply routing" allowlist). */
export const REPLY_FOR: Readonly<Record<string, string>> = {
  'pauth-request': 'pauth-reply',
  'acct-inquiry-request': 'acct-inquiry-reply',
  'date-inquiry-request': 'date-inquiry-reply',
};

/** messaging.md §1 constants. */
export const QUEUE_SETTINGS = {
  maxReceiveCount: 5,
  dlqRetentionDays: 14,
  replyRetentionSeconds: 60,
  visibilityTimeoutSeconds: 30,
  receiveWaitSeconds: 5,
} as const;
