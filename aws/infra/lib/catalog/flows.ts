/**
 * Step Functions flows from aws/contracts/batch.md §3. Each flow becomes state machine `carddemo-<env>-<name>`.
 * `args` are JSONata expressions (evaluated inside the state machine) appended to the job command after the
 * standard `--job`, `--runId`, `--businessDate` arguments. `$execInput` is the execution input.
 */
export interface FlowJobStep {
  readonly job: string;
  /** JSONata expression yielding an array of extra CLI arguments. */
  readonly args?: string;
}

export type FlowStep = FlowJobStep | { readonly parallel: FlowJobStep[] };

export type FlowTrigger =
  | { readonly kind: 'schedule'; readonly cronKey: 'dailyCycleCron' | 'weeklyRefreshCron' }
  | { readonly kind: 'after'; readonly flow: string }
  | { readonly kind: 'onDemand' };

/**
 * `monthStart`: run only when businessDate is the 1st of the month (or input `force: true`).
 * `parentNotSkipped`: run only when the triggering execution's output has `skipped: false` (or month start
 * when started manually, or `force: true`).
 */
export type FlowGate = 'monthStart' | 'parentNotSkipped';

export interface FlowSpec {
  readonly name: string;
  readonly description: string;
  readonly steps: FlowStep[];
  readonly trigger: FlowTrigger;
  readonly gate?: FlowGate;
  /** Legacy WAITSTEP (COBSWAIT PARM 00003600 centiseconds = 36 s) → `Wait` state. */
  readonly waitSeconds?: number;
}

const WAIT = 36;

export function flowsFor(enableAuthModule: boolean): FlowSpec[] {
  return [
    {
      name: 'daily-cycle',
      description: 'CA-7 SCHID 030: [CBPAUP0J] -> POSTTRAN -> WAITSTEP',
      steps: [
        ...(enableAuthModule
          ? [{ job: 'purge-expired-authorizations', args: "$exists($execInput.expiryDays) ? ['--expiryDays=' & $string($execInput.expiryDays)] : []" }]
          : []),
        { job: 'post-daily-transactions' },
      ],
      trigger: { kind: 'schedule', cronKey: 'dailyCycleCron' },
      waitSeconds: WAIT,
    },
    {
      name: 'daily-backup',
      description: 'Control-M DAILY-TransactionBackup: TRANBKP -> WAITSTEP',
      steps: [{ job: 'backup-transactions' }],
      trigger: { kind: 'after', flow: 'daily-cycle' },
      waitSeconds: WAIT,
    },
    {
      name: 'monthly-interest',
      description: 'Control-M MONTHLY-InterestCalculation: INTCALC -> COMBTRAN -> WAITSTEP (1st of month)',
      steps: [{ job: 'calculate-interest' }, { job: 'combine-transactions' }],
      trigger: { kind: 'after', flow: 'daily-backup' },
      gate: 'monthStart',
      waitSeconds: WAIT,
    },
    {
      name: 'statements',
      description: 'CA-7 SCHID 030: CREASTMT -> TXT2PDF1 -> WAITSTEP (after monthly interest)',
      steps: [{ job: 'create-statements' }, { job: 'statement-pdf' }],
      trigger: { kind: 'after', flow: 'monthly-interest' },
      gate: 'parentNotSkipped',
      waitSeconds: WAIT,
    },
    {
      name: 'weekly-trantype-refresh',
      description: 'Control-M WEEKLY-TransactionTypesDBRefresh: MNTTRDB2 -> TRANEXTR (Saturdays)',
      steps: [{ job: 'maintain-transaction-types' }, { job: 'extract-transaction-types' }],
      trigger: { kind: 'schedule', cronKey: 'weeklyRefreshCron' },
    },
    {
      name: 'weekly-disclosure-refresh',
      description: 'Control-M WEEKLY-DisclosureGroupsRefresh: DISCGRP -> WAITSTEP (after trantype refresh)',
      steps: [
        { job: 'backup-reference-data', args: "['--table=disclosure_group']" },
        { job: 'load-reference-data', args: "['--table=disclosure_group']" },
      ],
      trigger: { kind: 'after', flow: 'weekly-trantype-refresh' },
      waitSeconds: WAIT,
    },
    {
      name: 'report',
      description: 'CORPT00C -> TD JOBS -> TRANREPT (started by the report dispatcher from SQS carddemo-report-request)',
      steps: [{ job: 'transaction-report', args: "['--startDate=' & $execInput.startDate, '--endDate=' & $execInput.endDate]" }],
      trigger: { kind: 'onDemand' },
    },
    {
      name: 'extracts',
      description: 'CA-7 READACCT/READCARD/READCUST/READXREF (parallel)',
      steps: [{ parallel: [{ job: 'extract-accounts' }, { job: 'print-cards' }, { job: 'print-customers' }, { job: 'print-xref' }] }],
      trigger: { kind: 'onDemand' },
    },
    {
      name: 'export',
      description: 'CBEXPORT.jcl',
      steps: [{ job: 'export-customer-data', args: "$exists($execInput.branchId) ? ['--branchId=' & $string($execInput.branchId)] : []" }],
      trigger: { kind: 'onDemand' },
    },
    {
      name: 'import',
      description: 'CBIMPORT.jcl',
      steps: [
        {
          job: 'import-customer-data',
          args: "['--exportKey=' & $execInput.exportKey, '--load=' & ($exists($execInput.load) ? $string($execInput.load) : 'false')]",
        },
      ],
      trigger: { kind: 'onDemand' },
    },
  ];
}

export function isParallel(step: FlowStep): step is { readonly parallel: FlowJobStep[] } {
  return 'parallel' in step;
}

export function flowJobs(flow: FlowSpec): string[] {
  return flow.steps.flatMap((s) => (isParallel(s) ? s.parallel.map((p) => p.job) : [s.job]));
}
