import { type FlowJobStep, type FlowSpec, isParallel } from './flows';
import { jobDefinitionKey } from './jobs';

/**
 * Generates the default (JSONata) Amazon States Language definition for a flow, used when the batch session
 * has not provided `<flow>.asl.json`. Placeholders `${Name}` are resolved through CloudFormation
 * DefinitionSubstitutions (see `aslSubstitutionKeys`).
 *
 * Per-job pattern (batch.md §1.1):
 *   Run_<job>     batch:submitJob.sync (container exits 0 for RC 0/4, non-zero for 8/12/16 → Batch FAILED → flow fails)
 *   ReadRc_<job>  aws-sdk:s3:getObject runs/<runId>/<job>.json
 *   CheckRc_<job> RC 0 → next, RC 4 → Warn_<job> (SNS carddemo-batch-warning) → next, else → fail
 */

export type AslDocument = { StartAt: string; States: Record<string, AslState>; [k: string]: unknown };
export type AslState = { Type: string; [k: string]: unknown };

export const BASE_SUBSTITUTION_KEYS = [
  'Partition',
  'Region',
  'AccountId',
  'EnvName',
  'JobQueueArn',
  'JobDefinitionPrefix',
  'DataBucket',
  'WarningTopicArn',
  'StateMachinePrefix',
] as const;

export function aslSubstitutionKeys(jobs: string[]): string[] {
  return [...BASE_SUBSTITUTION_KEYS, ...jobs.map(jobDefinitionKey)];
}

/** Output of the parent execution when started by an EventBridge "after <flow>" chaining rule. */
const PARENT = '$parse($states.input.detail.output)';
const HAS_PARENT = '$exists($states.input.detail.output)';

const BUSINESS_DATE =
  "{% $exists($states.input.businessDate) ? $states.input.businessDate : " +
  `(${HAS_PARENT} and $exists(${PARENT}.businessDate)) ? ${PARENT}.businessDate : ` +
  "$now('[Y0001]-[M01]-[D01]') %}";

const RUN_ID =
  "{% $exists($states.input.runId) ? $states.input.runId : " +
  `(${HAS_PARENT} and $exists(${PARENT}.runId)) ? ${PARENT}.runId : ` +
  "$now('[Y0001][M01][D01]T[H01][m01][s01]Z') & '-' & $substring($replace($uuid(), '-', ''), 0, 8) %}";

const FORCE =
  '{% ($exists($states.input.force) and $states.input.force = true) or ' +
  `(${HAS_PARENT} and $exists(${PARENT}.force) and ${PARENT}.force = true) %}`;

const GATE_CONDITIONS: Record<NonNullable<FlowSpec['gate']>, string> = {
  monthStart: "{% $substring($businessDate, 8, 2) = '01' or $force %}",
  parentNotSkipped:
    "{% $force or ($exists($execInput.detail.output) ? $parse($execInput.detail.output).skipped = false : " +
    "$substring($businessDate, 8, 2) = '01') %}",
};

function varName(job: string): string {
  return `rc_${job.replace(/-/g, '_')}`;
}

function jobStates(flow: FlowSpec, step: FlowJobStep, commandPrefix: string[], next: string): Record<string, AslState> {
  const { job } = step;
  const rc = varName(job);
  const prefix = commandPrefix.map((s) => JSON.stringify(s)).join(', ');
  const command =
    `{% $append([${prefix}${prefix ? ', ' : ''}"--job=${job}", "--runId=" & $runId, "--businessDate=" & $businessDate], ` +
    `${step.args ?? '[]'}) %}`;
  return {
    [`Run_${job}`]: {
      Type: 'Task',
      Comment: `AWS Batch job carddemo-<env>-${job}`,
      Resource: 'arn:${Partition}:states:::batch:submitJob.sync',
      Arguments: {
        JobName: `{% '${job}-' & $runId %}`,
        JobQueue: '${JobQueueArn}',
        JobDefinition: `\${${jobDefinitionKey(job)}}`,
        ContainerOverrides: { Command: command },
      },
      Retry: [
        {
          ErrorEquals: ['Batch.AWSBatchException', 'Batch.TooManyRequestsException'],
          IntervalSeconds: 10,
          MaxAttempts: 3,
          BackoffRate: 2,
        },
      ],
      Next: `ReadRc_${job}`,
    },
    [`ReadRc_${job}`]: {
      Type: 'Task',
      Comment: 'Read s3://<bucket>/runs/<runId>/<job>.json written by the job (batch.md §1.1)',
      Resource: 'arn:${Partition}:states:::aws-sdk:s3:getObject',
      Arguments: { Bucket: '${DataBucket}', Key: `{% 'runs/' & $runId & '/${job}.json' %}` },
      Assign: { [rc]: '{% $parse($states.result.Body).returnCode %}' },
      Output: '{% $parse($states.result.Body) %}',
      Catch: [{ ErrorEquals: ['States.ALL'], Next: `MissingRc_${job}` }],
      Next: `CheckRc_${job}`,
    },
    [`CheckRc_${job}`]: {
      Type: 'Choice',
      Choices: [
        { Condition: `{% $${rc} = 0 %}`, Next: next },
        { Condition: `{% $${rc} = 4 %}`, Next: `Warn_${job}` },
      ],
      Default: `Failed_${job}`,
    },
    [`Warn_${job}`]: {
      Type: 'Task',
      Resource: 'arn:${Partition}:states:::sns:publish',
      Arguments: {
        TopicArn: '${WarningTopicArn}',
        Subject: `CardDemo batch warning: ${job} RC=4`,
        Message: `{% $string({'flow': '${flow.name}', 'job': '${job}', 'runId': $runId, 'businessDate': $businessDate, 'returnCode': 4}) %}`,
      },
      Next: next,
    },
    [`MissingRc_${job}`]: {
      Type: 'Fail',
      Error: 'MissingReturnCode',
      Cause: `runs/<runId>/${job}.json was not written or is not valid JSON`,
    },
    [`Failed_${job}`]: {
      Type: 'Fail',
      Error: 'BatchReturnCode',
      Cause: `${job} returned a return code other than 0 or 4`,
    },
  };
}

function firstStateOf(step: FlowSpec['steps'][number]): string {
  return isParallel(step) ? `Parallel_${step.parallel.map((p) => p.job).join('_')}`.slice(0, 80) : `Run_${step.job}`;
}

export function buildDefaultAsl(flow: FlowSpec, commandPrefix: string[]): AslDocument {
  if (flow.steps.length === 0) throw new Error(`Flow ${flow.name} has no steps`);
  const states: Record<string, AslState> = {};
  const afterSteps = flow.waitSeconds ? 'Wait' : 'Done';
  const first = firstStateOf(flow.steps[0]);

  states.Init = {
    Type: 'Pass',
    Comment: 'Resolve runId, businessDate and force from the input or the triggering execution output (restart = same runId)',
    Assign: { execInput: '{% $states.input %}', businessDate: BUSINESS_DATE, runId: RUN_ID, force: FORCE },
    Next: flow.gate ? 'Gate' : first,
  };
  if (flow.gate) {
    states.Gate = { Type: 'Choice', Choices: [{ Condition: GATE_CONDITIONS[flow.gate], Next: first }], Default: 'Skipped' };
    states.Skipped = {
      Type: 'Pass',
      Output: { flow: flow.name, skipped: true, businessDate: '{% $businessDate %}', runId: '{% $runId %}', force: '{% $force %}' },
      End: true,
    };
  }

  flow.steps.forEach((step, i) => {
    const next = i + 1 < flow.steps.length ? firstStateOf(flow.steps[i + 1]) : afterSteps;
    if (isParallel(step)) {
      states[firstStateOf(step)] = {
        Type: 'Parallel',
        Branches: step.parallel.map((p) => ({
          StartAt: `Run_${p.job}`,
          States: {
            ...jobStates(flow, p, commandPrefix, `Done_${p.job}`),
            [`Done_${p.job}`]: { Type: 'Succeed' },
          },
        })),
        Next: next,
      };
    } else {
      Object.assign(states, jobStates(flow, step, commandPrefix, next));
    }
  });

  if (flow.waitSeconds) {
    states.Wait = { Type: 'Wait', Comment: 'Legacy WAITSTEP (COBSWAIT/MVSWAIT)', Seconds: flow.waitSeconds, Next: 'Done' };
  }
  states.Done = {
    Type: 'Pass',
    Output: { flow: flow.name, skipped: false, businessDate: '{% $businessDate %}', runId: '{% $runId %}', force: '{% $force %}' },
    End: true,
  };

  return {
    Comment: `carddemo-${flow.name}: ${flow.description}`,
    QueryLanguage: 'JSONata',
    TimeoutSeconds: 6 * 3600,
    StartAt: 'Init',
    States: states,
  };
}

/** Returns `${Name}` placeholders used in an ASL document text. */
export function findPlaceholders(text: string): string[] {
  return [...new Set([...text.matchAll(/\$\{([A-Za-z0-9_]+)\}/g)].map((m) => m[1]))].sort();
}

/** Structural check: StartAt and every Next/Default target exist in the same States map (recursive). */
export function validateAsl(doc: AslDocument): string[] {
  const errors: string[] = [];
  const check = (scope: { StartAt: string; States: Record<string, AslState> }, path: string) => {
    const names = new Set(Object.keys(scope.States));
    if (!names.has(scope.StartAt)) errors.push(`${path}: StartAt ${scope.StartAt} missing`);
    for (const [name, st] of Object.entries(scope.States)) {
      const targets: unknown[] = [st.Next, st.Default];
      if (Array.isArray(st.Choices)) targets.push(...st.Choices.map((c: { Next?: unknown }) => c.Next));
      if (Array.isArray(st.Catch)) targets.push(...st.Catch.map((c: { Next?: unknown }) => c.Next));
      for (const t of targets) {
        if (typeof t === 'string' && !names.has(t)) errors.push(`${path}/${name}: target ${t} missing`);
      }
      const terminal = st.End === true || ['Succeed', 'Fail', 'Choice'].includes(st.Type);
      if (!terminal && typeof st.Next !== 'string') errors.push(`${path}/${name}: no Next/End`);
      if (Array.isArray(st.Branches)) {
        st.Branches.forEach((b: AslDocument, i: number) => check(b, `${path}/${name}[${i}]`));
      }
    }
  };
  check(doc, 'root');
  return errors;
}
