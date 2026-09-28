import { aslSubstitutionKeys, buildDefaultAsl, findPlaceholders, validateAsl } from '../lib/catalog/asl';
import { flowJobs, flowsFor } from '../lib/catalog/flows';
import { JOBS, jobsFor } from '../lib/catalog/jobs';

const prefix = ['java', '-jar', 'carddemo-batch.jar'];

describe.each([false, true])('generated ASL (auth module %s)', (auth) => {
  const flows = flowsFor(auth);
  const jobNames = jobsFor(auth).map((j) => j.name);

  test.each(flows.map((f) => [f.name, f]))('%s is structurally valid', (_name, flow) => {
    const doc = buildDefaultAsl(flow, prefix);
    expect(validateAsl(doc)).toEqual([]);
    expect(doc.QueryLanguage).toBe('JSONata');
    const text = JSON.stringify(doc);
    for (const p of findPlaceholders(text)) expect(aslSubstitutionKeys(jobNames)).toContain(p);
    for (const job of flowJobs(flow)) expect(jobNames).toContain(job);
  });
});

test('daily cycle: purge only with auth module, then post, RC check, 36 s wait', () => {
  const [daily] = flowsFor(true);
  const doc = buildDefaultAsl(daily, prefix);
  expect(Object.keys(doc.States)).toEqual(
    expect.arrayContaining(['Run_purge-expired-authorizations', 'Run_post-daily-transactions', 'CheckRc_post-daily-transactions', 'Wait']),
  );
  expect(doc.States.Wait.Seconds).toBe(36);
  const check = doc.States['CheckRc_post-daily-transactions'] as unknown as { Choices: { Condition: string; Next: string }[]; Default: string };
  expect(check.Choices.map((c) => c.Condition)).toEqual(['{% $rc_post_daily_transactions = 0 %}', '{% $rc_post_daily_transactions = 4 %}']);
  expect(check.Choices[1].Next).toBe('Warn_post-daily-transactions');
  expect(check.Default).toBe('Failed_post-daily-transactions');
  const run = doc.States['Run_post-daily-transactions'] as unknown as { Arguments: { ContainerOverrides: { Command: string } } };
  expect(run.Arguments.ContainerOverrides.Command).toContain('"--job=post-daily-transactions"');

  const noAuth = buildDefaultAsl(flowsFor(false)[0], prefix);
  expect(Object.keys(noAuth.States)).not.toContain('Run_purge-expired-authorizations');
  expect(noAuth.States.Init.Next).toBe('Run_post-daily-transactions');
});

test('gated flows skip via Choice', () => {
  const monthly = flowsFor(false).find((f) => f.name === 'monthly-interest')!;
  const doc = buildDefaultAsl(monthly, prefix);
  expect(doc.States.Init.Next).toBe('Gate');
  expect(doc.States.Gate.Default).toBe('Skipped');
});

test('extracts flow runs jobs in parallel', () => {
  const extracts = flowsFor(false).find((f) => f.name === 'extracts')!;
  const doc = buildDefaultAsl(extracts, prefix);
  const par = Object.values(doc.States).find((s) => s.Type === 'Parallel') as unknown as { Branches: unknown[] };
  expect(par.Branches).toHaveLength(4);
});

test('validateAsl reports dangling targets', () => {
  expect(validateAsl({ StartAt: 'A', States: { A: { Type: 'Pass', Next: 'B' } } })).toEqual(['root/A: target B missing']);
});

test('job catalogue names are unique and auth job is optional', () => {
  expect(new Set(JOBS.map((j) => j.name)).size).toBe(JOBS.length);
  expect(jobsFor(false).map((j) => j.name)).not.toContain('purge-expired-authorizations');
  expect(jobsFor(true).map((j) => j.name)).toContain('purge-expired-authorizations');
});
