import { describe, expect, it } from 'vitest';
import { DFHRESP } from '../../../src/online/index.js';
import { buildJobLines, createCORPT00C, type JobSubmissionSink } from '../../../src/online/transaction/index.js';
import { errmsg, field, menuOption, trnRuntime, value } from './helpers.js';

const jobs = (rt: ReturnType<typeof trnRuntime>['rt']): string[] => (rt.tdQueues.get('JOBS') ?? []).map((b) => b.toString('latin1'));

function reports(opts: Parameters<typeof trnRuntime>[0] = {}) {
  const t = trnRuntime(opts);
  const first = menuOption(t.rt, '9');
  return { ...t, first };
}

describe('CORPT00C transaction reports', () => {
  it('menu option 9 shows the report screen', () => {
    const { first } = reports();
    expect(first.programs).toEqual(['COMEN01C', 'CORPT00C']);
    expect(first.screen.map).toBe('CORPT0A');
    expect(first.nextTransid).toBe('CR00');
    expect(first.commarea).toHaveLength(160);
    expect(first.screen.cursor).toEqual({ row: 7, col: 10 });
  });

  it('monthly report: asks to confirm, then submits the TRNRPT00 JCL to TDQ JOBS', () => {
    const { rt } = reports();
    const ask = rt.sendInput('T1', { aid: 'ENTER', fields: { MONTHLY: 'X' } });
    expect(errmsg(ask)).toBe('Please confirm to print the Monthly report...');
    expect(jobs(rt)).toEqual([]);
    const r = rt.sendInput('T1', { aid: 'ENTER', fields: { MONTHLY: 'X', CONFIRM: 'Y' } });
    expect(errmsg(r)).toBe('Monthly report submitted for printing ...');
    expect(field(r, 'ERRMSG').color).toBe('GREEN');
    expect(value(r, 'MONTHLY')).toBe('');
    const lines = jobs(rt);
    expect(lines).toHaveLength(17);
    expect(lines.every((l) => l.length === 80)).toBe(true);
    expect(lines).toEqual(buildJobLines('2026-10-01', '2026-10-31'));
    expect(lines[0]!.trimEnd()).toBe("//TRNRPT00 JOB 'TRAN REPORT',CLASS=A,MSGCLASS=0,");
    expect(lines[10]!.trimEnd()).toBe("PARM-START-DATE,C'2026-10-01'");
    expect(lines[11]!.trimEnd()).toBe("PARM-END-DATE,C'2026-10-31'");
    expect(lines[14]!.trimEnd()).toBe('2026-10-01 2026-10-31');
    expect(lines[16]!.trimEnd()).toBe('/*EOF');
  });

  it('monthly end date handles February of a leap year and December', () => {
    const feb = reports({ clock: () => new Date(2028, 1, 10, 9, 0, 0) });
    feb.rt.sendInput('T1', { aid: 'ENTER', fields: { MONTHLY: 'X', CONFIRM: 'Y' } });
    expect(jobs(feb.rt)[14]!.trimEnd()).toBe('2028-02-01 2028-02-29');
    const dec = reports({ clock: () => new Date(2026, 11, 31, 9, 0, 0) });
    dec.rt.sendInput('T1', { aid: 'ENTER', fields: { MONTHLY: 'X', CONFIRM: 'y' } });
    expect(jobs(dec.rt)[14]!.trimEnd()).toBe('2026-12-01 2026-12-31');
  });

  it('yearly report', () => {
    const { rt } = reports();
    const r = rt.sendInput('T1', { aid: 'ENTER', fields: { YEARLY: 'X', CONFIRM: 'Y' } });
    expect(errmsg(r)).toBe('Yearly report submitted for printing ...');
    expect(jobs(rt)[14]!.trimEnd()).toBe('2026-01-01 2026-12-31');
  });

  it('custom report: NUMVAL-C normalises the date parts', () => {
    const { rt } = reports();
    const r = rt.sendInput('T1', {
      aid: 'ENTER',
      fields: { CUSTOM: 'X', SDTMM: '1', SDTDD: '15', SDTYYYY: '2022', EDTMM: '12', EDTDD: '31', EDTYYYY: '2022', CONFIRM: 'Y' },
    });
    expect(errmsg(r)).toBe('Custom report submitted for printing ...');
    expect(jobs(rt)[14]!.trimEnd()).toBe('2022-01-15 2022-12-31');
  });

  it('custom report validation', () => {
    const { rt } = reports();
    const dates = { CUSTOM: 'X', SDTMM: '01', SDTDD: '15', SDTYYYY: '2022', EDTMM: '12', EDTDD: '31', EDTYYYY: '2022', CONFIRM: 'Y' };
    const send = (fields: Record<string, string>) => rt.sendInput('T1', { aid: 'ENTER', fields: { ...dates, ...fields } });
    expect(errmsg(send({ SDTMM: '' }))).toBe('Start Date - Month can NOT be empty...');
    expect(errmsg(send({ EDTYYYY: '' }))).toBe('End Date - Year can NOT be empty...');
    const m = send({ SDTMM: '13' });
    expect(errmsg(m)).toBe('Start Date - Not a valid Month...');
    expect(value(m, 'SDTMM')).toBe('13');
    expect(errmsg(send({ EDTDD: '32' }))).toBe('End Date - Not a valid Day...');
    expect(errmsg(send({ SDTMM: '02', SDTDD: '30' }))).toBe('Start Date - Not a valid date...');
    expect(errmsg(send({ EDTMM: '00' }))).toBe('End Date - Not a valid date...');
    expect(jobs(rt)).toEqual([]);
  });

  it('no report type, N and invalid confirmation values', () => {
    const { rt } = reports();
    expect(errmsg(rt.sendInput('T1', { aid: 'ENTER', fields: { CONFIRM: 'Y' } }))).toBe('Select a report type to print report...');
    const n = rt.sendInput('T1', { aid: 'ENTER', fields: { YEARLY: 'X', CONFIRM: 'N' } });
    expect(errmsg(n)).toBe('');
    expect(value(n, 'YEARLY')).toBe('');
    expect(errmsg(rt.sendInput('T1', { aid: 'ENTER', fields: { YEARLY: 'X', CONFIRM: 'Q' } }))).toBe('"Q" is not a valid value to confirm...');
    expect(jobs(rt)).toEqual([]);
  });

  it('injected job-submission sink receives the records; a failing write is reported', () => {
    const seen: string[] = [];
    const capture: JobSubmissionSink = (_ctx, queue, record) => {
      seen.push(`${queue}:${record.trimEnd()}`);
      return { resp: DFHRESP.NORMAL, resp2: 0 };
    };
    const ok = reports({ replace: [createCORPT00C({ jobSink: capture })] });
    ok.rt.sendInput('T1', { aid: 'ENTER', fields: { YEARLY: 'X', CONFIRM: 'Y' } });
    expect(seen).toHaveLength(17);
    expect(seen[14]).toBe('JOBS:2026-01-01 2026-12-31');
    expect(jobs(ok.rt)).toEqual([]);

    let calls = 0;
    const failing: JobSubmissionSink = () => {
      calls++;
      return { resp: DFHRESP.QIDERR, resp2: 1 };
    };
    const bad = reports({ replace: [createCORPT00C({ jobSink: failing })] });
    const r = bad.rt.sendInput('T1', { aid: 'ENTER', fields: { MONTHLY: 'X', CONFIRM: 'Y' } });
    expect(errmsg(r)).toBe('Unable to Write TDQ (JOBS)...');
    expect(calls).toBe(1);
  });

  it('PF3 returns to the menu; other keys are invalid', () => {
    const { rt } = reports();
    expect(errmsg(rt.sendInput('T1', { aid: 'PF5' }))).toBe('Invalid key pressed. Please see below...');
    expect(rt.sendInput('T1', { aid: 'PF3' }).programs).toEqual(['CORPT00C', 'COMEN01C']);
  });
});
