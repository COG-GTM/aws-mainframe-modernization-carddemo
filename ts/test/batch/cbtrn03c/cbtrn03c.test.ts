import { join } from 'node:path';
import { describe, expect, it } from 'vitest';
import { cbtrn03c, REPORT_LINE_LENGTH } from '../../../src/batch/cbtrn03c/index.js';
import { TranRecordLayout } from '../../../src/common/copybooks/index.js';
import { loadSeedCatalog } from '../../../src/common/seed/index.js';
import { readLines, rtrim, tempDir, writeLines } from '../cbtrn/helpers.js';

const tran = (id: string, amt: string, opts: { card?: string; type?: string; ts?: string } = {}) =>
  TranRecordLayout.serialize({
    tranId: id,
    tranTypeCd: opts.type ?? '01',
    tranCatCd: 1,
    tranSource: 'POS TERM',
    tranAmt: amt,
    tranCardNum: opts.card ?? '4859452612877065',
    tranProcTs: opts.ts ?? '2022-06-10-10.00.00.000000',
  });

function run(trans: Buffer[], parm: string[] = ['2022-01-01 2022-12-31']) {
  const dir = tempDir();
  const res = cbtrn03c({
    catalog: loadSeedCatalog().catalog,
    tranfile: writeLines(join(dir, 't.txt'), trans),
    dateparm: writeLines(join(dir, 'p.txt'), parm),
    tranrept: join(dir, 'r.txt'),
  });
  return { res, report: readLines(join(dir, 'r.txt')) };
}

describe('CBTRN03C', () => {
  it('end of file adds the last amount twice and skips the last account total (bug-for-bug)', () => {
    const { res, report } = run([tran('T1', '10.00'), tran('T2', '5.00')]);
    expect(res.returnCode).toBe(0);
    expect(report.every((l) => l.length === REPORT_LINE_LENGTH)).toBe(true);
    const r = rtrim(report);
    expect(r[0]).toBe('DALYREPT                              Daily Transaction Report                 Date Range: 2022-01-01 to 2022-12-31');
    expect(r.slice(4)).toEqual([
      'T1               00000000007 01-Purchase        0001-Regular Sales Draft           POS TERM                10.00',
      'T2               00000000007 01-Purchase        0001-Regular Sales Draft           POS TERM                 5.00',
      'Page Total ' + '.'.repeat(86) + '+         20.00',
      '-'.repeat(133),
      'Grand Total' + '.'.repeat(86) + '+         20.00',
    ]);
    expect(res.sysout.slice(-3)).toEqual(['TRAN-AMT 00000000500+', 'WS-PAGE-TOTAL00000001500+', 'END OF EXECUTION OF PROGRAM CBTRN03C']);
  });

  it('account totals on card change and page break every 20 lines', () => {
    const trans = Array.from({ length: 20 }, (_, i) => tran(`A${i}`, '1.00'));
    trans.push(tran('B0', '-2.50', { card: '0927987108636232' }));
    const r = rtrim(run(trans).report);
    expect(r.filter((l) => l.startsWith('DALYREPT'))).toHaveLength(2);
    expect(r[20]).toMatch(/^Page Total \.+\+ +16\.00$/);
    expect(r.find((l) => l.startsWith('Account Total'))).toMatch(/\+ +20\.00$/);
    expect(r.at(-1)).toMatch(/^Grand Total\.+\+ +15\.00$/);
    expect(r).toContain('B0               00000000020 01-Purchase        0001-Regular Sales Draft           POS TERM      -          2.50');
  });

  it('first out-of-range record ends the report without totals (NEXT SENTENCE)', () => {
    const { report } = run([tran('T1', '1.00'), tran('T2', '2.00', { ts: '2023-01-01-00.00.00.000000' }), tran('T3', '3.00')]);
    expect(rtrim(report).slice(4)).toEqual([
      'T1               00000000007 01-Purchase        0001-Regular Sales Draft           POS TERM                 1.00',
    ]);
  });

  it('unknown transaction type abends with status 23', () => {
    const { res, report } = run([tran('T1', '1.00', { type: '99' })]);
    expect(res).toMatchObject({ returnCode: 999, abended: true });
    expect(res.sysout.slice(-3)).toEqual(['INVALID TRANSACTION TYPE : 99', 'FILE STATUS IS: NNNN0023', 'ABENDING PROGRAM']);
    expect(report).toEqual([]);
  });

  it('empty DATEPARM: nothing is reported', () => {
    const { res, report } = run([tran('T1', '1.00')], []);
    expect(res.sysout).toEqual(['START OF EXECUTION OF PROGRAM CBTRN03C', 'END OF EXECUTION OF PROGRAM CBTRN03C']);
    expect(report).toEqual([]);
  });
});
