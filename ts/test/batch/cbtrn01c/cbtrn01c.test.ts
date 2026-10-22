import { join } from 'node:path';
import { describe, expect, it } from 'vitest';
import { cbtrn01c } from '../../../src/batch/cbtrn01c/index.js';
import { readDailyTransactions } from '../../../src/common/seed/index.js';
import { dailytranPath, seedCatalog, tempDir, writeLines } from '../cbtrn/helpers.js';

const TAIL = ['END OF EXECUTION OF PROGRAM CBTRN01C'];

describe('CBTRN01C', () => {
  it('displays each record and its lookups; RC 0, no output files', () => {
    const res = cbtrn01c({ catalog: seedCatalog(), dalytran: dailytranPath() });
    expect(res).toMatchObject({ returnCode: 0, abended: false });
    expect(res.sysout[0]).toBe('START OF EXECUTION OF PROGRAM CBTRN01C');
    expect(res.sysout.slice(1, 7)).toEqual([
      readDailyTransactions()[0]!.toString('latin1'),
      'SUCCESSFUL READ OF XREF',
      'CARD NUMBER: 4859452612877065',
      'ACCOUNT ID : 00000000007',
      'CUSTOMER ID: 000000007',
      'SUCCESSFUL READ OF ACCOUNT FILE',
    ]);
  });

  it('repeats the lookups for the last record after end of file (bug-for-bug)', () => {
    const res = cbtrn01c({ catalog: seedCatalog(), dalytran: dailytranPath() });
    expect(res.sysout.filter((l) => l === 'SUCCESSFUL READ OF XREF')).toHaveLength(301);
    expect(res.sysout).toHaveLength(1 + 300 * 6 + 5 + 1);
  });

  it('reports a missing account and an unknown card', () => {
    const dir = tempDir();
    const recs = readDailyTransactions().slice(0, 2);
    recs[1]!.write('9999999999999999', 262, 'latin1');
    const res = cbtrn01c({ catalog: seedCatalog(['00000000007']), dalytran: writeLines(join(dir, 'd.txt'), recs) });
    const id = recs[1]!.toString('latin1', 0, 16);
    expect(res.sysout.slice(1)).toEqual([
      recs[0]!.toString('latin1'),
      'SUCCESSFUL READ OF XREF',
      'CARD NUMBER: 4859452612877065',
      'ACCOUNT ID : 00000000007',
      'CUSTOMER ID: 000000007',
      'INVALID ACCOUNT NUMBER FOUND',
      'ACCOUNT 00000000007 NOT FOUND',
      recs[1]!.toString('latin1'),
      'INVALID CARD NUMBER FOR XREF',
      `CARD NUMBER 9999999999999999 COULD NOT BE VERIFIED. SKIPPING TRANSACTION ID-${id}`,
      'INVALID CARD NUMBER FOR XREF',
      `CARD NUMBER 9999999999999999 COULD NOT BE VERIFIED. SKIPPING TRANSACTION ID-${id}`,
      ...TAIL,
    ]);
    expect(res.returnCode).toBe(0);
  });

  it('empty DALYTRAN: one lookup with the initial (blank) record', () => {
    const dir = tempDir();
    const res = cbtrn01c({ catalog: seedCatalog(), dalytran: writeLines(join(dir, 'd.txt'), []) });
    const blank = ' '.repeat(16);
    expect(res.sysout).toEqual([
      'START OF EXECUTION OF PROGRAM CBTRN01C',
      'INVALID CARD NUMBER FOR XREF',
      `CARD NUMBER ${blank} COULD NOT BE VERIFIED. SKIPPING TRANSACTION ID-${blank}`,
      ...TAIL,
    ]);
  });

  it('missing DALYTRAN abends (CEE3ABD 999)', () => {
    const res = cbtrn01c({ catalog: seedCatalog(), dalytran: join(tempDir(), 'missing.txt') });
    expect(res).toMatchObject({ returnCode: 999, abended: true });
    expect(res.sysout.slice(1)).toEqual(['ERROR OPENING DAILY TRANSACTION FILE', 'FILE STATUS IS: NNNN0035', 'ABENDING PROGRAM']);
  });
});
