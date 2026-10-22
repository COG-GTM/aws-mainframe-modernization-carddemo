/**
 * Parity with the original COBOL run under GnuCOBOL 3.1: test/batch/cbtrn/golden/*
 * were produced by tools/cbtrn/gen-golden-cbtrn.sh (TRAN-PROC-TS masked).
 */
import { join } from 'node:path';
import { describe, expect, it } from 'vitest';
import { cbtrn01c } from '../../../src/batch/cbtrn01c/index.js';
import { cbtrn02c } from '../../../src/batch/cbtrn02c/index.js';
import { cbtrn03c, tranreptSortInput } from '../../../src/batch/cbtrn03c/index.js';
import { loadSeedCatalog } from '../../../src/common/seed/index.js';
import {
  dailytranPath,
  dupDalytran,
  edgeDalytran,
  golden,
  goldenRc,
  goldenSysout,
  maskProcTs,
  readLines,
  rtrim,
  seedCatalog,
  tempDir,
  unload,
  writeLines,
} from './helpers.js';

describe('CBTRN01C golden', () => {
  it('SYSOUT and RC match', () => {
    const res = cbtrn01c({ catalog: seedCatalog(), dalytran: dailytranPath() });
    expect(res.returnCode).toBe(goldenRc('cbtrn01c'));
    expect(rtrim(res.sysout)).toEqual(goldenSysout('cbtrn01c'));
  });
});

describe.each([
  ['sample', () => dailytranPath(), [] as string[]],
  ['edge', (dir: string) => writeLines(join(dir, 'dalytran.txt'), edgeDalytran()), ['00000000020']],
  ['dup', (dir: string) => writeLines(join(dir, 'dalytran.txt'), dupDalytran()), []],
])('CBTRN02C golden: %s', (name, dalytran, drop) => {
  const dir = tempDir();
  const catalog = seedCatalog(drop);
  const res = cbtrn02c({ catalog, dalytran: dalytran(dir), dalyrejs: join(dir, 'dalyrejs.txt') });
  const g = `cbtrn02c-${name}`;

  it('RC and SYSOUT', () => {
    expect(res.returnCode & 0xff).toBe(goldenRc(g));
    expect(rtrim(res.sysout)).toEqual(goldenSysout(g));
  });
  it('posted TRANSACT (TRAN-PROC-TS masked)', () => {
    expect(unload(catalog, 'TRANFILE').map(maskProcTs)).toEqual(golden(g, 'transact.txt'));
  });
  it('ACCTDATA balances', () => {
    expect(unload(catalog, 'ACCTFILE')).toEqual(golden(g, 'acctdata.txt'));
  });
  it('TCATBAL create-or-update', () => {
    expect(unload(catalog, 'TCATBALF')).toEqual(golden(g, 'tcatbal.txt'));
  });
  it('DALYREJS', () => {
    expect(readLines(join(dir, 'dalyrejs.txt'))).toEqual(golden(g, 'dalyrejs.txt'));
  });
});

/** seqdata.py proc_ts: deterministic TRAN-PROC-TS stamped on the posted sample. */
function stamp(records: string[], variant: 'all' | 'mixed'): Buffer[] {
  return records.map((r, i) => {
    const ts =
      variant === 'mixed'
        ? i % 7 === 6
          ? '2022-08-01-10.00.00.000000'
          : `2022-06-${String(1 + (i % 28)).padStart(2, '0')}-10.00.00.000000`
        : '2022-06-10-10.00.00.000000';
    return Buffer.from(r.slice(0, 304) + ts + r.slice(330), 'latin1');
  });
}

describe.each([
  ['sorted', '2022-01-01', '2022-07-06'],
  ['unfiltered', '2022-06-01', '2022-06-30'],
  ['empty', '2023-01-01', '2023-12-31'],
])('CBTRN03C golden: %s', (name, start, end) => {
  const g = `cbtrn03c-${name}`;
  it('report and SYSOUT match', () => {
    const dir = tempDir();
    const { catalog } = loadSeedCatalog();
    const res = cbtrn03c({
      catalog,
      tranfile: writeLines(join(dir, 'tranfile.txt'), golden(g, 'tranfile.txt')),
      dateparm: writeLines(join(dir, 'dateparm.txt'), [`${start} ${end}`]),
      tranrept: join(dir, 'tranrept.txt'),
    });
    expect(res.returnCode).toBe(goldenRc(g));
    expect(rtrim(res.sysout)).toEqual(goldenSysout(g));
    expect(readLines(join(dir, 'tranrept.txt'))).toEqual(golden(g, 'tranrept.txt'));
  });
});

describe('TRANREPT STEP05R sort/filter', () => {
  it('reproduces the sorted CBTRN03C input from the posted sample', () => {
    const posted = stamp(golden('cbtrn02c-sample', 'transact.txt'), 'all');
    const sorted = tranreptSortInput(posted, '2022-01-01', '2022-07-06').map((b) => b.toString('latin1'));
    expect(sorted).toEqual(golden('cbtrn03c-sorted', 'tranfile.txt'));
    expect(tranreptSortInput(posted, '2023-01-01', '2023-12-31')).toEqual([]);
  });
});
