import { mkdtempSync, readFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { describe, expect, it } from 'vitest';
import { AccountRecordLayout, CardXrefRecordLayout } from '../../common/copybooks/account.js';
import { TranCatBalRecordLayout, TranRecordLayout } from '../../common/copybooks/transaction.js';
import { loadSeedCatalog } from '../../common/seed/index.js';
import { computeMonthlyInterest, db2FormatTimestamp, runCbact04c } from './index.js';

const fixedNow = () => new Date(2022, 6, 18, 9, 5, 7, 129);

function catalogWith(tcatbal: [string, string, string, string][]) {
  const { catalog } = loadSeedCatalog({ emptyClusters: ['TCATBALF'] });
  catalog.require('TCATBALF').load(
    tcatbal.map(([acct, type, cat, bal]) =>
      TranCatBalRecordLayout.serialize({ tranCatKey: { trancatAcctId: acct, trancatTypeCd: type, trancatCd: cat }, tranCatBal: bal }),
    ),
  );
  return catalog;
}
const account = (c: ReturnType<typeof catalogWith>, id: number) =>
  AccountRecordLayout.parse(c.require('ACCTFILE').read(String(id).padStart(11, '0'))!);

describe('computeMonthlyInterest: (TRAN-CAT-BAL * DIS-INT-RATE) / 1200 into S9(09)V99, truncated', () => {
  it.each([
    ['1234.56', '15.00', '15.43'],
    ['-500.01', '25.00', '-10.41'],
    ['0.07', '17.99', '0.00'],
    ['333.33', '-3.33', '-0.92'],
    ['999999999.99', '25.00', '20833333.33'],
    ['999999999.99', '9999.99', '333324999.91'],
    ['-12345.67', '9999.99', '-102880.48'],
  ])('%s * %s / 1200 = %s', (bal, rate, expected) => expect(computeMonthlyInterest(bal, rate).toFixed(2)).toBe(expected));
});

describe('CBACT04C', () => {
  it('formats CURRENT-DATE as a DB2 timestamp', () => expect(db2FormatTimestamp(fixedNow())).toBe('2022-07-18-09.05.07.120000'));

  it('seed data: DEFAULT group fallback for every category, zero-amount interest transactions', () => {
    const r = runCbact04c({ now: fixedNow });
    expect(r.returnCode).toBe(0);
    expect(r.recordCount).toBe(50);
    expect(r.display.filter((l) => l === 'TRY WITH DEFAULT GROUP CODE')).toHaveLength(50);
    expect(r.transactRecords).toHaveLength(50);
  });

  it('writes interest transactions and rewrites the previous account on account change', () => {
    const catalog = catalogWith([
      ['1', '01', '1', '1234.56'],
      ['1', '01', '2', '-500.01'],
      ['1', '02', '1', '777.77'],
      ['2', '01', '1', '100.00'],
    ]);
    const before2 = account(catalog, 2);
    const dir = mkdtempSync(join(tmpdir(), 'cbact04c-'));
    const r = runCbact04c({ catalog, now: fixedNow, parmDate: '2022071800', transactFile: join(dir, 'tx') });
    expect(r.transactRecords).toHaveLength(3); // type 02 cat 1 has a 0.00 DEFAULT rate
    expect(readFileSync(join(dir, 'tx')).length).toBe(3 * 350);
    const t = TranRecordLayout.parse(r.transactRecords[0]!);
    expect(t.tranId).toBe('2022071800000001');
    expect([t.tranTypeCd, t.tranCatCd.toString(), t.tranSource.trimEnd()]).toEqual(['01', '5', 'System']);
    expect(t.tranDesc.trimEnd()).toBe('Int. for a/c 00000000001');
    expect(t.tranAmt.toFixed(2)).toBe('15.43');
    expect(t.tranMerchantId.isZero()).toBe(true);
    const xref1 = catalog.require('XREFFILE').records().map((b) => CardXrefRecordLayout.parse(b)).find((x) => x.xrefAcctId.eq(1));
    expect(t.tranCardNum).toBe(xref1!.xrefCardNum);
    expect(t.tranOrigTs).toBe('2022-07-18-09.05.07.120000');
    expect(t.tranProcTs).toBe(t.tranOrigTs);
    expect(TranRecordLayout.parse(r.transactRecords[1]!).tranAmt.toFixed(2)).toBe('-10.41');
    const a1 = account(catalog, 1);
    expect(a1.acctCurrBal.toFixed(2)).toBe('199.02'); // 194.00 + 15.43 - 10.41
    expect([a1.acctCurrCycCredit.toFixed(2), a1.acctCurrCycDebit.toFixed(2)]).toEqual(['0.00', '0.00']);
    // Last account: interest transaction written but the account is never rewritten.
    expect(TranRecordLayout.parse(r.transactRecords[2]!).tranDesc.trimEnd()).toBe('Int. for a/c 00000000002');
    expect(account(catalog, 2)).toEqual(before2);
  });

  it.runIf(process.platform === 'linux')('out of space on TRANSACT: write status 34, abend 999', () => {
    const r = runCbact04c({ catalog: catalogWith([['1', '01', '1', '1234.56']]), transactFile: '/dev/full' });
    expect(r.abended).toBe(true);
    expect(r.abendCode).toBe(999);
    expect(r.display.slice(-3)).toEqual(['ERROR WRITING TRANSACTION RECORD', 'FILE STATUS IS: NNNN0034', 'ABENDING PROGRAM']);
  });

  it('account missing: ACCOUNT NOT FOUND, status 23, abend', () => {
    const r = runCbact04c({ catalog: catalogWith([['99', '01', '1', '1.00']]) });
    expect(r.abended).toBe(true);
    expect(r.display.slice(2)).toEqual(['ACCOUNT NOT FOUND: 00000000099', 'ERROR READING ACCOUNT FILE', 'FILE STATUS IS: NNNN0023', 'ABENDING PROGRAM']);
  });

  it('several cards for one account: alternate-key READ returns 02 and the program abends (IBM VSAM)', () => {
    const catalog = catalogWith([['1', '01', '1', '1.00']]);
    catalog.require('XREFFILE').insert(CardXrefRecordLayout.serialize({ xrefCardNum: '9999999999999999', xrefCustId: 1, xrefAcctId: 1 }));
    const r = runCbact04c({ catalog });
    expect(r.abended).toBe(true);
    expect(r.display.slice(-3)).toEqual(['ERROR READING XREF FILE', 'FILE STATUS IS: NNNN0002', 'ABENDING PROGRAM']);
  });

  it('DEFAULT group record missing too: abend', () => {
    const catalog = catalogWith([['1', '01', '1', '1.00']]);
    catalog.require('DISCGRP').delete('DEFAULT   010001');
    const r = runCbact04c({ catalog });
    expect(r.abended).toBe(true);
    expect(r.display.slice(-5)).toEqual([
      'DISCLOSURE GROUP RECORD MISSING',
      'TRY WITH DEFAULT GROUP CODE',
      'ERROR READING DEFAULT DISCLOSURE GROUP',
      'FILE STATUS IS: NNNN0023',
      'ABENDING PROGRAM',
    ]);
  });
});
