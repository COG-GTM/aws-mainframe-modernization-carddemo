import { join } from 'node:path';
import { describe, expect, it } from 'vitest';
import { db2FormatTimestamp, displaySigned, ioStatus04, truncateTo } from '../../../src/batch/cbtrn-common/index.js';
import { cbtrn02c, REJECT_RECORD_LENGTH } from '../../../src/batch/cbtrn02c/index.js';
import { Decimal } from '../../../src/common/codecs/index.js';
import { AccountRecordLayout, TranCatBalRecordLayout, TranRecordLayout } from '../../../src/common/copybooks/index.js';
import { readDailyTransactions } from '../../../src/common/seed/index.js';
import { dailytranPath, golden, readLines, seedCatalog, tempDir, unload, writeLines } from '../cbtrn/helpers.js';

const NOW = new Date(2026, 0, 2, 3, 4, 5, 67);

function account(catalog: ReturnType<typeof seedCatalog>, id: string) {
  return AccountRecordLayout.parse(unload(catalog, 'ACCTFILE').find((r) => r.startsWith(id))!);
}

describe('CBTRN02C on dailytran.txt', () => {
  const dir = tempDir();
  const catalog = seedCatalog();
  const res = cbtrn02c({ catalog, dalytran: dailytranPath(), dalyrejs: join(dir, 'rejs.txt'), now: () => NOW });

  it('300 processed, 38 rejected (all 102 overlimit), RC 4', () => {
    expect(res).toMatchObject({ transactionCount: 300, rejectCount: 38, returnCode: 4, abended: false });
    expect(res.sysout.slice(-3)).toEqual([
      'TRANSACTIONS PROCESSED :000000300',
      'TRANSACTIONS REJECTED  :000000038',
      'END OF EXECUTION OF PROGRAM CBTRN02C',
    ]);
    const rejects = readLines(join(dir, 'rejs.txt'));
    expect(rejects).toHaveLength(38);
    for (const r of rejects) {
      expect(r).toHaveLength(REJECT_RECORD_LENGTH);
      expect(r.slice(350)).toBe('0102' + 'OVERLIMIT TRANSACTION'.padEnd(76));
    }
  });

  it('posts 262 transactions stamped with CURRENT-DATE', () => {
    const posted = unload(catalog, 'TRANFILE').map((r) => TranRecordLayout.parse(r));
    expect(posted).toHaveLength(262);
    expect(new Set(posted.map((t) => t.tranProcTs))).toEqual(new Set(['2026-01-02-03.04.05.060000']));
  });
});

describe('CBTRN02C posting rules', () => {
  const run = (recs: Buffer[], drop: string[] = []) => {
    const dir = tempDir();
    const catalog = seedCatalog(drop);
    const res = cbtrn02c({ catalog, dalytran: writeLines(join(dir, 'd.txt'), recs), dalyrejs: join(dir, 'r.txt'), now: () => NOW });
    return { res, catalog, rejects: readLines(join(dir, 'r.txt')) };
  };
  const rejectedIds = new Set(golden('cbtrn02c-sample', 'dalyrejs.txt').map((r) => r.slice(0, 16)));
  const valid = readDailyTransactions().filter((r) => !rejectedIds.has(r.toString('latin1', 0, 16)));

  it('RC 0 when nothing is rejected; credit and balance updated', () => {
    const rec = valid[0]!;
    const before = account(seedCatalog(), '00000000007');
    const { res, catalog, rejects } = run([rec]);
    expect(res).toMatchObject({ returnCode: 0, transactionCount: 1, rejectCount: 0 });
    expect(rejects).toEqual([]);
    const after = account(catalog, '00000000007');
    expect(after.acctCurrBal.minus(before.acctCurrBal).toFixed(2)).toBe('504.77');
    expect(after.acctCurrCycCredit.minus(before.acctCurrCycCredit).toFixed(2)).toBe('504.77');
    expect(after.acctCurrCycDebit.equals(before.acctCurrCycDebit)).toBe(true);
  });

  it('negative amounts go to ACCT-CURR-CYC-DEBIT', () => {
    const rec = Buffer.from(valid[0]!);
    rec.write('0000001234J', 132, 'latin1');
    const before = account(seedCatalog(), '00000000007');
    const { catalog } = run([rec]);
    const after = account(catalog, '00000000007');
    expect(after.acctCurrCycDebit.minus(before.acctCurrCycDebit).toFixed(2)).toBe('-123.41');
    expect(after.acctCurrCycCredit.equals(before.acctCurrCycCredit)).toBe(true);
  });

  it('TCATBAL: creates a missing key, then adds to it', () => {
    const a = Buffer.from(valid[0]!);
    a.write('077777', 16, 'latin1');
    const b = Buffer.from(a);
    b.write('X000000000000001', 0, 'latin1');
    const { res, catalog } = run([a, b]);
    expect(res.sysout.filter((l) => l.startsWith('TCATBAL'))).toEqual(['TCATBAL record not found for key : 00000000007077777.. Creating.']);
    const rec = unload(catalog, 'TCATBALF').find((r) => r.startsWith('00000000007077777'))!;
    expect(TranCatBalRecordLayout.parse(rec).tranCatBal.toFixed(2)).toBe('1009.54');
  });

  it('reject codes 100, 101, 102 and 103 (103 overrides 102)', () => {
    const recs = [0, 1, 2, 3].map((i) => Buffer.from(valid[i]!));
    recs[0]!.write('9999999999999999', 262, 'latin1');
    recs[2]!.write('9999999999I', 132, 'latin1');
    recs[3]!.write('9999999999I', 132, 'latin1');
    recs[3]!.write('2099-01-01', 278, 'latin1');
    const acct1 = valid[1]!.toString('latin1', 262, 278);
    const xref = unload(seedCatalog(), 'XREFFILE').find((r) => r.startsWith(acct1))!;
    const { res, rejects } = run(recs, [xref.slice(25, 36)]);
    expect(res).toMatchObject({ rejectCount: 4, returnCode: 4 });
    expect(rejects.map((r) => r.slice(350, 354))).toEqual(['0100', '0101', '0102', '0103']);
  });

  it('duplicate TRAN-ID: WRITE status 22 abends after TCATBAL/ACCOUNT were updated', () => {
    const { res, catalog } = run([valid[0]!, valid[0]!]);
    expect(res).toMatchObject({ returnCode: 999, abended: true });
    expect(res.sysout.slice(-3)).toEqual(['ERROR WRITING TO TRANSACTION FILE', 'FILE STATUS IS: NNNN0022', 'ABENDING PROGRAM']);
    const before = account(seedCatalog(), '00000000007');
    expect(account(catalog, '00000000007').acctCurrBal.minus(before.acctCurrBal).toFixed(2)).toBe('1009.54');
  });

  it('missing DALYTRAN abends at OPEN', () => {
    const res = cbtrn02c({ catalog: seedCatalog(), dalytran: join(tempDir(), 'none'), dalyrejs: join(tempDir(), 'r') });
    expect(res.sysout.slice(1)).toEqual(['ERROR OPENING DALYTRAN', 'FILE STATUS IS: NNNN0035', 'ABENDING PROGRAM']);
    expect(res.returnCode).toBe(999);
  });
});

describe('cbtrn-common helpers', () => {
  it('WS-TEMP-BAL S9(09)V99 truncates high-order digits', () => {
    expect(truncateTo('1000999999.99', 9, 2).toFixed(2)).toBe('999999.99');
    expect(truncateTo('-12.345', 9, 2).toFixed(2)).toBe('-12.34');
    expect(truncateTo(new Decimal('-1000000000.00'), 9, 2).isZero()).toBe(true);
  });
  it('9910-DISPLAY-IO-STATUS formatting', () => {
    expect(ioStatus04('35')).toBe('0035');
    expect(ioStatus04('9A')).toBe('9065');
    expect(ioStatus04('93')).toBe('9051');
  });
  it('DISPLAY of signed numerics and DB2 timestamps', () => {
    expect(displaySigned('765.66', 9, 2)).toBe('00000076566+');
    expect(displaySigned('-1.5', 9, 2)).toBe('00000000150-');
    expect(db2FormatTimestamp(NOW)).toBe('2026-01-02-03.04.05.060000');
  });
});
