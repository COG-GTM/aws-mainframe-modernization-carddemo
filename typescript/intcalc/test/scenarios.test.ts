import { existsSync, mkdtempSync, readFileSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { describe, expect, it } from 'vitest';

import { main } from '../src/cli.ts';
import { splitFixedWidth } from '../src/io.ts';
import {
  ACCOUNT_RECORD_LENGTH,
  TRAN_RECORD_LENGTH,
  formatAccount,
  formatCardXref,
  formatDisGroup,
  formatTranCatBal,
  parseAccount,
  parseTran,
  type AccountRecord,
  type CardXrefRecord,
  type DisGroupRecord,
  type TranCatBalRecord,
  type TranRecord,
} from '../src/records.ts';
import { accountRecord, balanceRecord, discgrpRecord, xrefRecord } from './helpers.ts';

/**
 * End-to-end scenarios over hand-built (mocked) fixed-width files. The
 * `app/data/ASCII` fixtures carry only zero balances, so these files exercise
 * every migrated rule with real money: rate lookup, the DEFAULT fallback, the
 * zero-rate skip, truncation toward zero, per-category accumulation, negative
 * balances, the account update, and defect D1.
 */

const PARM = '2022071800';

interface Scenario {
  exitCode: number;
  transactions: TranRecord[];
  accounts: Map<string, AccountRecord>;
  transactFile: string;
}

function runScenario(
  balances: readonly TranCatBalRecord[],
  accounts: readonly AccountRecord[],
  xrefs: readonly CardXrefRecord[],
  discgrps: readonly DisGroupRecord[],
  parm: string = PARM,
): Scenario {
  const dir = mkdtempSync(join(tmpdir(), 'intcalc-scenario-'));
  const write = (name: string, images: readonly string[]): string => {
    const path = join(dir, name);
    writeFileSync(path, images.map((image) => `${image}\n`).join(''), 'latin1');
    return path;
  };

  const accountFile = write('acct.txt', accounts.map(formatAccount));
  const transactFile = join(dir, 'systran.txt');

  const exitCode = main([
    '--parm',
    parm,
    '--tcatbal',
    write('tcatbal.txt', balances.map(formatTranCatBal)),
    '--xref',
    write('xref.txt', xrefs.map(formatCardXref)),
    '--discgrp',
    write('discgrp.txt', discgrps.map(formatDisGroup)),
    '--account',
    accountFile,
    '--transact-out',
    transactFile,
    '--quiet',
  ]);

  const updated = splitFixedWidth(
    readFileSync(accountFile, 'latin1'),
    ACCOUNT_RECORD_LENGTH,
    'ACCTFILE',
  ).map(parseAccount);

  return {
    exitCode,
    transactions: existsSync(transactFile)
      ? splitFixedWidth(
          readFileSync(transactFile, 'latin1'),
          TRAN_RECORD_LENGTH,
          'TRANSACT',
        ).map(parseTran)
      : [],
    accounts: new Map(updated.map((account) => [account.acctId, account])),
    transactFile,
  };
}

const GOLD = 'GOLD      ';
const ZEROGRP = 'ZEROGRP   ';

/**
 * Four accounts, each exercising a different rule:
 *   101 GOLD    two categories at 18 % on 33.33 → truncation + accumulation
 *   102 (blank) no group row → DEFAULT fallback at 15 %
 *   103 ZEROGRP zero rate → no interest, no transaction
 *   104 GOLD    negative balance, and last in the file → defect D1
 */
const BALANCES = [
  balanceRecord('101', '01', '0001', '33.33'),
  balanceRecord('101', '01', '0002', '33.33'),
  balanceRecord('102', '01', '0001', '500.00'),
  balanceRecord('103', '01', '0001', '777.77'),
  balanceRecord('104', '01', '0001', '-200.00'),
];

const ACCOUNTS = [
  accountRecord('101', GOLD, '1000.00'),
  accountRecord('102', ' '.repeat(10), '500.00'),
  accountRecord('103', ZEROGRP, '777.77'),
  accountRecord('104', GOLD, '250.00'),
];

const XREFS = [
  xrefRecord('101', '4111111111110101'),
  xrefRecord('102', '4111111111110102'),
  xrefRecord('103', '4111111111110103'),
  xrefRecord('104', '4111111111110104'),
];

const DISCGRPS = [
  discgrpRecord(GOLD, '01', '0001', '18.00'),
  discgrpRecord(GOLD, '01', '0002', '18.00'),
  discgrpRecord(ZEROGRP, '01', '0001', '0.00'),
  discgrpRecord('DEFAULT', '01', '0001', '15.00'),
];

describe('scenario: every interest rule over mocked data', () => {
  const run = runScenario(BALANCES, ACCOUNTS, XREFS, DISCGRPS);

  it('completes normally', () => {
    expect(run.exitCode).toBe(0);
  });

  it('writes a transaction per interest-bearing category and skips the zero rate', () => {
    // 5 balance records in, 4 transactions out: account 103's rate is 0.00.
    expect(run.transactions.map((tran) => tran.tranAmt.toFixed(2))).toEqual([
      '0.49',
      '0.49',
      '6.25',
      '-3.00',
    ]);
    expect(run.transactions.map((tran) => tran.tranDesc.trimEnd())).not.toContain(
      'Int. for a/c 00000000103',
    );
  });

  it('truncates toward zero per category rather than rounding half up', () => {
    // 33.33 x 18 / 1200 = 0.49995 -> 0.49 (HALF_UP would give 0.50).
    expect(run.transactions[0]?.tranAmt.toFixed(2)).toBe('0.49');
  });

  it('adds the sum of the truncated category amounts, not the truncation of the sum', () => {
    // 0.49 + 0.49 = 0.98, whereas truncating 0.9999 on the sum would give 0.99.
    expect(run.accounts.get('00000000101')?.currBal.toFixed(2)).toBe('1000.98');
  });

  it('falls back to the DEFAULT group when the account group has no row', () => {
    // 500.00 × 15 / 1200 = 6.25 from group DEFAULT, not GOLD's 18 %.
    expect(run.transactions[2]?.tranAmt.toFixed(2)).toBe('6.25');
    expect(run.accounts.get('00000000102')?.currBal.toFixed(2)).toBe('506.25');
  });

  it('still updates an account whose rate is zero', () => {
    const account = run.accounts.get('00000000103');
    expect(account?.currBal.toFixed(2)).toBe('777.77');
    expect(account?.currCycCredit.isZero()).toBe(true);
    expect(account?.currCycDebit.isZero()).toBe(true);
  });

  it('zeroes the current-cycle buckets of every updated account', () => {
    for (const acctId of ['00000000101', '00000000102', '00000000103']) {
      expect(run.accounts.get(acctId)?.currCycCredit.isZero()).toBe(true);
      expect(run.accounts.get(acctId)?.currCycDebit.isZero()).toBe(true);
    }
  });

  it('computes negative interest on a negative balance', () => {
    // -200.00 × 18 / 1200 = -3.00.
    expect(run.transactions[3]?.tranAmt.toFixed(2)).toBe('-3.00');
  });

  it('never updates the last account of the run (defect D1)', () => {
    const account = run.accounts.get('00000000104');
    // The -3.00 transaction was written, but the balance and the cycle buckets
    // are untouched: the COBOL's final 1050-UPDATE-ACCOUNT is unreachable.
    expect(account?.currBal.toFixed(2)).toBe('250.00');
    expect(account?.currCycCredit.toFixed(2)).toBe('11.11');
    expect(account?.currCycDebit.toFixed(2)).toBe('22.22');
  });

  it('composes TRAN-IDs from the PARM plus a six-digit run counter', () => {
    expect(run.transactions.map((tran) => tran.tranId)).toEqual([
      `${PARM}000001`,
      `${PARM}000002`,
      `${PARM}000003`,
      `${PARM}000004`,
    ]);
  });

  it('stamps the fixed transaction attributes and the cross-referenced card', () => {
    const tran = run.transactions[2];
    expect(tran?.tranTypeCd).toBe('01');
    expect(tran?.tranCatCd).toBe('0005');
    expect(tran?.tranSource.trimEnd()).toBe('System');
    expect(tran?.tranDesc.trimEnd()).toBe('Int. for a/c 00000000102');
    expect(tran?.cardNum).toBe('4111111111110102');
    expect(tran?.merchantId).toBe('000000000');
    expect(tran?.merchantName.trim()).toBe('');
    expect(tran?.origTs).toBe(tran?.procTs);
  });
});

describe('scenario: missing DEFAULT disclosure row', () => {
  const run = runScenario(
    [balanceRecord('101', '01', '0001', '33.33'), balanceRecord('105', '01', '0001', '100.00')],
    [...ACCOUNTS, accountRecord('105', 'NOSUCH    ', '100.00')],
    [...XREFS, xrefRecord('105', '4111111111110105')],
    // No DEFAULT row for 01/0001 this time, so account 105 cannot be priced.
    [discgrpRecord(GOLD, '01', '0001', '18.00')],
  );

  it('abends the run', () => {
    expect(run.exitCode).toBe(99);
  });

  it('discards the SYSTRAN generation the way DISP=(NEW,CATLG,DELETE) does', () => {
    expect(existsSync(run.transactFile)).toBe(false);
  });

  it('leaves account rewrites made before the abend committed (defect D6)', () => {
    expect(run.accounts.get('00000000101')?.currBal.toFixed(2)).toBe('1000.49');
  });
});

describe('scenario: the run-date PARM is not validated', () => {
  const run = runScenario(
    [balanceRecord('101', '01', '0001', '33.33'), balanceRecord('102', '01', '0001', '500.00')],
    ACCOUNTS,
    XREFS,
    DISCGRPS,
    'NOTADATE00',
  );

  it('accepts a non-date PARM and copies it straight into the TRAN-ID', () => {
    expect(run.exitCode).toBe(0);
    expect(run.transactions[0]?.tranId).toBe('NOTADATE00000001');
  });

  it('timestamps from the wall clock, not from the PARM', () => {
    const today = new Date();
    expect(run.transactions[0]?.origTs.slice(0, 4)).toBe(String(today.getFullYear()));
  });
});
