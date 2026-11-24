import { copyFileSync, mkdtempSync, readFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { describe, expect, it } from 'vitest';

import { main } from '../src/cli.ts';
import { splitFixedWidth } from '../src/io.ts';
import {
  ACCOUNT_RECORD_LENGTH,
  TRAN_RECORD_LENGTH,
  parseAccount,
  parseTran,
} from '../src/records.ts';
import { ASCII_DATA_DIR } from './fixtures.ts';

const PARM = '2022071800';

interface RunOutput {
  exitCode: number;
  transactions: string[];
  accountsBefore: string[];
  accountsAfter: string[];
}

function runBatch(): RunOutput {
  const workDir = mkdtempSync(join(tmpdir(), 'intcalc-e2e-'));
  const accountFile = join(workDir, 'acctdata.txt');
  copyFileSync(join(ASCII_DATA_DIR, 'acctdata.txt'), accountFile);
  const transactFile = join(workDir, 'systran.txt');

  const exitCode = main([
    '--parm',
    PARM,
    '--tcatbal',
    join(ASCII_DATA_DIR, 'tcatbal.txt'),
    '--xref',
    join(ASCII_DATA_DIR, 'cardxref.txt'),
    '--discgrp',
    join(ASCII_DATA_DIR, 'discgrp.txt'),
    '--account',
    accountFile,
    '--transact-out',
    transactFile,
    '--quiet',
  ]);

  return {
    exitCode,
    transactions: splitFixedWidth(
      readFileSync(transactFile, 'latin1'),
      TRAN_RECORD_LENGTH,
      'TRANSACT',
    ),
    accountsBefore: splitFixedWidth(
      readFileSync(join(ASCII_DATA_DIR, 'acctdata.txt'), 'latin1'),
      ACCOUNT_RECORD_LENGTH,
      'ACCTFILE',
    ),
    accountsAfter: splitFixedWidth(
      readFileSync(accountFile, 'latin1'),
      ACCOUNT_RECORD_LENGTH,
      'ACCTFILE',
    ),
  };
}

describe('end-to-end run over the app/data/ASCII fixtures', () => {
  const run = runBatch();

  it('completes without abending', () => {
    expect(run.exitCode).toBe(0);
  });

  it('writes one 350-byte transaction per category balance record', () => {
    // Every account in acctdata.txt carries a blank ACCT-GROUP-ID, so each of
    // the 50 balance records falls back to the DEFAULT 01/0001 row (1.50 %).
    expect(run.transactions).toHaveLength(50);
    for (const image of run.transactions) {
      expect(image).toHaveLength(TRAN_RECORD_LENGTH);
    }
  });

  it('generates sequential transaction ids from the PARM', () => {
    const ids = run.transactions.map((image) => parseTran(image).tranId);
    expect(ids[0]).toBe(`${PARM}000001`);
    expect(ids.at(-1)).toBe(`${PARM}000050`);
  });

  it('carries the account id, card number and classification on each transaction', () => {
    const first = parseTran(run.transactions[0] ?? '');
    expect(first.tranTypeCd).toBe('01');
    expect(first.tranCatCd).toBe('0005');
    expect(first.tranSource.trimEnd()).toBe('System');
    expect(first.tranDesc.trimEnd()).toBe('Int. for a/c 00000000001');
    expect(first.cardNum.trim()).not.toBe('');
    expect(first.origTs).toMatch(/^\d{4}-\d{2}-\d{2}-\d{2}\.\d{2}\.\d{2}\.\d{2}0000$/);
    expect(first.procTs).toBe(first.origTs);
  });

  it('accrues zero interest because every fixture balance is zero', () => {
    for (const image of run.transactions) {
      expect(parseTran(image).tranAmt.isZero()).toBe(true);
    }
  });

  it('rewrites the account file with the same record count and length', () => {
    expect(run.accountsAfter).toHaveLength(run.accountsBefore.length);
    for (const image of run.accountsAfter) {
      expect(image).toHaveLength(ACCOUNT_RECORD_LENGTH);
    }
  });

  it('zeroes the cycle buckets of every updated account and keeps balances unchanged', () => {
    for (const [index, image] of run.accountsAfter.entries()) {
      const before = parseAccount(run.accountsBefore[index] ?? '');
      const after = parseAccount(image);
      expect(after.acctId).toBe(before.acctId);
      expect(after.currBal.toFixed(2)).toBe(before.currBal.toFixed(2));
      expect(after.currCycCredit.isZero()).toBe(true);
      expect(after.currCycDebit.isZero()).toBe(true);
    }
  });

  it('leaves the last account of the run untouched (defect D1)', () => {
    const lastBefore = run.accountsBefore.at(-1) ?? '';
    const lastAfter = run.accountsAfter.at(-1) ?? '';
    expect(parseAccount(lastBefore).acctId).toBe('00000000050');
    expect(lastAfter).toBe(lastBefore);
  });

  it('does not modify the input fixtures', () => {
    const tcatbal = readFileSync(join(ASCII_DATA_DIR, 'tcatbal.txt'), 'latin1');
    expect(tcatbal.startsWith('000000000010100010000000000{')).toBe(true);
  });
});
