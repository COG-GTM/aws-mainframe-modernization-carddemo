import { mkdtempSync, readFileSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { describe, expect, it } from 'vitest';

import { AccountStore, detectLineTerminator, readTranCatBalFile } from '../src/io.ts';
import { ACCOUNT_RECORD_LENGTH, formatAccount, formatTranCatBal } from '../src/records.ts';
import { accountRecord, balanceRecord } from './helpers.ts';

function tempFile(name: string, content: string): string {
  const path = join(mkdtempSync(join(tmpdir(), 'intcalc-io-')), name);
  writeFileSync(path, content, 'latin1');
  return path;
}

describe('readTranCatBalFile', () => {
  it('orders records by key bytes, not by locale collation', () => {
    // localeCompare puts 'a1' before 'A1'; a VSAM browse returns 'A1' first.
    const path = tempFile(
      'tcatbal.txt',
      [
        balanceRecord('101', 'a1', '0001', '10.00'),
        balanceRecord('101', 'A1', '0001', '10.00'),
        balanceRecord('101', '01', '0001', '10.00'),
      ]
        .map((record) => `${formatTranCatBal(record)}\n`)
        .join(''),
    );

    expect(readTranCatBalFile(path).map((record) => record.tranTypeCd)).toEqual(['01', 'A1', 'a1']);
  });
});

describe('record framing', () => {
  it('detects how a file separates its records', () => {
    expect(detectLineTerminator('aaa\nbbb\n')).toBe('\n');
    expect(detectLineTerminator('aaa\r\nbbb\r\n')).toBe('\r\n');
    expect(detectLineTerminator('aaabbb')).toBe('');
  });

  it('rewrites an account master with the framing it was read with', () => {
    const images = [
      accountRecord('101', 'GOLD      ', '1.00'),
      accountRecord('102', 'GOLD      ', '2.00'),
    ].map(formatAccount);

    for (const [content, expectedLength] of [
      [images.join(''), images.length * ACCOUNT_RECORD_LENGTH],
      [images.map((image) => `${image}\n`).join(''), images.length * (ACCOUNT_RECORD_LENGTH + 1)],
      [
        images.map((image) => `${image}\r\n`).join(''),
        images.length * (ACCOUNT_RECORD_LENGTH + 2),
      ],
    ] as const) {
      const path = tempFile('acct.txt', content);
      const store = AccountStore.fromFile(path);
      store.writeToFile(path);
      expect(readFileSync(path, 'latin1').length).toBe(expectedLength);
    }
  });
});
