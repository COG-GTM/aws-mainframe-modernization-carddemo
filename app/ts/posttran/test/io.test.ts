import assert from 'node:assert/strict';
import { mkdtempSync, readFileSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { describe, it } from 'node:test';

import { fromDecimalString } from '../src/codec/money.ts';
import { closePostingFiles, openPostingFiles, RejectFile } from '../src/io/datasets.ts';
import { FileAbendError } from '../src/io/errors.ts';
import { readRecordLines, writeRecordLines } from '../src/io/fixedWidthFile.ts';
import { KeyedLoader, KeyedStore } from '../src/io/keyedStore.ts';
import type { TransactionCategoryBalance } from '../src/records/transactionCategoryBalance.ts';
import { transactionCategoryBalanceKey } from '../src/records/transactionCategoryBalance.ts';
import { SAMPLE_DATA_DIR } from './helpers/sampleData.ts';

function balance(accountId: string, amount: string): TransactionCategoryBalance {
  return {
    accountId,
    typeCode: '01',
    categoryCode: '0001',
    balance: fromDecimalString(amount),
    filler: ' '.repeat(22),
  };
}

function scratchDir(): string {
  return mkdtempSync(join(tmpdir(), 'posttran-io-'));
}

void describe('fixed-width files', () => {
  void it('pads short lines, strips CRLF and skips blank lines', () => {
    const dir = scratchDir();
    const path = join(dir, 'tcatbal.txt');
    writeFileSync(path, '00000000001010001\r\n\n00000000002010001\n', 'latin1');
    const lines = readRecordLines(path, 50);
    assert.equal(lines.length, 2);
    assert.equal(lines[0], '00000000001010001'.padEnd(50, ' '));
  });

  void it('writes newline-separated records and an empty file for no records', () => {
    const dir = scratchDir();
    const path = join(dir, 'out.txt');
    writeRecordLines(path, ['a'.repeat(5), 'b'.repeat(5)]);
    assert.equal(readFileSync(path, 'latin1'), 'aaaaa\nbbbbb\n');
    writeRecordLines(path, []);
    assert.equal(readFileSync(path, 'latin1'), '');
  });
});

void describe('keyed store', () => {
  const options = { ddName: 'TCATBALF', keyOf: transactionCategoryBalanceKey };

  void it('reads by key and returns undefined for INVALID KEY', () => {
    const store = new KeyedStore<TransactionCategoryBalance>(options, [balance('00000000001', '5.00')]);
    assert.equal(store.read('00000000001010001')?.balance, 500n);
    assert.equal(store.read('00000000009010001'), undefined);
  });

  void it('abends on a duplicate WRITE and on a REWRITE of an unknown key', () => {
    const store = new KeyedStore<TransactionCategoryBalance>(options, [balance('00000000001', '5.00')]);
    assert.throws(() => { store.write(balance('00000000001', '1.00')); }, FileAbendError);
    assert.throws(() => { store.rewrite(balance('00000000002', '1.00')); }, FileAbendError);
    store.write(balance('00000000002', '1.00'));
    store.rewrite(balance('00000000002', '2.00'));
    assert.equal(store.read('00000000002010001')?.balance, 200n);
  });

  void it('returns records in ascending key order', () => {
    const store = new KeyedStore<TransactionCategoryBalance>(options, [
      balance('00000000003', '3.00'),
      balance('00000000001', '1.00'),
    ]);
    assert.deepEqual(
      store.all().map((record) => record.accountId),
      ['00000000001', '00000000003'],
    );
  });
});

void describe('keyed loader (OPEN OUTPUT)', () => {
  const options = { ddName: 'TCATBALF', keyOf: transactionCategoryBalanceKey };

  void it('accepts ascending keys and abends on out-of-sequence or duplicate keys', () => {
    const loader = new KeyedLoader<TransactionCategoryBalance>(options);
    loader.write(balance('00000000001', '1.00'));
    loader.write(balance('00000000002', '2.00'));
    assert.equal(loader.size, 2);
    assert.throws(() => { loader.write(balance('00000000001', '1.00')); }, FileAbendError);
    assert.throws(() => { loader.write(balance('00000000002', '2.00')); }, FileAbendError);
  });
});

void describe('reject file', () => {
  void it('only accepts 430-byte records', () => {
    const rejects = new RejectFile();
    assert.throws(() => { rejects.write('short'); }, RangeError);
    rejects.write('x'.repeat(430));
    assert.equal(rejects.all().length, 1);
  });
});

void describe('posting file adapters', () => {
  void it('opens the six sample datasets and writes the updated ones back', () => {
    const dir = scratchDir();
    const paths = {
      dalytran: join(SAMPLE_DATA_DIR, 'dailytran.txt'),
      xreffile: join(SAMPLE_DATA_DIR, 'cardxref.txt'),
      acctfile: join(dir, 'acctdata.txt'),
      tcatbalf: join(dir, 'tcatbal.txt'),
      tranfile: join(dir, 'transact.txt'),
      dalyrejs: join(dir, 'dalyrejs.txt'),
    };
    writeFileSync(paths.acctfile, readFileSync(join(SAMPLE_DATA_DIR, 'acctdata.txt')));
    writeFileSync(paths.tcatbalf, readFileSync(join(SAMPLE_DATA_DIR, 'tcatbal.txt')));

    const files = openPostingFiles(paths);
    assert.equal(files.dalytran.length, 300);
    assert.equal(files.acctfile.size, 50);
    assert.equal(files.tcatbalf.size, 50);
    assert.ok(files.xreffile.read('0500024453765740') !== undefined);
    assert.equal(files.xreffile.read('9999999999999999'), undefined);

    closePostingFiles(files, paths);
    assert.deepEqual(
      readRecordLines(paths.acctfile, 300).length,
      50,
    );
    assert.equal(readFileSync(paths.tranfile, 'latin1'), '');
    assert.equal(readFileSync(paths.dalyrejs, 'latin1'), '');
  });
});
