import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
import { describe, it } from 'node:test';

import { toDecimalString } from '../src/codec/money.ts';
import { parseAccount, serializeAccount } from '../src/records/account.ts';
import {
  parseCardCrossReference,
  serializeCardCrossReference,
} from '../src/records/cardCrossReference.ts';
import {
  parseDailyTransaction,
  serializeDailyTransaction,
} from '../src/records/dailyTransaction.ts';
import { parseRejectRecord, serializeRejectRecord } from '../src/records/rejectRecord.ts';
import {
  parseTransactionCategoryBalance,
  serializeTransactionCategoryBalance,
  transactionCategoryBalanceKey,
} from '../src/records/transactionCategoryBalance.ts';
import {
  parseTransactionMaster,
  serializeTransactionMaster,
} from '../src/records/transactionMaster.ts';
import { SAMPLE_DATA_DIR } from './helpers/sampleData.ts';

function sampleLines(fileName: string): string[] {
  return readFileSync(`${SAMPLE_DATA_DIR}/${fileName}`, 'latin1')
    .split('\n')
    .map((line) => line.replace(/\r$/, ''))
    .filter((line) => line.length > 0);
}

void describe('record layouts', () => {
  void it('parses the first sample daily transaction', () => {
    const line = sampleLines('dailytran.txt')[0] ?? '';
    const record = parseDailyTransaction(line);
    assert.equal(record.id, '0000000000683580');
    assert.equal(record.typeCode, '01');
    assert.equal(record.categoryCode, '0001');
    assert.equal(record.source, 'POS TERM  ');
    assert.equal(toDecimalString(record.amount), '504.77');
    assert.equal(record.merchantId, '800000000');
    assert.equal(record.cardNumber, '4859452612877065');
    assert.equal(record.originationTimestamp, '2022-06-10 19:27:53.000000');
  });

  void it('round-trips every sample daily transaction byte-for-byte', () => {
    for (const line of sampleLines('dailytran.txt')) {
      assert.equal(serializeDailyTransaction(parseDailyTransaction(line)), line.padEnd(350, ' '));
    }
  });

  void it('round-trips every sample account record byte-for-byte', () => {
    for (const line of sampleLines('acctdata.txt')) {
      assert.equal(serializeAccount(parseAccount(line)), line.padEnd(300, ' '));
    }
  });

  void it('parses account amounts and dates at the documented offsets', () => {
    const record = parseAccount(sampleLines('acctdata.txt')[0] ?? '');
    assert.equal(record.id, '00000000001');
    assert.equal(record.activeStatus, 'Y');
    assert.equal(toDecimalString(record.currentBalance), '194.00');
    assert.equal(toDecimalString(record.creditLimit), '2020.00');
    assert.equal(record.expirationDate, '2025-05-20');
    assert.equal(toDecimalString(record.currentCycleCredit), '0.00');
    assert.equal(toDecimalString(record.currentCycleDebit), '0.00');
  });

  void it('round-trips the sample cross-reference file (short lines are space padded)', () => {
    for (const line of sampleLines('cardxref.txt')) {
      assert.equal(
        serializeCardCrossReference(parseCardCrossReference(line)),
        line.padEnd(50, ' '),
      );
    }
    const record = parseCardCrossReference(sampleLines('cardxref.txt')[0] ?? '');
    assert.equal(record.cardNumber, '0500024453765740');
    assert.equal(record.accountId, '00000000050');
  });

  void it('round-trips the sample category balances and builds the 17-byte key', () => {
    for (const line of sampleLines('tcatbal.txt')) {
      assert.equal(
        serializeTransactionCategoryBalance(parseTransactionCategoryBalance(line)),
        line.padEnd(50, ' '),
      );
    }
    const record = parseTransactionCategoryBalance(sampleLines('tcatbal.txt')[0] ?? '');
    assert.equal(transactionCategoryBalanceKey(record), '00000000001010001');
    assert.equal(toDecimalString(record.balance), '0.00');
  });

  void it('treats the transaction master as the daily layout with the TRAN- prefix', () => {
    const line = sampleLines('dailytran.txt')[0] ?? '';
    const master = parseTransactionMaster(line);
    const daily = parseDailyTransaction(line);
    assert.equal(master.id, daily.id);
    assert.equal(master.amount, daily.amount);
    assert.equal(serializeTransactionMaster(master), serializeDailyTransaction(daily));
  });

  void it('builds a 430-byte reject record from the untouched transaction image', () => {
    const image = serializeDailyTransaction(parseDailyTransaction(sampleLines('dailytran.txt')[0] ?? ''));
    const line = serializeRejectRecord({
      transactionImage: image,
      reasonCode: 103,
      reasonDescription: 'TRANSACTION RECEIVED AFTER ACCT EXPIRATION',
    });
    assert.equal(line.length, 430);
    assert.equal(line.slice(0, 350), image);
    assert.equal(line.slice(350, 354), '0103');
    const parsed = parseRejectRecord(line);
    assert.equal(parsed.reasonCode, 103);
    assert.equal(parsed.reasonDescription.trimEnd(), 'TRANSACTION RECEIVED AFTER ACCT EXPIRATION');
  });
});
