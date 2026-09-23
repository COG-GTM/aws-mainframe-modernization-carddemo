import assert from 'node:assert/strict';
import { describe, it } from 'node:test';

import { fromDecimalString } from '../src/codec/money.ts';
import type { AccountRecord } from '../src/records/account.ts';
import {
  isWithinCreditLimit,
  isWithinExpiry,
  validateTransaction,
} from '../src/validation/validateTransaction.ts';
import { account, accountStore, crossReference, daily, xrefStore } from './helpers/fixtures.ts';

void describe('validation', () => {
  void it('passes a transaction with a known card, known account, within limit and expiry', () => {
    const result = validateTransaction(
      daily(),
      xrefStore([crossReference()]),
      accountStore([account()]),
    );
    assert.equal(result.reason.code, 0);
    assert.equal(result.account?.id, '00000000001');
    assert.equal(result.crossReference?.accountId, '00000000001');
  });

  void it('rejects 100 when the card is not in the cross-reference', () => {
    const result = validateTransaction(
      daily({ cardNumber: '9999999999999999' }),
      xrefStore([crossReference()]),
      accountStore([account()]),
    );
    assert.equal(result.reason.code, 100);
    assert.equal(result.reason.description, 'INVALID CARD NUMBER FOUND');
  });

  void it('skips the account lookup entirely after reason 100 (short circuit)', () => {
    let accountReads = 0;
    const acctfile = {
      read(key: string): AccountRecord | undefined {
        accountReads += 1;
        return accountStore([account()]).read(key);
      },
    };
    const result = validateTransaction(
      daily({ cardNumber: '9999999999999999' }),
      xrefStore([crossReference()]),
      acctfile,
    );
    assert.equal(result.reason.code, 100);
    assert.equal(accountReads, 0);
  });

  void it('rejects 101 when the cross-referenced account is missing', () => {
    const result = validateTransaction(daily(), xrefStore([crossReference()]), accountStore([]));
    assert.equal(result.reason.code, 101);
    assert.equal(result.reason.description, 'ACCOUNT RECORD NOT FOUND');
    assert.equal(result.account, undefined);
  });

  void it('rejects 102 when cycle credit - cycle debit + amount exceeds the credit limit', () => {
    const result = validateTransaction(
      daily({ amount: fromDecimalString('2020.01') }),
      xrefStore([crossReference()]),
      accountStore([account()]),
    );
    assert.equal(result.reason.code, 102);
    assert.equal(result.reason.description, 'OVERLIMIT TRANSACTION');
  });

  void it('treats exactly equal to the credit limit as within limit (>=)', () => {
    assert.equal(isWithinCreditLimit(account(), daily({ amount: fromDecimalString('2020.00') })), true);
    assert.equal(isWithinCreditLimit(account(), daily({ amount: fromDecimalString('2020.01') })), false);
  });

  void it('uses cycle credit minus cycle debit, not the current balance', () => {
    const subject = account({
      currentBalance: fromDecimalString('999999.00'),
      currentCycleCredit: fromDecimalString('2000.00'),
      currentCycleDebit: fromDecimalString('-500.00'),
    });
    // 2000.00 - (-500.00) + 100.00 = 2600.00 > 2020.00 credit limit.
    assert.equal(isWithinCreditLimit(subject, daily()), false);
  });

  void it('rejects 103 when the origination date is after the account expiry date', () => {
    const result = validateTransaction(
      daily({ originationTimestamp: '2025-05-21 00:00:00.000000' }),
      xrefStore([crossReference()]),
      accountStore([account()]),
    );
    assert.equal(result.reason.code, 103);
    assert.equal(result.reason.description, 'TRANSACTION RECEIVED AFTER ACCT EXPIRATION');
  });

  void it('compares expiry as 10 characters, not as dates', () => {
    const subject = account({ expirationDate: '2025-05-20' });
    assert.equal(isWithinExpiry(subject, daily({ originationTimestamp: '2025-05-20 23:59:59.999999' })), true);
    assert.equal(isWithinExpiry(subject, daily({ originationTimestamp: '2025-05-21 00:00:00.000000' })), false);
    // Lexicographic, so a non ISO-8601 or blank value silently changes the outcome.
    assert.equal(isWithinExpiry(subject, daily({ originationTimestamp: ' '.repeat(26) })), true);
    assert.equal(isWithinExpiry(account({ expirationDate: '          ' }), daily()), false);
  });

  void it('reports 103 and hides 102 when a transaction fails both tests', () => {
    const result = validateTransaction(
      daily({
        amount: fromDecimalString('99999.00'),
        originationTimestamp: '2025-05-21 00:00:00.000000',
      }),
      xrefStore([crossReference()]),
      accountStore([account()]),
    );
    assert.equal(result.reason.code, 103);
  });

  void it('does not check ACCT-ACTIVE-STATUS: transactions post to closed accounts', () => {
    const result = validateTransaction(
      daily(),
      xrefStore([crossReference()]),
      accountStore([account({ activeStatus: 'N' })]),
    );
    assert.equal(result.reason.code, 0);
  });
});
