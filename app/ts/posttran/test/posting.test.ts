import assert from 'node:assert/strict';
import { describe, it } from 'node:test';

import { fromDecimalString, toDecimalString } from '../src/codec/money.ts';
import type { KeyedUpdater } from '../src/io/keyedStore.ts';
import type { AccountRecord } from '../src/records/account.ts';
import type { DailyTransaction } from '../src/records/dailyTransaction.ts';
import { RejectFile } from '../src/io/datasets.ts';
import { KeyedLoader } from '../src/io/keyedStore.ts';
import { parseRejectRecord } from '../src/records/rejectRecord.ts';
import { transactionCategoryBalanceKey } from '../src/records/transactionCategoryBalance.ts';
import type { TransactionMasterRecord } from '../src/records/transactionMaster.ts';
import { transactionMasterKey } from '../src/records/transactionMaster.ts';
import { runPostingJob } from '../src/posting/job.ts';
import {
  applyAccountBalances,
  postTransaction,
  updateAccountRecord,
  updateCategoryBalance,
} from '../src/posting/postTransaction.ts';
import { db2FormatTimestamp } from '../src/posting/timestamp.ts';
import {
  account,
  accountStore,
  categoryBalance,
  categoryBalanceStore,
  crossReference,
  daily,
  xrefStore,
} from './helpers/fixtures.ts';

const TS = '2025-01-02-03.04.05.060000';

function tranfile(): KeyedLoader<TransactionMasterRecord> {
  return new KeyedLoader<TransactionMasterRecord>({
    ddName: 'TRANFILE',
    keyOf: transactionMasterKey,
  });
}

void describe('processing timestamp', () => {
  void it('formats the clock as DB2 YYYY-MM-DD-HH.MM.SS.hh0000', () => {
    assert.equal(db2FormatTimestamp(new Date(2025, 0, 2, 3, 4, 5, 67)), '2025-01-02-03.04.05.060000');
    assert.equal(db2FormatTimestamp(new Date(2025, 11, 31, 23, 59, 59, 999)), '2025-12-31-23.59.59.990000');
  });
});

void describe('account balance arithmetic', () => {
  void it('adds a positive amount to the balance and to cycle credit only', () => {
    const updated = applyAccountBalances(account(), daily({ amount: fromDecimalString('100.00') }));
    assert.equal(toDecimalString(updated.currentBalance), '294.00');
    assert.equal(toDecimalString(updated.currentCycleCredit), '100.00');
    assert.equal(toDecimalString(updated.currentCycleDebit), '0.00');
  });

  void it('adds a negative amount to the balance and makes cycle debit more negative', () => {
    const updated = applyAccountBalances(account(), daily({ amount: fromDecimalString('-50.25') }));
    assert.equal(toDecimalString(updated.currentBalance), '143.75');
    assert.equal(toDecimalString(updated.currentCycleCredit), '0.00');
    assert.equal(toDecimalString(updated.currentCycleDebit), '-50.25');
  });

  void it('treats zero as a credit (>= 0) and applies no rounding', () => {
    const updated = applyAccountBalances(
      account({ currentBalance: fromDecimalString('0.01') }),
      daily({ amount: fromDecimalString('0.00') }),
    );
    assert.equal(toDecimalString(updated.currentBalance), '0.01');
    assert.equal(toDecimalString(updated.currentCycleCredit), '0.00');
    assert.equal(toDecimalString(updated.currentCycleDebit), '0.00');
  });
});

void describe('category balance update', () => {
  void it('creates the record with the transaction amount when the key is new', () => {
    const tcatbalf = categoryBalanceStore([]);
    const created = updateCategoryBalance(daily(), crossReference(), tcatbalf);
    assert.equal(created, true);
    const key = transactionCategoryBalanceKey(categoryBalance());
    assert.equal(toDecimalString(tcatbalf.read(key)?.balance ?? 0n), '100.00');
  });

  void it('accumulates onto an existing balance', () => {
    const tcatbalf = categoryBalanceStore([
      categoryBalance({ balance: fromDecimalString('25.50') }),
    ]);
    const created = updateCategoryBalance(
      daily({ amount: fromDecimalString('-10.50') }),
      crossReference(),
      tcatbalf,
    );
    assert.equal(created, false);
    assert.equal(
      toDecimalString(tcatbalf.read(transactionCategoryBalanceKey(categoryBalance()))?.balance ?? 0n),
      '15.00',
    );
  });

  void it('keys on the cross-reference account id, not the card number', () => {
    const tcatbalf = categoryBalanceStore([]);
    updateCategoryBalance(daily(), crossReference({ accountId: '00000000099' }), tcatbalf);
    assert.equal(tcatbalf.all()[0]?.accountId, '00000000099');
  });
});

void describe('reason 109 seam (2800-UPDATE-ACCOUNT-REC)', () => {
  void it('returns reason 109 when the account has vanished, without throwing', () => {
    const reason = updateAccountRecord(account(), daily(), accountStore([]));
    assert.equal(reason.code, 109);
    assert.equal(reason.description, 'ACCOUNT RECORD NOT FOUND');
  });

  void it('posts the transaction anyway, which is the COBOL bug', () => {
    const targets = {
      tcatbalf: categoryBalanceStore([]),
      acctfile: accountStore([]),
      tranfile: tranfile(),
    };
    const outcome = postTransaction(daily(), crossReference(), account(), TS, targets);
    assert.equal(outcome.reason.code, 109);
    assert.equal(targets.tranfile.all().length, 1);
  });

  void it('is not turned into a reject by default, but can be by the option', () => {
    // The account is visible to validation but has vanished by rewrite time.
    const vanishingAcctfile = (): KeyedUpdater<AccountRecord> => {
      let reads = 0;
      return {
        read: (): AccountRecord | undefined => {
          reads += 1;
          return reads === 1 ? account() : undefined;
        },
        write: (): void => undefined,
        rewrite: (): void => {
          throw new Error('rewrite must not be reached');
        },
      };
    };
    const files = (): {
      dalytran: DailyTransaction[];
      xreffile: ReturnType<typeof xrefStore>;
      acctfile: KeyedUpdater<AccountRecord>;
      tcatbalf: ReturnType<typeof categoryBalanceStore>;
      tranfile: KeyedLoader<TransactionMasterRecord>;
      dalyrejs: RejectFile;
    } => ({
      dalytran: [daily()],
      xreffile: xrefStore([crossReference()]),
      acctfile: vanishingAcctfile(),
      tcatbalf: categoryBalanceStore([]),
      tranfile: tranfile(),
      dalyrejs: new RejectFile(),
    });

    const faithful = files();
    const faithfulResult = runPostingJob(faithful);
    assert.equal(faithfulResult.transactionsRejected, 0);
    assert.equal(faithfulResult.returnCode, 0);
    assert.equal(faithful.tranfile.all().length, 1, 'COBOL still writes the transaction');

    const corrected = files();
    const correctedResult = runPostingJob(corrected, { treatAccountRewriteFailureAsReject: true });
    assert.equal(correctedResult.transactionsRejected, 1);
    assert.equal(correctedResult.returnCode, 4);
    assert.equal(parseRejectRecord(corrected.dalyrejs.all()[0] ?? '').reasonCode, 109);
  });
});

void describe('posting job', () => {
  void it('regenerates the processing timestamp from the injected clock', () => {
    const files = {
      dalytran: [daily({ processingTimestamp: 'INBOUND VALUE DISCARDED  ' })],
      xreffile: xrefStore([crossReference()]),
      acctfile: accountStore([account()]),
      tcatbalf: categoryBalanceStore([]),
      tranfile: tranfile(),
      dalyrejs: new RejectFile(),
    };
    const result = runPostingJob(files, { clock: () => new Date(2025, 0, 2, 3, 4, 5, 60) });
    assert.equal(result.returnCode, 0);
    assert.equal(files.tranfile.all()[0]?.processingTimestamp, TS);
    assert.equal(files.tranfile.all()[0]?.originationTimestamp, daily().originationTimestamp);
  });

  void it('writes a 430-byte reject holding the original image and the reason', () => {
    const dalyrejs = new RejectFile();
    const result = runPostingJob({
      dalytran: [daily({ cardNumber: '9999999999999999' })],
      xreffile: xrefStore([crossReference()]),
      acctfile: accountStore([account()]),
      tcatbalf: categoryBalanceStore([]),
      tranfile: tranfile(),
      dalyrejs,
    });
    assert.equal(result.transactionsProcessed, 1);
    assert.equal(result.transactionsRejected, 1);
    assert.equal(result.returnCode, 4);

    const image = dalyrejs.all()[0] ?? '';
    assert.equal(image.length, 430);
    const reject = parseRejectRecord(image);
    assert.equal(reject.reasonCode, 100);
    assert.equal(reject.reasonDescription.trim(), 'INVALID CARD NUMBER FOUND');
    assert.equal(reject.transactionImage.slice(262, 278), '9999999999999999');
  });

  void it('does not post rejected transactions to any file', () => {
    const files = {
      dalytran: [daily({ cardNumber: '9999999999999999' })],
      xreffile: xrefStore([crossReference()]),
      acctfile: accountStore([account()]),
      tcatbalf: categoryBalanceStore([]),
      tranfile: tranfile(),
      dalyrejs: new RejectFile(),
    };
    runPostingJob(files);
    assert.equal(files.tranfile.all().length, 0);
    assert.equal(files.tcatbalf.all().length, 0);
    assert.equal(toDecimalString(files.acctfile.read('00000000001')?.currentBalance ?? 0n), '194.00');
  });

  void it('counts every input record and returns 0 when nothing is rejected', () => {
    const logged: string[] = [];
    const result = runPostingJob(
      {
        dalytran: [daily(), daily({ id: '0000000000683581' })],
        xreffile: xrefStore([crossReference()]),
        acctfile: accountStore([account({ creditLimit: fromDecimalString('99999.00') })]),
        tcatbalf: categoryBalanceStore([]),
        tranfile: tranfile(),
        dalyrejs: new RejectFile(),
      },
      { log: (line) => logged.push(line) },
    );
    assert.equal(result.transactionsProcessed, 2);
    assert.equal(result.transactionsRejected, 0);
    assert.equal(result.returnCode, 0);
    assert.ok(logged.includes('TRANSACTIONS PROCESSED :000000002'));
    assert.ok(logged.includes('TRANSACTIONS REJECTED  :000000000'));
  });

  void it('accumulates repeated postings for the same account and category', () => {
    const files = {
      dalytran: [daily(), daily({ id: '0000000000683581', amount: fromDecimalString('-40.00') })],
      xreffile: xrefStore([crossReference()]),
      acctfile: accountStore([account({ creditLimit: fromDecimalString('99999.00') })]),
      tcatbalf: categoryBalanceStore([]),
      tranfile: tranfile(),
      dalyrejs: new RejectFile(),
    };
    runPostingJob(files);
    const posted = files.acctfile.read('00000000001');
    assert.equal(toDecimalString(posted?.currentBalance ?? 0n), '254.00');
    assert.equal(toDecimalString(posted?.currentCycleCredit ?? 0n), '100.00');
    assert.equal(toDecimalString(posted?.currentCycleDebit ?? 0n), '-40.00');
    assert.equal(toDecimalString(files.tcatbalf.all()[0]?.balance ?? 0n), '60.00');
  });
});
