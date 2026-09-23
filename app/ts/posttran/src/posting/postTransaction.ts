import type { KeyedUpdater } from '../io/keyedStore.ts';
import type { KeyedLoader } from '../io/keyedStore.ts';
import type { AccountRecord } from '../records/account.ts';
import type { CardCrossReference } from '../records/cardCrossReference.ts';
import type { DailyTransaction } from '../records/dailyTransaction.ts';
import type { TransactionCategoryBalance } from '../records/transactionCategoryBalance.ts';
import { transactionCategoryBalanceKey } from '../records/transactionCategoryBalance.ts';
import type { TransactionMasterRecord } from '../records/transactionMaster.ts';
import type { ValidationReason } from '../validation/validateTransaction.ts';
import { VALIDATION_REASONS } from '../validation/validateTransaction.ts';

/** `2000-POST-TRANSACTION` (CBTRN02C l.424-444) and the paragraphs it calls. */

export interface PostingTargets {
  readonly tcatbalf: KeyedUpdater<TransactionCategoryBalance>;
  readonly acctfile: KeyedUpdater<AccountRecord>;
  readonly tranfile: Pick<KeyedLoader<TransactionMasterRecord>, 'write'>;
}

export interface PostTransactionOutcome {
  /**
   * Reason code left in `WS-VALIDATION-FAIL-REASON` after posting. This is
   * `109` when the account rewrite failed; in the COBOL nothing ever tests it
   * (spec §8.2), so it is reported here but does not reject the transaction.
   */
  readonly reason: ValidationReason;
  readonly categoryBalanceCreated: boolean;
  readonly transactionWritten: TransactionMasterRecord;
}

/**
 * `2700-UPDATE-TCATBAL` (l.467-542): the running balance is keyed on the
 * cross-reference account id plus the transaction type and category; it is
 * created with the transaction amount on first use and accumulated after.
 */
export function updateCategoryBalance(
  transaction: DailyTransaction,
  crossReference: CardCrossReference,
  tcatbalf: KeyedUpdater<TransactionCategoryBalance>,
): boolean {
  const key = transactionCategoryBalanceKey({
    accountId: crossReference.accountId,
    typeCode: transaction.typeCode,
    categoryCode: transaction.categoryCode,
  });
  const existing = tcatbalf.read(key);

  if (existing === undefined) {
    tcatbalf.write({
      accountId: crossReference.accountId,
      typeCode: transaction.typeCode,
      categoryCode: transaction.categoryCode,
      balance: transaction.amount,
      filler: ' '.repeat(22),
    });
    return true;
  }

  tcatbalf.rewrite({ ...existing, balance: existing.balance + transaction.amount });
  return false;
}

/**
 * `2800-UPDATE-ACCOUNT-REC` (l.545-560).
 *
 * Sign convention exactly as coded: the amount is always added to
 * `ACCT-CURR-BAL`; a non-negative amount is added to `ACCT-CURR-CYC-CREDIT`
 * and a negative amount is added to `ACCT-CURR-CYC-DEBIT`, making that bucket
 * more negative (spec §4.2).
 */
export function applyAccountBalances(
  accountRecord: AccountRecord,
  transaction: DailyTransaction,
): AccountRecord {
  const amount = transaction.amount;
  return {
    ...accountRecord,
    currentBalance: accountRecord.currentBalance + amount,
    currentCycleCredit:
      amount >= 0n ? accountRecord.currentCycleCredit + amount : accountRecord.currentCycleCredit,
    currentCycleDebit:
      amount < 0n ? accountRecord.currentCycleDebit + amount : accountRecord.currentCycleDebit,
  };
}

/**
 * Rewrites the updated account. On `INVALID KEY` the COBOL sets reason 109 and
 * carries on — no reject, no abend, and the transaction is still written to
 * the master. This function is the single seam for that dead logic: it returns
 * the reason so a future correction can act on it (spec §8.2).
 */
export function updateAccountRecord(
  accountRecord: AccountRecord,
  transaction: DailyTransaction,
  acctfile: KeyedUpdater<AccountRecord>,
): ValidationReason {
  const updated = applyAccountBalances(accountRecord, transaction);
  if (acctfile.read(updated.id) === undefined) {
    return VALIDATION_REASONS.accountRewriteFailed;
  }
  acctfile.rewrite(updated);
  return VALIDATION_REASONS.none;
}

/** `2000-POST-TRANSACTION` l.425-438: the transaction master image. */
export function buildTransactionMasterRecord(
  transaction: DailyTransaction,
  processingTimestamp: string,
): TransactionMasterRecord {
  return {
    id: transaction.id,
    typeCode: transaction.typeCode,
    categoryCode: transaction.categoryCode,
    source: transaction.source,
    description: transaction.description,
    amount: transaction.amount,
    merchantId: transaction.merchantId,
    merchantName: transaction.merchantName,
    merchantCity: transaction.merchantCity,
    merchantZip: transaction.merchantZip,
    cardNumber: transaction.cardNumber,
    originationTimestamp: transaction.originationTimestamp,
    // The inbound DALYTRAN-PROC-TS is discarded and regenerated (spec §4.3).
    processingTimestamp,
    filler: ' '.repeat(20),
  };
}

/**
 * Posts one validated transaction: category balance, then account balances,
 * then the transaction master write — the same order, and the same lack of
 * atomicity, as the COBOL.
 */
export function postTransaction(
  transaction: DailyTransaction,
  crossReference: CardCrossReference,
  accountRecord: AccountRecord,
  processingTimestamp: string,
  targets: PostingTargets,
): PostTransactionOutcome {
  const record = buildTransactionMasterRecord(transaction, processingTimestamp);
  const categoryBalanceCreated = updateCategoryBalance(transaction, crossReference, targets.tcatbalf);
  const reason = updateAccountRecord(accountRecord, transaction, targets.acctfile);
  targets.tranfile.write(record);
  return { reason, categoryBalanceCreated, transactionWritten: record };
}
