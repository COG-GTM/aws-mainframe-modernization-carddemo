import type { AccountRecord } from '../records/account.ts';
import type { CardCrossReference } from '../records/cardCrossReference.ts';
import type { DailyTransaction } from '../records/dailyTransaction.ts';
import type { KeyedReader } from '../io/keyedStore.ts';

/**
 * `1500-VALIDATE-TRAN` (CBTRN02C l.370-422).
 *
 * Reason codes and their descriptions are the literals moved into
 * `WS-VALIDATION-FAIL-REASON` / `WS-VALIDATION-FAIL-REASON-DESC`.
 */
export const VALIDATION_REASONS = {
  none: { code: 0, description: '' },
  invalidCardNumber: { code: 100, description: 'INVALID CARD NUMBER FOUND' },
  accountNotFound: { code: 101, description: 'ACCOUNT RECORD NOT FOUND' },
  overLimit: { code: 102, description: 'OVERLIMIT TRANSACTION' },
  expiredAccount: {
    code: 103,
    description: 'TRANSACTION RECEIVED AFTER ACCT EXPIRATION',
  },
  /** Set by `2800-UPDATE-ACCOUNT-REC` only; never tested (spec §3.1, §8.2). */
  accountRewriteFailed: { code: 109, description: 'ACCOUNT RECORD NOT FOUND' },
} as const;

export interface ValidationReason {
  readonly code: number;
  readonly description: string;
}

export interface ValidationResult {
  /** The single reason field of the COBOL: 0 when the transaction may post. */
  readonly reason: ValidationReason;
  /** The cross-reference record, when the card number was found. */
  readonly crossReference?: CardCrossReference;
  /** The account record, when the account was found. */
  readonly account?: AccountRecord;
}

/**
 * Over-limit test exactly as coded (l.403-407):
 * `ACCT-CREDIT-LIMIT >= ACCT-CURR-CYC-CREDIT - ACCT-CURR-CYC-DEBIT + DALYTRAN-AMT`.
 * Note it does not use `ACCT-CURR-BAL` (spec §8.6).
 */
export function isWithinCreditLimit(account: AccountRecord, transaction: DailyTransaction): boolean {
  const projected = account.currentCycleCredit - account.currentCycleDebit + transaction.amount;
  return account.creditLimit >= projected;
}

/**
 * Expiry test exactly as coded (l.414): a 10-character comparison of
 * `ACCT-EXPIRAION-DATE` against the first 10 characters of `DALYTRAN-ORIG-TS`.
 * This is a string comparison, not date arithmetic (spec §7).
 */
export function isWithinExpiry(account: AccountRecord, transaction: DailyTransaction): boolean {
  return account.expirationDate >= transaction.originationTimestamp.slice(0, 10);
}

/**
 * Validates one daily transaction.
 *
 * Bug-for-bug behaviour reproduced here:
 * - the cross-reference lookup short-circuits: reason 100 skips the account
 *   lookup entirely, so 100 and 101 can never both apply (l.372-376);
 * - the over-limit and expiry tests are independent `IF`s writing the same
 *   single reason field, so a transaction failing both is reported as 103 and
 *   the over-limit failure is invisible (l.407-420, spec §3.1).
 */
export function validateTransaction(
  transaction: DailyTransaction,
  xreffile: KeyedReader<CardCrossReference>,
  acctfile: KeyedReader<AccountRecord>,
): ValidationResult {
  const crossReference = xreffile.read(transaction.cardNumber);
  if (crossReference === undefined) {
    return { reason: VALIDATION_REASONS.invalidCardNumber };
  }

  const account = acctfile.read(crossReference.accountId);
  if (account === undefined) {
    return { reason: VALIDATION_REASONS.accountNotFound, crossReference };
  }

  let reason: ValidationReason = VALIDATION_REASONS.none;
  if (!isWithinCreditLimit(account, transaction)) {
    reason = VALIDATION_REASONS.overLimit;
  }
  if (!isWithinExpiry(account, transaction)) {
    reason = VALIDATION_REASONS.expiredAccount;
  }

  return { reason, crossReference, account };
}
