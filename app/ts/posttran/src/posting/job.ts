import type { PostingFiles } from '../io/datasets.ts';
import { serializeDailyTransaction } from '../records/dailyTransaction.ts';
import { serializeRejectRecord } from '../records/rejectRecord.ts';
import { transactionCategoryBalanceKey } from '../records/transactionCategoryBalance.ts';
import { validateTransaction } from '../validation/validateTransaction.ts';
import { postTransaction } from './postTransaction.ts';
import type { Clock } from './timestamp.ts';
import { db2FormatTimestamp, systemClock } from './timestamp.ts';

/** Main processing loop of CBTRN02C (l.193-234). */

export interface PostingJobOptions {
  /** Injectable clock for `TRAN-PROC-TS`; defaults to the system clock. */
  readonly clock?: Clock;
  /** Sink for the program's DISPLAY output; defaults to discarding it. */
  readonly log?: (line: string) => void;
  /**
   * Bug-for-bug seam for reason 109 (spec §8.2). `false` (the default)
   * reproduces the COBOL, where a failed account rewrite is silently ignored
   * and the transaction is still posted to the transaction master. `true`
   * turns it into a reject, which is the correction the business may want.
   */
  readonly treatAccountRewriteFailureAsReject?: boolean;
}

export interface PostingJobResult {
  readonly transactionsProcessed: number;
  readonly transactionsRejected: number;
  /** `RETURN-CODE`: 4 when anything was rejected, otherwise 0 (l.229-231). */
  readonly returnCode: number;
}

export function runPostingJob(files: PostingFiles, options: PostingJobOptions = {}): PostingJobResult {
  const clock = options.clock ?? systemClock;
  const log = options.log ?? ((): void => undefined);
  const rejectOnAccountRewriteFailure = options.treatAccountRewriteFailureAsReject ?? false;

  log('START OF EXECUTION OF PROGRAM CBTRN02C');

  let transactionsProcessed = 0;
  let transactionsRejected = 0;

  for (const transaction of files.dalytran) {
    transactionsProcessed += 1;

    const validation = validateTransaction(transaction, files.xreffile, files.acctfile);
    let reason = validation.reason;

    if (reason.code === 0 && validation.crossReference !== undefined && validation.account !== undefined) {
      const key = transactionCategoryBalanceKey({
        accountId: validation.crossReference.accountId,
        typeCode: transaction.typeCode,
        categoryCode: transaction.categoryCode,
      });
      if (files.tcatbalf.read(key) === undefined) {
        log(`TCATBAL record not found for key : ${key}.. Creating.`);
      }

      const outcome = postTransaction(
        transaction,
        validation.crossReference,
        validation.account,
        db2FormatTimestamp(clock()),
        files,
      );
      if (rejectOnAccountRewriteFailure) {
        reason = outcome.reason;
      }
    }

    if (reason.code !== 0) {
      transactionsRejected += 1;
      files.dalyrejs.write(
        serializeRejectRecord({
          transactionImage: serializeDailyTransaction(transaction),
          reasonCode: reason.code,
          reasonDescription: reason.description,
        }),
      );
    }
  }

  log(`TRANSACTIONS PROCESSED :${transactionsProcessed.toString().padStart(9, '0')}`);
  log(`TRANSACTIONS REJECTED  :${transactionsRejected.toString().padStart(9, '0')}`);
  log('END OF EXECUTION OF PROGRAM CBTRN02C');

  return {
    transactionsProcessed,
    transactionsRejected,
    returnCode: transactionsRejected > 0 ? 4 : 0,
  };
}
