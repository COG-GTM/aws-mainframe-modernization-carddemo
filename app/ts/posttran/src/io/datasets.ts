import type { AccountRecord } from '../records/account.ts';
import { ACCOUNT_LENGTH, accountKey, parseAccount, serializeAccount } from '../records/account.ts';
import type { CardCrossReference } from '../records/cardCrossReference.ts';
import {
  CARD_XREF_LENGTH,
  cardCrossReferenceKey,
  parseCardCrossReference,
} from '../records/cardCrossReference.ts';
import type { DailyTransaction } from '../records/dailyTransaction.ts';
import { DAILY_TRANSACTION_LENGTH, parseDailyTransaction } from '../records/dailyTransaction.ts';
import { REJECT_LENGTH } from '../records/rejectRecord.ts';
import type { TransactionCategoryBalance } from '../records/transactionCategoryBalance.ts';
import {
  TRAN_CAT_BAL_LENGTH,
  parseTransactionCategoryBalance,
  serializeTransactionCategoryBalance,
  transactionCategoryBalanceKey,
} from '../records/transactionCategoryBalance.ts';
import type { TransactionMasterRecord } from '../records/transactionMaster.ts';
import {
  serializeTransactionMaster,
  transactionMasterKey,
} from '../records/transactionMaster.ts';
import { readRecordLines, writeRecordLines } from './fixedWidthFile.ts';
import type { KeyedReader, KeyedUpdater, SequentialWriter } from './keyedStore.ts';
import { KeyedLoader, KeyedStore } from './keyedStore.ts';

/**
 * The six DD names of job step `POSTTRAN.STEP15`, as in-process equivalents.
 * No database or external infrastructure is involved: VSAM KSDS files become
 * keyed stores loaded from, and written back to, fixed-width files.
 */
export interface PostingFiles {
  /** `DALYTRAN` — sequential input, read once from top to bottom. */
  readonly dalytran: readonly DailyTransaction[];
  /** `XREFFILE` — KSDS read by card number. */
  readonly xreffile: KeyedReader<CardCrossReference>;
  /** `ACCTFILE` — KSDS opened I-O, read and rewritten by account id. */
  readonly acctfile: KeyedUpdater<AccountRecord>;
  /** `TCATBALF` — KSDS opened I-O, read, rewritten and written. */
  readonly tcatbalf: KeyedUpdater<TransactionCategoryBalance>;
  /** `TRANFILE` — KSDS opened OUTPUT, i.e. loaded from empty. */
  readonly tranfile: KeyedLoader<TransactionMasterRecord>;
  /** `DALYREJS` — sequential output of 430-byte reject records. */
  readonly dalyrejs: SequentialWriter;
}

export interface PostingFilePaths {
  readonly dalytran: string;
  readonly xreffile: string;
  readonly acctfile: string;
  readonly tcatbalf: string;
  readonly tranfile: string;
  readonly dalyrejs: string;
}

/** Collects reject images in memory and writes them on close. */
export class RejectFile implements SequentialWriter {
  private readonly images: string[] = [];

  write(image: string): void {
    if (image.length !== REJECT_LENGTH) {
      throw new RangeError(`reject record must be ${REJECT_LENGTH} bytes, got ${image.length}`);
    }
    this.images.push(image);
  }

  all(): readonly string[] {
    return this.images;
  }
}

export interface OpenPostingFiles extends PostingFiles {
  readonly acctfile: KeyedStore<AccountRecord>;
  readonly tcatbalf: KeyedStore<TransactionCategoryBalance>;
  readonly dalyrejs: RejectFile;
}

/** Opens all six datasets, loading the keyed files into memory. */
export function openPostingFiles(paths: PostingFilePaths): OpenPostingFiles {
  const dalytran = readRecordLines(paths.dalytran, DAILY_TRANSACTION_LENGTH).map(
    parseDailyTransaction,
  );
  const xreffile = new KeyedStore<CardCrossReference>(
    { ddName: 'XREFFILE', keyOf: cardCrossReferenceKey },
    readRecordLines(paths.xreffile, CARD_XREF_LENGTH).map(parseCardCrossReference),
  );
  const acctfile = new KeyedStore<AccountRecord>(
    { ddName: 'ACCTFILE', keyOf: accountKey },
    readRecordLines(paths.acctfile, ACCOUNT_LENGTH).map(parseAccount),
  );
  const tcatbalf = new KeyedStore<TransactionCategoryBalance>(
    { ddName: 'TCATBALF', keyOf: transactionCategoryBalanceKey },
    readRecordLines(paths.tcatbalf, TRAN_CAT_BAL_LENGTH).map(parseTransactionCategoryBalance),
  );
  const tranfile = new KeyedLoader<TransactionMasterRecord>({
    ddName: 'TRANFILE',
    keyOf: transactionMasterKey,
  });

  return { dalytran, xreffile, acctfile, tcatbalf, tranfile, dalyrejs: new RejectFile() };
}

/**
 * Writes the updated datasets back. `TRANFILE` is replaced wholesale because
 * CBTRN02C opens it OUTPUT (spec §4.3), and `DALYREJS` is a new generation of
 * the GDG, so it is always written even when empty.
 */
export function closePostingFiles(files: OpenPostingFiles, paths: PostingFilePaths): void {
  writeRecordLines(paths.acctfile, files.acctfile.all().map(serializeAccount));
  writeRecordLines(paths.tcatbalf, files.tcatbalf.all().map(serializeTransactionCategoryBalance));
  writeRecordLines(paths.tranfile, files.tranfile.all().map(serializeTransactionMaster));
  writeRecordLines(paths.dalyrejs, files.dalyrejs.all());
}
