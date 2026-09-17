/**
 * The resources a CICS task has addressable: the files named in the
 * `FILE(...)` options of the transaction programs, the transient data queue
 * the report program writes to, and the clock behind `EIBDATE`/`EIBTIME`.
 */

import type { AccountRecord, CardXrefRecord, TransactionRecord } from "@carddemo/domain";
import { cardXrefCodec, padKey } from "@carddemo/domain";
import {
  dataFiles,
  openAccountFile,
  openCardXrefFile,
  openTransactionFile,
  Ksds,
} from "@carddemo/vsam";

import type { ReportRequestQueue } from "./report-queue.js";
import { InMemoryReportRequestQueue } from "./report-queue.js";

export interface OnlineFiles {
  /** `TRANSACT`, keyed on `TRAN-ID`. */
  readonly transactions: Ksds<TransactionRecord>;
  /** `ACCTDAT`, keyed on `ACCT-ID`. */
  readonly accounts: Ksds<AccountRecord>;
  /** `CCXREF`, keyed on `XREF-CARD-NUM`. */
  readonly cardXref: Ksds<CardXrefRecord>;
  /**
   * `CXACAIX`, the alternate index over the card xref keyed on
   * `XREF-ACCT-ID`: the same records under a different key.
   */
  readonly acctXref: Ksds<CardXrefRecord>;
}

export interface OnlineContext {
  readonly files: OnlineFiles;
  readonly reports: ReportRequestQueue;
  /** Clock used for the screen header and the generated timestamps. */
  readonly now: () => Date;
  /**
   * Whether a successful `WRITE`/`REWRITE` is flushed back to the data file,
   * matching the VSAM update a CICS task performs.
   */
  readonly persist: boolean;
}

export interface OpenOnlineFilesOptions {
  /** `TRANSACT`; the repository ships no sample file, so a path is required. */
  readonly transactionFile: string;
  readonly accountFile?: string;
  readonly cardXrefFile?: string;
}

/** Opens the four files with `OPEN` semantics, leaving them ready for I/O. */
export function openOnlineFiles(options: OpenOnlineFilesOptions): OnlineFiles {
  const xrefPath = options.cardXrefFile ?? dataFiles.cardxref;
  const files: OnlineFiles = {
    transactions: openTransactionFile(options.transactionFile),
    accounts: openAccountFile(options.accountFile ?? dataFiles.acctdata),
    cardXref: openCardXrefFile(xrefPath),
    acctXref: new Ksds<CardXrefRecord>({
      name: "CXACAIX",
      path: xrefPath,
      codec: cardXrefCodec,
      keyOf: (record) => padKey(record.xrefAcctId, 11),
    }),
  };
  files.transactions.openFile();
  files.accounts.openFile();
  files.cardXref.openFile();
  files.acctXref.openFile();
  return files;
}

export interface CreateContextOptions extends OpenOnlineFilesOptions {
  readonly reports?: ReportRequestQueue;
  readonly now?: () => Date;
  readonly persist?: boolean;
}

export function createOnlineContext(options: CreateContextOptions): OnlineContext {
  return {
    files: openOnlineFiles(options),
    reports: options.reports ?? new InMemoryReportRequestQueue(),
    now: options.now ?? ((): Date => new Date()),
    persist: options.persist ?? true,
  };
}
