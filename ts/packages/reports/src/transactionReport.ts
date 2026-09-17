/**
 * `CBTRN03C` — the daily transaction detail report.
 *
 * Line layouts come from `CVTRA07Y`; the control break structure (headers on
 * every page, account totals on a card number break, page totals every
 * `WS-PAGE-SIZE` report lines and a grand total at end of file) is preserved
 * line for line, as is the `DATEPARM` selection.
 */

import type {
  CardXrefRecord,
  TranCatRecord,
  TranTypeRecord,
  TransactionRecord,
} from "@carddemo/domain";
import { FileStatus, type Ksds } from "@carddemo/vsam";

import { AbendError } from "./abend.js";
import { addMoney } from "./money.js";
import { alphanumeric, displayDigits, formatEdited } from "./picture.js";

/** `FD-REPTFILE-REC PIC X(133)`. */
export const REPORT_RECORD_LENGTH = 133;

/** `WS-PAGE-SIZE PIC 9(03) COMP-3 VALUE 20`. */
export const PAGE_SIZE = 20;

const DETAIL_AMOUNT_PICTURE = "-ZZZ,ZZZ,ZZZ.ZZ";
const TOTAL_AMOUNT_PICTURE = "+ZZZ,ZZZ,ZZZ.ZZ";

/** `WS-DATEPARM-RECORD`: `WS-START-DATE PIC X(10)` and `WS-END-DATE PIC X(10)`. */
export interface DateParm {
  readonly startDate: string;
  readonly endDate: string;
}

export interface TransactionReportFiles {
  /** `TRANFILE`, already filtered and sorted by card number by the JCL SORT step. */
  readonly transactions: readonly TransactionRecord[];
  /** `CARDXREF`. */
  readonly xref: Ksds<CardXrefRecord>;
  /** `TRANTYPE`. */
  readonly tranTypes: Ksds<TranTypeRecord>;
  /** `TRANCATG`. */
  readonly tranCats: Ksds<TranCatRecord>;
}

function reportRecord(text: string): string {
  return alphanumeric(text, REPORT_RECORD_LENGTH);
}

/** `REPORT-NAME-HEADER`. */
export function reportNameHeader(dateParm: DateParm): string {
  return reportRecord(
    `${alphanumeric("DALYREPT", 38)}${alphanumeric("Daily Transaction Report", 41)}` +
      `Date Range: ${alphanumeric(dateParm.startDate, 10)} to ${alphanumeric(dateParm.endDate, 10)}`,
  );
}

/** `TRANSACTION-HEADER-1`. */
export const TRANSACTION_HEADER_1 = reportRecord(
  `${alphanumeric("Transaction ID", 17)}${alphanumeric("Account ID", 12)}` +
    `${alphanumeric("Transaction Type", 19)}${alphanumeric("Tran Category", 35)}` +
    `${alphanumeric("Tran Source", 14)} ${alphanumeric("        Amount", 16)}`,
);

/** `TRANSACTION-HEADER-2 PIC X(133) VALUE ALL '-'`. */
export const TRANSACTION_HEADER_2 = "-".repeat(REPORT_RECORD_LENGTH);

/** `WS-BLANK-LINE PIC X(133) VALUE SPACES`. */
export const BLANK_LINE = " ".repeat(REPORT_RECORD_LENGTH);

/** `TRANSACTION-DETAIL-REPORT`. */
export function transactionDetailLine(
  transaction: TransactionRecord,
  xref: CardXrefRecord,
  tranType: TranTypeRecord,
  tranCat: TranCatRecord,
): string {
  return reportRecord(
    `${alphanumeric(transaction.tranId, 16)} ${alphanumeric(displayDigits(xref.xrefAcctId, 11), 11)} ` +
      `${alphanumeric(transaction.tranTypeCd, 2)}-${alphanumeric(tranType.tranTypeDesc, 15)} ` +
      `${displayDigits(transaction.tranCatCd, 4)}-${alphanumeric(tranCat.tranCatTypeDesc, 29)} ` +
      `${alphanumeric(transaction.tranSource, 10)}    ` +
      `${formatEdited(DETAIL_AMOUNT_PICTURE, transaction.tranAmt)}  `,
  );
}

const totalLine = (label: string, labelWidth: number, fillWidth: number, total: number): string =>
  reportRecord(
    `${alphanumeric(label, labelWidth)}${".".repeat(fillWidth)}` +
      formatEdited(TOTAL_AMOUNT_PICTURE, total),
  );

/** `REPORT-PAGE-TOTALS`. */
export const pageTotalLine = (total: number): string => totalLine("Page Total", 11, 86, total);

/** `REPORT-ACCOUNT-TOTALS`. */
export const accountTotalLine = (total: number): string =>
  totalLine("Account Total", 13, 84, total);

/** `REPORT-GRAND-TOTALS`. */
export const grandTotalLine = (total: number): string => totalLine("Grand Total", 11, 86, total);

export interface TransactionSelection {
  readonly transactions: readonly TransactionRecord[];
  /** A transaction outside the range ended the read loop before end of file. */
  readonly stoppedEarly: boolean;
}

/**
 * Applies the `DATEPARM` selection on `TRAN-PROC-TS (1:10)`.
 *
 * COBOL leaves the read loop with `NEXT SENTENCE` as soon as a transaction
 * falls outside the range, so reporting stops at the first such record rather
 * than skipping it, and the end of file totals are never reached. The JCL SORT
 * step ahead of the program filters the input on the same dates, so a run over
 * `TRANSACT.DALY` never trips that path.
 */
export function selectTransactions(
  transactions: readonly TransactionRecord[],
  dateParm: DateParm,
): TransactionSelection {
  const selected: TransactionRecord[] = [];
  for (const transaction of transactions) {
    const processedDate = transaction.tranProcTs.slice(0, 10);
    if (processedDate < dateParm.startDate || processedDate > dateParm.endDate) {
      return { transactions: selected, stoppedEarly: true };
    }
    selected.push(transaction);
  }
  return { transactions: selected, stoppedEarly: false };
}

class TransactionReportWriter {
  private readonly lines: string[] = [];
  private firstTime = true;
  private lineCounter = 0;
  private pageTotal = 0;
  private accountTotal = 0;
  private grandTotal = 0;

  constructor(private readonly dateParm: DateParm) {}

  /** `1111-WRITE-REPORT-REC`. */
  private writeReportRec(line: string): void {
    this.lines.push(line);
  }

  /** `1120-WRITE-HEADERS`. */
  private writeHeaders(): void {
    this.writeReportRec(reportNameHeader(this.dateParm));
    this.lineCounter += 1;
    this.writeReportRec(BLANK_LINE);
    this.lineCounter += 1;
    this.writeReportRec(TRANSACTION_HEADER_1);
    this.lineCounter += 1;
    this.writeReportRec(TRANSACTION_HEADER_2);
    this.lineCounter += 1;
  }

  /** `1110-WRITE-PAGE-TOTALS`. */
  writePageTotals(): void {
    this.writeReportRec(pageTotalLine(this.pageTotal));
    this.grandTotal = addMoney(this.grandTotal, this.pageTotal);
    this.pageTotal = 0;
    this.lineCounter += 1;
    this.writeReportRec(TRANSACTION_HEADER_2);
    this.lineCounter += 1;
  }

  /** `1120-WRITE-ACCOUNT-TOTALS`. */
  writeAccountTotals(): void {
    this.writeReportRec(accountTotalLine(this.accountTotal));
    this.accountTotal = 0;
    this.lineCounter += 1;
    this.writeReportRec(TRANSACTION_HEADER_2);
    this.lineCounter += 1;
  }

  /** `1110-WRITE-GRAND-TOTALS`. */
  writeGrandTotals(): void {
    this.writeReportRec(grandTotalLine(this.grandTotal));
  }

  /** `1100-WRITE-TRANSACTION-REPORT` including `1120-WRITE-DETAIL`. */
  writeTransaction(
    transaction: TransactionRecord,
    xref: CardXrefRecord,
    tranType: TranTypeRecord,
    tranCat: TranCatRecord,
  ): void {
    if (this.firstTime) {
      this.firstTime = false;
      this.writeHeaders();
    }

    if (this.lineCounter % PAGE_SIZE === 0) {
      this.writePageTotals();
      this.writeHeaders();
    }

    this.accumulate(transaction.tranAmt);
    this.writeReportRec(transactionDetailLine(transaction, xref, tranType, tranCat));
    this.lineCounter += 1;
  }

  /** `ADD TRAN-AMT TO WS-PAGE-TOTAL WS-ACCOUNT-TOTAL`. */
  accumulate(amount: number): void {
    this.pageTotal = addMoney(this.pageTotal, amount);
    this.accountTotal = addMoney(this.accountTotal, amount);
  }

  get started(): boolean {
    return !this.firstTime;
  }

  toLines(): string[] {
    return [...this.lines];
  }
}

/** `CARD-XREF-RECORD` before the first `1500-A-LOOKUP-XREF`. */
const emptyXref: CardXrefRecord = { xrefCardNum: "", xrefCustId: 0, xrefAcctId: 0 };

function lookup<T>(store: Ksds<T>, key: string, description: string): T {
  const result = store.read(key);
  if (result.status !== FileStatus.ok || result.record === undefined) {
    throw new AbendError(`${description} : ${key}`);
  }
  return result.record;
}

/**
 * Runs the report, returning the `TRANREPT` records. Every line is exactly
 * {@link REPORT_RECORD_LENGTH} characters, as written by the `FD`.
 */
export function generateTransactionReport(
  files: TransactionReportFiles,
  dateParm: DateParm,
): string[] {
  const writer = new TransactionReportWriter(dateParm);
  const selection = selectTransactions(files.transactions, dateParm);

  let currentCardNum = "";
  let xref = emptyXref;
  let lastAmount = 0;

  for (const transaction of selection.transactions) {
    if (currentCardNum !== transaction.tranCardNum) {
      if (writer.started) {
        writer.writeAccountTotals();
      }
      currentCardNum = transaction.tranCardNum;
      xref = lookup(files.xref, currentCardNum, "INVALID CARD NUMBER");
    }

    const tranType = lookup(files.tranTypes, transaction.tranTypeCd, "INVALID TRANSACTION TYPE");
    const tranCat = lookup(
      files.tranCats,
      `${transaction.tranTypeCd}${displayDigits(transaction.tranCatCd, 4)}`,
      "INVALID TRANSACTION CATEGORY",
    );

    writer.writeTransaction(transaction, xref, tranType, tranCat);
    lastAmount = transaction.tranAmt;
  }

  if (!selection.stoppedEarly) {
    // End of file: the last transaction amount is still in `TRAN-RECORD`, so
    // the program adds it once more before the page and grand totals.
    writer.accumulate(lastAmount);
    writer.writePageTotals();
    writer.writeGrandTotals();
  }

  return writer.toLines();
}
