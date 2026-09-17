/**
 * `CBIMPORT` — branch migration import.
 *
 * Splits an export file back into the five normalized record streams. A
 * record whose type is none of `C`, `A`, `X`, `T` or `D` is counted and
 * written to the error file instead.
 */

import type {
  AccountRecord,
  CardRecord,
  CardXrefRecord,
  CustomerRecord,
  TransactionRecord,
} from "@carddemo/domain";

import { EXPORT_RECORD_LENGTH, decodeExportRecord } from "./export-records.js";
import { readFixedLengthRecords } from "./fixed-length-file.js";

export const ERROR_RECORD_LENGTH = 132;

export const UNKNOWN_RECORD_TYPE_MESSAGE = "Unknown record type encountered";

export interface ImportStatistics {
  readonly totalRecordsRead: number;
  readonly customersImported: number;
  readonly accountsImported: number;
  readonly xrefsImported: number;
  readonly transactionsImported: number;
  readonly cardsImported: number;
  readonly errorRecordsWritten: number;
  readonly unknownRecordTypeCount: number;
}

export interface ImportResult {
  readonly customers: readonly CustomerRecord[];
  readonly accounts: readonly AccountRecord[];
  readonly xrefs: readonly CardXrefRecord[];
  readonly transactions: readonly TransactionRecord[];
  readonly cards: readonly CardRecord[];
  /** `ERROR-OUTPUT` records, already formatted to 132 bytes. */
  readonly errors: readonly string[];
  readonly statistics: ImportStatistics;
}

export interface ImportOptions {
  /** `FUNCTION CURRENT-DATE` for the error records. */
  readonly timestamp?: string;
}

/** `WS-ERROR-RECORD`: pipe-separated timestamp, type, sequence and message. */
export function formatErrorRecord(
  timestamp: string,
  recordType: string,
  sequence: number,
  message: string,
): string {
  const record =
    timestamp.padEnd(26, " ").slice(0, 26) +
    "|" +
    recordType.padEnd(1, " ").slice(0, 1) +
    "|" +
    String(sequence % 10_000_000).padStart(7, "0") +
    "|" +
    message.padEnd(50, " ").slice(0, 50);
  return record.padEnd(ERROR_RECORD_LENGTH, " ");
}

export function cbimport(lines: readonly string[], options: ImportOptions = {}): ImportResult {
  const customers: CustomerRecord[] = [];
  const accounts: AccountRecord[] = [];
  const xrefs: CardXrefRecord[] = [];
  const transactions: TransactionRecord[] = [];
  const cards: CardRecord[] = [];
  const errors: string[] = [];

  for (const line of lines) {
    const record = decodeExportRecord(line);
    switch (record.recType) {
      case "C":
        customers.push(record.customer);
        break;
      case "A":
        accounts.push(record.account);
        break;
      case "X":
        xrefs.push(record.xref);
        break;
      case "T":
        transactions.push(record.transaction);
        break;
      case "D":
        cards.push(record.card);
        break;
      case "unknown":
        errors.push(
          formatErrorRecord(
            options.timestamp ?? new Date().toISOString(),
            record.header.recType,
            record.header.sequenceNum,
            UNKNOWN_RECORD_TYPE_MESSAGE,
          ),
        );
        break;
    }
  }

  return {
    customers,
    accounts,
    xrefs,
    transactions,
    cards,
    errors,
    statistics: {
      totalRecordsRead: lines.length,
      customersImported: customers.length,
      accountsImported: accounts.length,
      xrefsImported: xrefs.length,
      transactionsImported: transactions.length,
      cardsImported: cards.length,
      errorRecordsWritten: errors.length,
      unknownRecordTypeCount: errors.length,
    },
  };
}

/** Reads a `RECFM=F`, `LRECL=500` export data set and imports it. */
export function importExportFile(path: string, options: ImportOptions = {}): ImportResult {
  return cbimport(readFixedLengthRecords(path, EXPORT_RECORD_LENGTH), options);
}
