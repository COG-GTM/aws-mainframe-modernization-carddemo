/**
 * `CBEXPORT` — branch migration export.
 *
 * Reads the customer, account, cross-reference, transaction and card files in
 * that order and writes one 500 byte export record per input record, stamped
 * with a shared timestamp and an ascending sequence number.
 */

import type {
  AccountRecord,
  CardRecord,
  CardXrefRecord,
  CustomerRecord,
  TransactionRecord,
} from "@carddemo/domain";

import {
  EXPORT_RECORD_LENGTH,
  encodeExportRecord,
  type ExportHeader,
  type ExportRecord,
} from "./export-records.js";
import { writeFixedLengthRecords } from "./fixed-length-file.js";

/** The branch and region `CBEXPORT` stamps on every record. */
export const EXPORT_BRANCH_ID = "0001";
export const EXPORT_REGION_CODE = "NORTH";

export interface ExportInput {
  readonly customers?: readonly CustomerRecord[];
  readonly accounts?: readonly AccountRecord[];
  readonly xrefs?: readonly CardXrefRecord[];
  readonly transactions?: readonly TransactionRecord[];
  readonly cards?: readonly CardRecord[];
}

export interface ExportOptions {
  /** `WS-FORMATTED-TIMESTAMP`; defaults to the current date and time. */
  readonly timestamp?: string;
  readonly branchId?: string;
  readonly regionCode?: string;
}

export interface ExportStatistics {
  readonly customersExported: number;
  readonly accountsExported: number;
  readonly xrefsExported: number;
  readonly transactionsExported: number;
  readonly cardsExported: number;
  readonly totalExported: number;
}

export interface ExportResult {
  readonly records: readonly ExportRecord[];
  /** The encoded 500 byte records, in write order. */
  readonly lines: readonly string[];
  readonly statistics: ExportStatistics;
  readonly timestamp: string;
}

/** `1050-GENERATE-TIMESTAMP`: `YYYY-MM-DD HH:MM:SS.00`, padded to 26 bytes. */
export function formatExportTimestamp(now: Date = new Date()): string {
  const pad = (value: number, width = 2): string => String(value).padStart(width, "0");
  const date = `${pad(now.getFullYear(), 4)}-${pad(now.getMonth() + 1)}-${pad(now.getDate())}`;
  const time = `${pad(now.getHours())}:${pad(now.getMinutes())}:${pad(now.getSeconds())}`;
  return `${date} ${time}.00`.padEnd(26, " ");
}

export function cbexport(input: ExportInput, options: ExportOptions = {}): ExportResult {
  const timestamp = options.timestamp ?? formatExportTimestamp();
  const branchId = options.branchId ?? EXPORT_BRANCH_ID;
  const regionCode = options.regionCode ?? EXPORT_REGION_CODE;

  let sequence = 0;
  const header = (recType: string): ExportHeader => {
    sequence += 1;
    return { recType, timestamp, sequenceNum: sequence, branchId, regionCode };
  };

  const records: ExportRecord[] = [
    ...(input.customers ?? []).map(
      (customer): ExportRecord => ({ recType: "C", header: header("C"), customer }),
    ),
    ...(input.accounts ?? []).map(
      (account): ExportRecord => ({ recType: "A", header: header("A"), account }),
    ),
    ...(input.xrefs ?? []).map(
      (xref): ExportRecord => ({ recType: "X", header: header("X"), xref }),
    ),
    ...(input.transactions ?? []).map(
      (transaction): ExportRecord => ({ recType: "T", header: header("T"), transaction }),
    ),
    ...(input.cards ?? []).map(
      (card): ExportRecord => ({ recType: "D", header: header("D"), card }),
    ),
  ];

  const statistics: ExportStatistics = {
    customersExported: input.customers?.length ?? 0,
    accountsExported: input.accounts?.length ?? 0,
    xrefsExported: input.xrefs?.length ?? 0,
    transactionsExported: input.transactions?.length ?? 0,
    cardsExported: input.cards?.length ?? 0,
    totalExported: records.length,
  };

  return { records, lines: records.map(encodeExportRecord), statistics, timestamp };
}

/** Writes an export run to a `RECFM=F`, `LRECL=500` data set. */
export function writeExportFile(path: string, result: ExportResult): void {
  writeFixedLengthRecords(path, result.lines, EXPORT_RECORD_LENGTH);
}
