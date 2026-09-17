/**
 * Dataset plumbing shared by the posting programs: default paths for the DD
 * names in `POSTTRAN.jcl` and the `OPEN` behaviours the programs rely on.
 */

import { writeFileSync } from "node:fs";

import type { Ksds } from "@carddemo/vsam";
import { dataFiles, type FileStatusCode } from "@carddemo/vsam";

export interface DatasetPaths {
  /** `DALYTRAN` — daily transaction sequential file. */
  readonly dailyTran: string;
  /** `XREFFILE` — card cross-reference KSDS. */
  readonly xref: string;
  /** `ACCTFILE` — account master KSDS. */
  readonly account: string;
  /** `CARDFILE` — card master KSDS. */
  readonly card: string;
  /** `CUSTFILE` — customer master KSDS. */
  readonly customer: string;
  /** `TCATBALF` — transaction category balance KSDS. */
  readonly tranCatBal: string;
  /** `TRANFILE` — transaction master KSDS. */
  readonly transaction: string;
}

export type DatasetOverrides = Partial<DatasetPaths> &
  Pick<DatasetPaths, "transaction">;

export function resolveDatasets(overrides: DatasetOverrides): DatasetPaths {
  return {
    dailyTran: overrides.dailyTran ?? dataFiles.dailytran,
    xref: overrides.xref ?? dataFiles.cardxref,
    account: overrides.account ?? dataFiles.acctdata,
    card: overrides.card ?? dataFiles.carddata,
    customer: overrides.customer ?? dataFiles.custdata,
    tranCatBal: overrides.tranCatBal ?? dataFiles.tcatbal,
    transaction: overrides.transaction,
  };
}

/** `OPEN OUTPUT` on a KSDS: the data set starts empty. */
export function openOutputKsds<T>(
  store: Ksds<T>,
  path: string,
): FileStatusCode {
  writeFileSync(path, "", "latin1");
  return store.openFile();
}
