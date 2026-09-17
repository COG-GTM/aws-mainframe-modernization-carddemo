/**
 * The files `COACTVWC` and `COACTUPC` read: `ACCTDAT`, `CUSTDAT` and the
 * account path over the card cross reference (`CXACAIX`).
 */

import type { AccountRecord, CardXrefRecord, CustomerRecord } from "@carddemo/domain";
import {
  FileStatus,
  FileStatusError,
  openAccountFile,
  openCardXrefFile,
  openCustomerFile,
  type FileStatusCode,
  type IoResult,
  type Ksds,
} from "@carddemo/vsam";

export const ACCT_FILE_NAME = "ACCTDAT";
export const CUST_FILE_NAME = "CUSTDAT";
export const XREF_ACCT_PATH_NAME = "CXACAIX";

export interface AccountFiles {
  readonly accounts: Ksds<AccountRecord>;
  readonly customers: Ksds<CustomerRecord>;
  readonly cardXrefs: Ksds<CardXrefRecord>;
}

/** Opens the three files with the ASCII data sets under `app/data/ASCII`. */
export function openAccountFiles(): AccountFiles {
  const files: AccountFiles = {
    accounts: openAccountFile(),
    customers: openCustomerFile(),
    cardXrefs: openCardXrefFile(),
  };
  open(files.accounts, ACCT_FILE_NAME);
  open(files.customers, CUST_FILE_NAME);
  open(files.cardXrefs, XREF_ACCT_PATH_NAME);
  return files;
}

/**
 * A file that will not open is the `0000-*-OPEN` abend in the COBOL: the
 * transaction never reaches the map, so the port throws instead.
 */
function open<T>(file: Ksds<T>, name: string): void {
  const status = file.openFile();
  if (status !== FileStatus.ok) {
    throw new FileStatusError(name, status, "OPEN");
  }
}

export function closeAccountFiles(files: AccountFiles): void {
  files.accounts.close();
  files.customers.close();
  files.cardXrefs.close();
}

/**
 * `9200-GETCARDXREF-BYACCT`: reads the cross reference through its account
 * path, whose key is the account id rather than the card number.
 */
export function readCardXrefByAccount(
  files: AccountFiles,
  acctId: number,
): IoResult<CardXrefRecord> {
  const match = files.cardXrefs.toArray().find((xref) => xref.xrefAcctId === acctId);
  return match === undefined
    ? { status: FileStatus.notFound }
    : { status: FileStatus.ok, record: match };
}

/** `9300-GETACCTDATA-BYACCT`. */
export function readAccount(files: AccountFiles, acctId: number): IoResult<AccountRecord> {
  return files.accounts.read(String(acctId).padStart(11, "0"));
}

/** `9400-GETCUSTDATA-BYCUST`. */
export function readCustomer(files: AccountFiles, custId: number): IoResult<CustomerRecord> {
  return files.customers.read(String(custId).padStart(9, "0"));
}

export function rewriteAccount(files: AccountFiles, record: AccountRecord): FileStatusCode {
  return files.accounts.rewrite(record);
}

export function rewriteCustomer(files: AccountFiles, record: CustomerRecord): FileStatusCode {
  return files.customers.rewrite(record);
}
