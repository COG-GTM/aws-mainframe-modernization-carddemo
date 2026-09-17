/**
 * CBTRN01C — daily transaction validation / read.
 *
 * Reads the daily transaction file sequentially and, for every record, verifies
 * the card against the cross-reference file and reads the account it points at.
 * The program reports its findings through `DISPLAY` only; no file is updated.
 */

import {
  dailyTransactionCodec,
  padKey,
  type CardXrefRecord,
  type DailyTransactionRecord,
} from "@carddemo/domain";
import {
  FileStatus,
  openAccountFile,
  openCardFile,
  openCardXrefFile,
  openCustomerFile,
  openDailyTransactionFile,
  openTransactionFile,
  type FileStatusCode,
} from "@carddemo/vsam";

import { createFileStatusGuard } from "./abend.js";
import { resolveDatasets, type DatasetOverrides } from "./files.js";

export interface DailyTransactionReadOptions extends DatasetOverrides {
  readonly log?: (line: string) => void;
}

export interface DailyTransactionReadResult {
  /** Records read from `DALYTRAN`. */
  readonly transactionCount: number;
  /** Cards resolved through `XREFFILE` whose account was also found. */
  readonly verifiedCount: number;
  /** `INVALID CARD NUMBER FOR XREF`. */
  readonly xrefNotFoundCount: number;
  /** `INVALID ACCOUNT NUMBER FOUND`. */
  readonly accountNotFoundCount: number;
}

export function runDailyTransactionRead(
  options: DailyTransactionReadOptions,
): DailyTransactionReadResult {
  const paths = resolveDatasets(options);
  const log = options.log ?? ((): void => {});
  const guard = createFileStatusGuard(log);

  const dailyTran = openDailyTransactionFile(paths.dailyTran);
  const customers = openCustomerFile(paths.customer);
  const xrefs = openCardXrefFile(paths.xref);
  const cards = openCardFile(paths.card);
  const accounts = openAccountFile(paths.account);
  const transactions = openTransactionFile(paths.transaction);

  log("START OF EXECUTION OF PROGRAM CBTRN01C");

  guard(
    "DALYTRAN",
    "OPEN INPUT",
    dailyTran.openFile(),
    "ERROR OPENING DAILY TRANSACTION FILE",
  );
  guard(
    "CUSTFILE",
    "OPEN INPUT",
    customers.openFile(),
    "ERROR OPENING CUSTOMER FILE",
  );
  guard(
    "XREFFILE",
    "OPEN INPUT",
    xrefs.openFile(),
    "ERROR OPENING CROSS REF FILE",
  );
  guard("CARDFILE", "OPEN INPUT", cards.openFile(), "ERROR OPENING CARD FILE");
  guard(
    "ACCTFILE",
    "OPEN INPUT",
    accounts.openFile(),
    "ERROR OPENING ACCOUNT FILE",
  );
  guard(
    "TRANFILE",
    "OPEN INPUT",
    transactions.openFile(),
    "ERROR OPENING TRANSACTION FILE",
  );

  let transactionCount = 0;
  let verifiedCount = 0;
  let xrefNotFoundCount = 0;
  let accountNotFoundCount = 0;
  let current: DailyTransactionRecord | undefined;
  let endOfFile = false;

  while (!endOfFile) {
    const read = dailyTran.readNext();
    guard(
      "DALYTRAN",
      "READ",
      read.status,
      "ERROR READING DAILY TRANSACTION FILE",
      [FileStatus.ok, FileStatus.endOfFile],
    );
    if (read.record === undefined) {
      endOfFile = true;
    } else {
      current = read.record;
      transactionCount += 1;
      log(dailyTransactionCodec.encode(read.record));
    }

    // The lookups sit outside the end-of-file check in CBTRN01C, so the last
    // record read is looked up a second time on the end-of-file pass.
    if (current === undefined) {
      continue;
    }

    const xref = lookupXref(xrefs.read(current.dalytranCardNum), log);
    if (xref === undefined) {
      xrefNotFoundCount += 1;
      log(
        `CARD NUMBER ${current.dalytranCardNum} COULD NOT BE VERIFIED.` +
          ` SKIPPING TRANSACTION ID-${current.dalytranId}`,
      );
      continue;
    }

    const account = accounts.read(padKey(xref.xrefAcctId, 11));
    if (account.status === FileStatus.ok) {
      log("SUCCESSFUL READ OF ACCOUNT FILE");
      verifiedCount += 1;
    } else {
      log("INVALID ACCOUNT NUMBER FOUND");
      log(`ACCOUNT ${padKey(xref.xrefAcctId, 11)} NOT FOUND`);
      accountNotFoundCount += 1;
    }
  }

  guard("DALYTRAN", "CLOSE", dailyTran.close(), "ERROR CLOSING CUSTOMER FILE");
  guard("CUSTFILE", "CLOSE", customers.close(), "ERROR CLOSING CUSTOMER FILE");
  guard("XREFFILE", "CLOSE", xrefs.close(), "ERROR CLOSING CROSS REF FILE");
  guard("CARDFILE", "CLOSE", cards.close(), "ERROR CLOSING CARD FILE");
  guard("ACCTFILE", "CLOSE", accounts.close(), "ERROR CLOSING ACCOUNT FILE");
  guard(
    "TRANFILE",
    "CLOSE",
    transactions.close(),
    "ERROR CLOSING TRANSACTION FILE",
  );

  log("END OF EXECUTION OF PROGRAM CBTRN01C");

  return {
    transactionCount,
    verifiedCount,
    xrefNotFoundCount,
    accountNotFoundCount,
  };
}

function lookupXref(
  result: { readonly status: FileStatusCode; readonly record?: CardXrefRecord },
  log: (line: string) => void,
): CardXrefRecord | undefined {
  if (result.status !== FileStatus.ok || result.record === undefined) {
    log("INVALID CARD NUMBER FOR XREF");
    return undefined;
  }
  log("SUCCESSFUL READ OF XREF");
  log(`CARD NUMBER: ${result.record.xrefCardNum}`);
  log(`ACCOUNT ID : ${padKey(result.record.xrefAcctId, 11)}`);
  log(`CUSTOMER ID: ${padKey(result.record.xrefCustId, 9)}`);
  return result.record;
}
