/**
 * CBTRN02C — daily transaction posting.
 *
 * Validates every daily transaction against the cross-reference and account
 * master, posts the accepted ones to the transaction master, updates the
 * account balances and the transaction category balances, and writes the
 * rejected ones to `DALYREJS` with their validation reason.
 */

import {
  dailyTransactionCodec,
  padKey,
  type AccountRecord,
  type CardXrefRecord,
  type DailyTransactionRecord,
  type TranCatBalRecord,
  type TransactionRecord,
} from "@carddemo/domain";
import {
  FileStatus,
  SequentialWriter,
  openAccountFile,
  openCardXrefFile,
  openDailyTransactionFile,
  openTranCatBalFile,
  openTransactionFile,
  type Ksds,
} from "@carddemo/vsam";

import { createFileStatusGuard, type FileStatusGuard } from "./abend.js";
import {
  openOutputKsds,
  resolveDatasets,
  type DatasetOverrides,
} from "./files.js";
import { addMoney, cycleBalance } from "./money.js";
import { db2FormatTimestamp } from "./timestamp.js";

/** `WS-VALIDATION-FAIL-REASON` values set by `1500-VALIDATE-TRAN`. */
export const RejectReason = {
  none: 0,
  invalidCardNumber: 100,
  accountNotFound: 101,
  overlimit: 102,
  accountExpired: 103,
  accountRewriteFailed: 109,
} as const;

export type RejectReasonCode = (typeof RejectReason)[keyof typeof RejectReason];

/** Every reason except `none`, i.e. the ones that reject a transaction. */
export type RejectedReasonCode = Exclude<
  RejectReasonCode,
  typeof RejectReason.none
>;

const REJECT_REASON_DESC: Record<RejectReasonCode, string> = {
  [RejectReason.none]: "",
  [RejectReason.invalidCardNumber]: "INVALID CARD NUMBER FOUND",
  [RejectReason.accountNotFound]: "ACCOUNT RECORD NOT FOUND",
  [RejectReason.overlimit]: "OVERLIMIT TRANSACTION",
  [RejectReason.accountExpired]: "TRANSACTION RECEIVED AFTER ACCT EXPIRATION",
  [RejectReason.accountRewriteFailed]: "ACCOUNT RECORD NOT FOUND",
};

export interface PostingOptions extends DatasetOverrides {
  /** `DALYREJS` — rejected transactions with their validation trailer. */
  readonly rejects: string;
  readonly now?: () => Date;
  readonly log?: (line: string) => void;
}

export interface PostingResult {
  /** `WS-TRANSACTION-COUNT`. */
  readonly transactionCount: number;
  /** `WS-REJECT-COUNT`. */
  readonly rejectCount: number;
  /** Rejects grouped by `WS-VALIDATION-FAIL-REASON`. */
  readonly rejectsByReason: Readonly<Record<number, number>>;
  /** Category balance records created because the key was absent. */
  readonly categoryBalancesCreated: number;
  /** `RETURN-CODE`: 4 when at least one transaction was rejected. */
  readonly returnCode: number;
}

type Validation =
  | {
      readonly reason: typeof RejectReason.none;
      readonly xref: CardXrefRecord;
      readonly account: AccountRecord;
    }
  | { readonly reason: RejectedReasonCode };

export function runDailyTransactionPosting(
  options: PostingOptions,
): PostingResult {
  const paths = resolveDatasets(options);
  const log = options.log ?? ((): void => {});
  const now = options.now ?? ((): Date => new Date());
  const guard = createFileStatusGuard(log);

  const dailyTran = openDailyTransactionFile(paths.dailyTran);
  const transactions = openTransactionFile(paths.transaction);
  const xrefs = openCardXrefFile(paths.xref);
  const rejects = new SequentialWriter<DailyTransactionRecord>({
    name: "DALYREJS",
    path: options.rejects,
    codec: dailyTransactionCodec,
  });
  const accounts = openAccountFile(paths.account);
  const categoryBalances = openTranCatBalFile(paths.tranCatBal);

  log("START OF EXECUTION OF PROGRAM CBTRN02C");

  guard(
    "DALYTRAN",
    "OPEN INPUT",
    dailyTran.openFile(),
    "ERROR OPENING DALYTRAN",
  );
  guard(
    "TRANFILE",
    "OPEN OUTPUT",
    openOutputKsds(transactions, paths.transaction),
    "ERROR OPENING TRANSACTION FILE",
  );
  guard(
    "XREFFILE",
    "OPEN INPUT",
    xrefs.openFile(),
    "ERROR OPENING CROSS REF FILE",
  );
  guard(
    "DALYREJS",
    "OPEN OUTPUT",
    rejects.openFile(),
    "ERROR OPENING DALY REJECTS FILE",
  );
  guard(
    "ACCTFILE",
    "OPEN I-O",
    accounts.openFile(),
    "ERROR OPENING ACCOUNT MASTER FILE",
  );
  guard(
    "TCATBALF",
    "OPEN I-O",
    categoryBalances.openFile(),
    "ERROR OPENING TRANSACTION BALANCE FILE",
  );

  let transactionCount = 0;
  let rejectCount = 0;
  let categoryBalancesCreated = 0;
  const rejectsByReason: Record<number, number> = {};

  for (;;) {
    const read = dailyTran.readNext();
    guard("DALYTRAN", "READ", read.status, "ERROR READING DALYTRAN FILE", [
      FileStatus.ok,
      FileStatus.endOfFile,
    ]);
    const dalytran = read.record;
    if (dalytran === undefined) {
      break;
    }
    transactionCount += 1;

    const validation = validateTransaction(dalytran, xrefs, accounts);
    if (validation.reason === RejectReason.none) {
      if (
        postCategoryBalance(dalytran, validation.xref, categoryBalances, guard)
      ) {
        categoryBalancesCreated += 1;
      }
      updateAccount(dalytran, validation.account, accounts, log);
      const posted = buildTransaction(dalytran, db2FormatTimestamp(now()));
      guard(
        "TRANFILE",
        "WRITE",
        transactions.write(posted),
        "ERROR WRITING TO TRANSACTION FILE",
      );
    } else {
      rejectCount += 1;
      rejectsByReason[validation.reason] =
        (rejectsByReason[validation.reason] ?? 0) + 1;
      guard(
        "DALYREJS",
        "WRITE",
        rejects.writeLine(buildRejectRecord(dalytran, validation.reason)),
        "ERROR WRITING TO REJECTS FILE",
      );
    }
  }

  guard("DALYTRAN", "CLOSE", dailyTran.close(), "ERROR CLOSING DALYTRAN FILE");
  transactions.save();
  guard(
    "TRANFILE",
    "CLOSE",
    transactions.close(),
    "ERROR CLOSING TRANSACTION FILE",
  );
  guard("XREFFILE", "CLOSE", xrefs.close(), "ERROR CLOSING CROSS REF FILE");
  guard(
    "DALYREJS",
    "CLOSE",
    rejects.close(),
    "ERROR CLOSING DAILY REJECTS FILE",
  );
  accounts.save();
  guard("ACCTFILE", "CLOSE", accounts.close(), "ERROR CLOSING ACCOUNT FILE");
  categoryBalances.save();
  guard(
    "TCATBALF",
    "CLOSE",
    categoryBalances.close(),
    "ERROR CLOSING TRANSACTION BALANCE FILE",
  );

  log(`TRANSACTIONS PROCESSED :${padKey(transactionCount, 9)}`);
  log(`TRANSACTIONS REJECTED  :${padKey(rejectCount, 9)}`);
  log("END OF EXECUTION OF PROGRAM CBTRN02C");

  return {
    transactionCount,
    rejectCount,
    rejectsByReason,
    categoryBalancesCreated,
    returnCode: rejectCount > 0 ? 4 : 0,
  };
}

/** `1500-VALIDATE-TRAN`. */
function validateTransaction(
  dalytran: DailyTransactionRecord,
  xrefs: Ksds<CardXrefRecord>,
  accounts: Ksds<AccountRecord>,
): Validation {
  const xrefRead = xrefs.read(dalytran.dalytranCardNum);
  if (xrefRead.status !== FileStatus.ok || xrefRead.record === undefined) {
    return { reason: RejectReason.invalidCardNumber };
  }
  const xref = xrefRead.record;

  const accountRead = accounts.read(padKey(xref.xrefAcctId, 11));
  if (
    accountRead.status !== FileStatus.ok ||
    accountRead.record === undefined
  ) {
    return { reason: RejectReason.accountNotFound };
  }
  const account = accountRead.record;

  // Both checks run in CBTRN02C, so an expired account overrides an overlimit.
  let reason: RejectedReasonCode | undefined;
  const cycleTotal = cycleBalance(
    account.acctCurrCycCredit,
    account.acctCurrCycDebit,
    dalytran.dalytranAmt,
  );
  if (account.acctCreditLimit < cycleTotal) {
    reason = RejectReason.overlimit;
  }
  if (account.acctExpiraionDate < dalytran.dalytranOrigTs.slice(0, 10)) {
    reason = RejectReason.accountExpired;
  }

  return reason === undefined
    ? { reason: RejectReason.none, xref, account }
    : { reason };
}

/** `2700-UPDATE-TCATBAL`; returns true when the record had to be created. */
function postCategoryBalance(
  dalytran: DailyTransactionRecord,
  xref: CardXrefRecord,
  categoryBalances: Ksds<TranCatBalRecord>,
  guard: FileStatusGuard,
): boolean {
  const key = `${padKey(xref.xrefAcctId, 11)}${padKey(dalytran.dalytranTypeCd, 2)}${padKey(dalytran.dalytranCatCd, 4)}`;
  const existing = categoryBalances.read(key);
  guard(
    "TCATBALF",
    "READ",
    existing.status,
    "ERROR READING TRANSACTION BALANCE FILE",
    [FileStatus.ok, FileStatus.notFound],
  );

  if (
    existing.status === FileStatus.notFound ||
    existing.record === undefined
  ) {
    const created: TranCatBalRecord = {
      trancatAcctId: xref.xrefAcctId,
      trancatTypeCd: dalytran.dalytranTypeCd,
      trancatCd: dalytran.dalytranCatCd,
      tranCatBal: addMoney(0, dalytran.dalytranAmt),
    };
    guard(
      "TCATBALF",
      "WRITE",
      categoryBalances.write(created),
      "ERROR WRITING TRANSACTION BALANCE FILE",
    );
    return true;
  }

  const updated: TranCatBalRecord = {
    ...existing.record,
    tranCatBal: addMoney(existing.record.tranCatBal, dalytran.dalytranAmt),
  };
  guard(
    "TCATBALF",
    "REWRITE",
    categoryBalances.rewrite(updated),
    "ERROR REWRITING TRANSACTION BALANCE FILE",
  );
  return false;
}

/** `2800-UPDATE-ACCOUNT-REC`. */
function updateAccount(
  dalytran: DailyTransactionRecord,
  account: AccountRecord,
  accounts: Ksds<AccountRecord>,
  log: (line: string) => void,
): void {
  const amount = dalytran.dalytranAmt;
  const updated: AccountRecord = {
    ...account,
    acctCurrBal: addMoney(account.acctCurrBal, amount),
    acctCurrCycCredit:
      amount >= 0
        ? addMoney(account.acctCurrCycCredit, amount)
        : account.acctCurrCycCredit,
    acctCurrCycDebit:
      amount >= 0
        ? account.acctCurrCycDebit
        : addMoney(account.acctCurrCycDebit, amount),
  };

  if (accounts.rewrite(updated) !== FileStatus.ok) {
    log(REJECT_REASON_DESC[RejectReason.accountRewriteFailed]);
  }
}

/** `2000-POST-TRANSACTION` field moves. */
function buildTransaction(
  dalytran: DailyTransactionRecord,
  processedTimestamp: string,
): TransactionRecord {
  return {
    tranId: dalytran.dalytranId,
    tranTypeCd: dalytran.dalytranTypeCd,
    tranCatCd: dalytran.dalytranCatCd,
    tranSource: dalytran.dalytranSource,
    tranDesc: dalytran.dalytranDesc,
    tranAmt: dalytran.dalytranAmt,
    tranMerchantId: dalytran.dalytranMerchantId,
    tranMerchantName: dalytran.dalytranMerchantName,
    tranMerchantCity: dalytran.dalytranMerchantCity,
    tranMerchantZip: dalytran.dalytranMerchantZip,
    tranCardNum: dalytran.dalytranCardNum,
    tranOrigTs: dalytran.dalytranOrigTs,
    tranProcTs: processedTimestamp,
  };
}

/** `REJECT-RECORD`: the 350 byte daily transaction plus an 80 byte trailer. */
export function buildRejectRecord(
  dalytran: DailyTransactionRecord,
  reason: RejectedReasonCode,
): string {
  const trailer = `${padKey(reason, 4)}${REJECT_REASON_DESC[reason].padEnd(76, " ")}`;
  return `${dailyTransactionCodec.encode(dalytran)}${trailer}`;
}
