import { copyFileSync, mkdtempSync, readFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  dataFiles,
  openAccountFile,
  openTranCatBalFile,
  openTransactionFile,
} from "@carddemo/vsam";
import { beforeAll, describe, expect, it } from "vitest";

import type { AbendError } from "./abend.js";
import {
  RejectReason,
  runDailyTransactionPosting,
  type PostingResult,
} from "./cbtrn02c.js";

/** Scratch copies of the datasets CBTRN02C updates, taken from `app/data/ASCII`. */
interface Run {
  readonly result: PostingResult;
  readonly accountPath: string;
  readonly tranCatBalPath: string;
  readonly transactionPath: string;
  readonly rejectsPath: string;
}

function post(): Run {
  const dir = mkdtempSync(join(tmpdir(), "carddemo-posting-"));
  const accountPath = join(dir, "acctdata.txt");
  const tranCatBalPath = join(dir, "tcatbal.txt");
  const transactionPath = join(dir, "trandata.txt");
  const rejectsPath = join(dir, "dalyrejs.txt");
  copyFileSync(dataFiles.acctdata, accountPath);
  copyFileSync(dataFiles.tcatbal, tranCatBalPath);

  const result = runDailyTransactionPosting({
    account: accountPath,
    tranCatBal: tranCatBalPath,
    transaction: transactionPath,
    rejects: rejectsPath,
    now: () => new Date(2022, 5, 11, 1, 2, 3, 40),
  });

  return { result, accountPath, tranCatBalPath, transactionPath, rejectsPath };
}

describe("CBTRN02C", () => {
  let run: Run;

  beforeAll(() => {
    run = post();
  });

  it("posts the accepted daily transactions and rejects the rest", () => {
    expect(run.result.transactionCount).toBe(300);
    expect(run.result.rejectCount).toBe(38);
    expect(run.result.rejectsByReason).toEqual({
      [RejectReason.overlimit]: 38,
    });
    expect(run.result.returnCode).toBe(4);
  });

  it("writes the posted transactions to the transaction master", () => {
    const transactions = openTransactionFile(run.transactionPath);
    transactions.openFile();
    expect(transactions.toArray()).toHaveLength(262);

    const posted = transactions.read("0000000000683580").record;
    expect(posted).toMatchObject({
      tranTypeCd: "01",
      tranCatCd: 1,
      tranSource: "POS TERM",
      tranAmt: 504.77,
      tranCardNum: "4859452612877065",
      tranOrigTs: "2022-06-10 19:27:53.000000",
      tranProcTs: "2022-06-11-01.02.03.040000",
    });
  });

  it("updates the account balance and the cycle credit and debit totals", () => {
    const accounts = openAccountFile(run.accountPath);
    accounts.openFile();
    const account = accounts.read("00000000001").record;
    expect(account).toMatchObject({
      acctCurrBal: 1288.1,
      acctCurrCycCredit: 1164.87,
      acctCurrCycDebit: -70.77,
    });
  });

  it("updates existing category balances and creates the missing ones", () => {
    const balances = openTranCatBalFile(run.tranCatBalPath);
    balances.openFile();
    expect(run.result.categoryBalancesCreated).toBe(50);
    expect(balances.toArray()).toHaveLength(100);
    expect(balances.read("00000000007010001").record?.tranCatBal).toBe(1287.09);
    expect(balances.read("00000000020030001").record?.tranCatBal).toBe(-919);
  });

  it("writes rejects with the daily transaction and its validation trailer", () => {
    const lines = readFileSync(run.rejectsPath, "latin1")
      .split("\n")
      .filter(Boolean);
    expect(lines).toHaveLength(38);

    const [first = ""] = lines;
    expect(first).toHaveLength(430);
    expect(first.slice(0, 16)).toBe("0000000040455859");
    expect(first.slice(350, 354)).toBe("0102");
    expect(first.slice(354).trim()).toBe("OVERLIMIT TRANSACTION");
  });

  it("abends with file status 35 when a dataset is missing", () => {
    const dir = mkdtempSync(join(tmpdir(), "carddemo-posting-"));
    expect(() =>
      runDailyTransactionPosting({
        account: join(dir, "absent.txt"),
        transaction: join(dir, "trandata.txt"),
        rejects: join(dir, "dalyrejs.txt"),
      }),
    ).toThrowError(
      expect.objectContaining({
        name: "AbendError",
        status: "35",
        abcode: 999,
      }) as AbendError,
    );
  });
});
