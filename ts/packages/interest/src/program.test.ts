import { copyFileSync, mkdtempSync, readFileSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  accountCodec,
  discGroupCodec,
  tranCatBalCodec,
  transactionCodec,
  type AccountRecord,
  type DiscGroupRecord,
  type TranCatBalRecord,
  type TransactionRecord,
} from "@carddemo/domain";
import { dataFiles } from "@carddemo/vsam";
import { beforeEach, describe, expect, it } from "vitest";

import { AbendError, runInterestCalculation, type InterestSummary } from "./program.js";

const PARM_DATE = "2022071800";
const CLOCK = new Date(2022, 6, 18, 9, 30, 15, 250);

let workDir: string;

function readLines(path: string): string[] {
  return readFileSync(path, "latin1")
    .split("\n")
    .filter((line) => line.trim().length > 0);
}

function sampleRecords<T>(file: string, decode: (line: string) => T): T[] {
  return readLines(file).map(decode);
}

function writeDataset<T>(name: string, records: readonly T[], encode: (record: T) => string): string {
  const path = join(workDir, name);
  writeFileSync(path, `${records.map(encode).join("\n")}\n`, "latin1");
  return path;
}

function scratchAccountFile(overrides: Partial<AccountRecord> = {}): string {
  if (Object.keys(overrides).length === 0) {
    const path = join(workDir, "acctdata.txt");
    copyFileSync(dataFiles.acctdata, path);
    return path;
  }
  const accounts = sampleRecords(dataFiles.acctdata, accountCodec.decode).map((account) => ({
    ...account,
    ...overrides,
  }));
  return writeDataset("acctdata.txt", accounts, accountCodec.encode);
}

function run(
  overrides: {
    accountPath?: string;
    tcatbalPath?: string;
    discgrpPath?: string;
    display?: (line: string) => void;
  } = {},
): InterestSummary {
  return runInterestCalculation({
    parmDate: PARM_DATE,
    accountPath: overrides.accountPath ?? scratchAccountFile(),
    transactionPath: join(workDir, "systran.txt"),
    now: () => CLOCK,
    display: overrides.display ?? ((): void => {}),
    ...(overrides.tcatbalPath === undefined ? {} : { tcatbalPath: overrides.tcatbalPath }),
    ...(overrides.discgrpPath === undefined ? {} : { discgrpPath: overrides.discgrpPath }),
  });
}

function writtenTransactions(): TransactionRecord[] {
  return sampleRecords(join(workDir, "systran.txt"), transactionCodec.decode);
}

beforeEach(() => {
  workDir = mkdtempSync(join(tmpdir(), "carddemo-interest-"));
});

describe("CBACT04C against the sample data", () => {
  it("writes one interest transaction per category balance", () => {
    const messages: string[] = [];
    const summary = run({ display: (line) => messages.push(line) });

    expect(summary.recordCount).toBe(readLines(dataFiles.tcatbal).length);
    expect(summary.transactionCount).toBe(summary.recordCount);

    const transactions = writtenTransactions();
    expect(transactions).toHaveLength(summary.transactionCount);

    const [first] = transactions;
    expect(first?.tranId).toBe(`${PARM_DATE}000001`);
    expect(first?.tranTypeCd).toBe("01");
    expect(first?.tranCatCd).toBe(5);
    expect(first?.tranSource).toBe("System");
    expect(first?.tranDesc).toBe("Int. for a/c 00000000001");
    expect(first?.tranCardNum).toBe("9680294154603697");
    expect(first?.tranOrigTs).toBe("2022-07-18-09.30.15.250000");
    expect(first?.tranProcTs).toBe(first?.tranOrigTs);
    // Every sampled category balance is zero, so no interest accrues.
    expect(transactions.every((transaction) => transaction.tranAmt === 0)).toBe(true);
    // The sample account master carries the group id in ACCT-ADDR-ZIP, leaving
    // ACCT-GROUP-ID blank, so every lookup takes the DEFAULT group path.
    expect(messages).toContain("DISCLOSURE GROUP RECORD MISSING");
  });

  it("leaves the last account unposted, as the COBOL loop does", () => {
    const accountPath = scratchAccountFile();
    const summary = run({ accountPath });

    // 1050-UPDATE-ACCOUNT runs when the account changes; the END-OF-FILE branch
    // of the driving PERFORM is unreachable, so the final account is skipped.
    expect(summary.accountsUpdated).toBe(summary.recordCount - 1);
    expect(summary.totalInterest).toBe(0);
    expect(readLines(accountPath)).toHaveLength(readLines(dataFiles.acctdata).length);
  });

  it("falls back to the DEFAULT disclosure group when the account group is missing", () => {
    const messages: string[] = [];
    const accountPath = scratchAccountFile({ acctGroupId: "NOSUCHGRP" });
    const balances = sampleRecords(dataFiles.tcatbal, tranCatBalCodec.decode).map((balance) => ({
      ...balance,
      tranCatBal: 1200,
    }));

    const summary = run({
      accountPath,
      tcatbalPath: writeDataset("tcatbal.txt", balances, tranCatBalCodec.encode),
      display: (line) => messages.push(line),
    });

    expect(messages).toContain("DISCLOSURE GROUP RECORD MISSING");
    expect(messages).toContain("TRY WITH DEFAULT GROUP CODE");
    // DEFAULT / 01 / 0001 carries the same 15.00% as the account's own group.
    expect(writtenTransactions().every((transaction) => transaction.tranAmt === 15)).toBe(true);
    expect(summary.totalInterest).toBe(15 * (summary.recordCount - 1));
  });

  it("abends when the DEFAULT disclosure group is missing too", () => {
    const groups = sampleRecords(dataFiles.discgrp, discGroupCodec.decode).filter(
      (group: DiscGroupRecord) => group.disAcctGroupId !== "DEFAULT",
    );

    expect(() =>
      run({
        accountPath: scratchAccountFile({ acctGroupId: "NOSUCHGRP" }),
        discgrpPath: writeDataset("discgrp.txt", groups, discGroupCodec.encode),
      }),
    ).toThrow(AbendError);
  });

  it("abends when the account master has no record for a category balance", () => {
    const balances: TranCatBalRecord[] = [
      ...sampleRecords(dataFiles.tcatbal, tranCatBalCodec.decode),
      { trancatAcctId: 99999999999, trancatTypeCd: "01", trancatCd: 1, tranCatBal: 0 },
    ];

    expect(() =>
      run({ tcatbalPath: writeDataset("tcatbal.txt", balances, tranCatBalCodec.encode) }),
    ).toThrow(AbendError);
  });
});

describe("interest posting", () => {
  it("truncates each monthly amount and posts the accumulated interest", () => {
    // Category 0001 of group A000000000 carries 15.00%: 6.00 accrues exactly
    // 0.075 a month, which truncates to 0.07 rather than rounding to 0.08.
    const balances: TranCatBalRecord[] = [
      { trancatAcctId: 1, trancatTypeCd: "01", trancatCd: 1, tranCatBal: 6 },
      { trancatAcctId: 1, trancatTypeCd: "01", trancatCd: 2, tranCatBal: 6 },
      { trancatAcctId: 1, trancatTypeCd: "01", trancatCd: 3, tranCatBal: 6 },
      { trancatAcctId: 2, trancatTypeCd: "01", trancatCd: 1, tranCatBal: 0 },
    ];
    const messages: string[] = [];
    const accountPath = scratchAccountFile({ acctGroupId: "A000000000" });
    const balance = sampleRecords(dataFiles.acctdata, accountCodec.decode)[0] as AccountRecord;

    const summary = run({
      accountPath,
      tcatbalPath: writeDataset("tcatbal.txt", balances, tranCatBalCodec.encode),
      display: (line) => messages.push(line),
    });

    expect(messages).not.toContain("DISCLOSURE GROUP RECORD MISSING");

    // Categories 0002 and 0003 of group A000000000 carry 25.00%: 6.00 accrues
    // 0.125 a month, truncated to 0.12.
    expect(writtenTransactions().map((transaction) => transaction.tranAmt)).toEqual([
      0.07, 0.12, 0.12, 0,
    ]);
    expect(summary.totalInterest).toBe(0.31);

    const posted = sampleRecords(accountPath, accountCodec.decode)[0] as AccountRecord;
    expect(posted.acctCurrBal).toBe(balance.acctCurrBal + 0.31);
    expect(posted.acctCurrCycCredit).toBe(0);
    expect(posted.acctCurrCycDebit).toBe(0);
  });
});
