import { mkdtempSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { describe, expect, it } from "vitest";

import { runDailyTransactionRead } from "./cbtrn01c.js";

function emptyTransactionFile(): string {
  const path = join(
    mkdtempSync(join(tmpdir(), "carddemo-read-")),
    "trandata.txt",
  );
  writeFileSync(path, "", "latin1");
  return path;
}

describe("CBTRN01C", () => {
  it("verifies every daily transaction against the xref and account files", () => {
    const lines: string[] = [];
    const result = runDailyTransactionRead({
      transaction: emptyTransactionFile(),
      log: (line) => lines.push(line),
    });

    expect(result.transactionCount).toBe(300);
    expect(result.xrefNotFoundCount).toBe(0);
    expect(result.accountNotFoundCount).toBe(0);
    // The last record is looked up again on the end-of-file pass, as in COBOL.
    expect(result.verifiedCount).toBe(301);

    expect(lines[0]).toBe("START OF EXECUTION OF PROGRAM CBTRN01C");
    expect(lines).toContain("ACCOUNT ID : 00000000007");
    expect(lines.at(-1)).toBe("END OF EXECUTION OF PROGRAM CBTRN01C");
  });

  it("reports unverified cards and missing accounts", () => {
    const dir = mkdtempSync(join(tmpdir(), "carddemo-read-"));
    const dailyTranPath = join(dir, "dailytran.txt");
    writeFileSync(
      dailyTranPath,
      `${"9".repeat(16)}${" ".repeat(334)}\n`,
      "latin1",
    );

    const lines: string[] = [];
    const result = runDailyTransactionRead({
      dailyTran: dailyTranPath,
      transaction: emptyTransactionFile(),
      log: (line) => lines.push(line),
    });

    expect(result.transactionCount).toBe(1);
    expect(result.xrefNotFoundCount).toBe(2);
    expect(lines).toContain("INVALID CARD NUMBER FOR XREF");
  });

  it("abends when the daily transaction file cannot be opened", () => {
    const dir = mkdtempSync(join(tmpdir(), "carddemo-read-"));
    expect(() =>
      runDailyTransactionRead({
        dailyTran: join(dir, "absent.txt"),
        transaction: emptyTransactionFile(),
      }),
    ).toThrowError(/DALYTRAN failed, FILE STATUS IS: NNNN0035/);
  });
});
