import { mkdtempSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { describe, expect, it } from "vitest";

import { buildTrnxFile, loadPostedTransactions, openStatementIo } from "./datasets.js";
import {
  HTML_RECORD_LENGTH,
  STATEMENT_RECORD_LENGTH,
  generateStatements,
} from "./statement.js";

const workDir = mkdtempSync(join(tmpdir(), "carddemo-statements-"));
const trnxPath = join(workDir, "trnxfile.txt");
const transactions = buildTrnxFile(loadPostedTransactions(), trnxPath);
const output = generateStatements(openStatementIo(trnxPath));

/** Reads back a `Z(9).99-` edited amount. */
function parseTrailingSign(edited: string): number {
  const digits = Number(edited.replace(/[ -]/g, ""));
  return edited.trimEnd().endsWith("-") ? -digits : digits;
}

const startLines = output.statement.filter((line) => line.includes("START OF STATEMENT"));
const endLines = output.statement.filter((line) => line.includes("END OF STATEMENT"));

describe("CBSTM03A over the sample data", () => {
  it("writes fixed length records to both files", () => {
    expect(new Set(output.statement.map((line) => line.length))).toEqual(
      new Set([STATEMENT_RECORD_LENGTH]),
    );
    expect(new Set(output.html.map((line) => line.length))).toEqual(new Set([HTML_RECORD_LENGTH]));
  });

  it("produces one statement per cross reference record", () => {
    expect(startLines).toHaveLength(50);
    expect(endLines).toHaveLength(50);
    expect(output.html.filter((line) => line.startsWith("<!DOCTYPE html>"))).toHaveLength(50);
  });

  it("writes every transaction of the card exactly once", () => {
    const tranLines = output.statement.filter((line) => /^\d{16} /.test(line));
    expect(tranLines).toHaveLength(transactions.length);
    expect(transactions).toHaveLength(300);
  });

  it("totals the transactions of each statement", () => {
    const firstCard = transactions[0]?.trnxCardNum ?? "";
    const expected = transactions
      .filter((record) => record.trnxCardNum === firstCard)
      .reduce((total, record) => total + record.trnxAmt, 0);
    const totalLine = output.statement.find((line) => line.startsWith("Total EXP:")) as string;
    expect(parseTrailingSign(totalLine.slice(67))).toBeCloseTo(expected, 2);
  });

  it("renders the first plain text statement", () => {
    const first = output.statement.slice(0, output.statement.indexOf(endLines[0] as string) + 1);
    expect(first.join("\n")).toMatchSnapshot();
  });

  it("renders the first HTML statement", () => {
    const end = output.html.indexOf("</html>".padEnd(HTML_RECORD_LENGTH, " "));
    expect(output.html.slice(0, end + 1).join("\n")).toMatchSnapshot();
  });
});
