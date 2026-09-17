import type { TransactionRecord } from "@carddemo/domain";
import { describe, expect, it } from "vitest";

import {
  defaultDateParm,
  loadPostedTransactions,
  openTransactionReportFiles,
  sortTransactionsForReport,
} from "./datasets.js";
import { formatEdited } from "./picture.js";
import {
  PAGE_SIZE,
  REPORT_RECORD_LENGTH,
  TRANSACTION_HEADER_1,
  TRANSACTION_HEADER_2,
  generateTransactionReport,
  selectTransactions,
} from "./transactionReport.js";

const posted = loadPostedTransactions();
const sorted = sortTransactionsForReport(posted, defaultDateParm);
const report = generateTransactionReport(openTransactionReportFiles(sorted), defaultDateParm);

const detailLines = report.filter(
  (line) =>
    !line.startsWith("DALYREPT") &&
    !line.startsWith("Page Total") &&
    !line.startsWith("Account Total") &&
    !line.startsWith("Grand Total") &&
    line !== TRANSACTION_HEADER_1 &&
    line !== TRANSACTION_HEADER_2 &&
    line.trim().length > 0,
);

describe("CBTRN03C over the sample data", () => {
  it("writes fixed length records", () => {
    expect(new Set(report.map((line) => line.length))).toEqual(new Set([REPORT_RECORD_LENGTH]));
  });

  it("opens with the report name header and the column headings", () => {
    expect(report.slice(0, 4)).toEqual([
      "DALYREPT                              Daily Transaction Report                 " +
        "Date Range: 2022-01-01 to 2022-07-06                  ",
      " ".repeat(REPORT_RECORD_LENGTH),
      "Transaction ID   Account ID  Transaction Type   Tran Category                      " +
        "Tran Source            Amount                     ",
      "-".repeat(REPORT_RECORD_LENGTH),
    ]);
  });

  it("reports every selected transaction with its type and category descriptions", () => {
    expect(sorted).toHaveLength(300);
    expect(detailLines).toHaveLength(300);
    expect(detailLines[0]).toBe(
      "0000000058866561 00000000050 01-Purchase        0001-Regular Sales Draft           " +
        "POS TERM               183.88                     ",
    );
    expect(new Set(detailLines.map((line) => line.slice(29, 47)))).toEqual(
      new Set(["01-Purchase       ", "03-Credit         "]),
    );
    expect(new Set(detailLines.map((line) => line.slice(48, 82)))).toEqual(
      new Set(["0001-Regular Sales Draft          ", "0001-Credit to Account            "]),
    );
  });

  it("breaks pages every WS-PAGE-SIZE report lines", () => {
    const pageTotals = report.filter((line) => line.startsWith("Page Total"));
    const headers = report.filter((line) => line.startsWith("DALYREPT"));
    expect(report).toHaveLength(501);
    expect(pageTotals).toHaveLength(17);
    // One header block per page, and the first page is not preceded by a total.
    expect(headers).toHaveLength(pageTotals.length);

    const firstPageTotal = report.indexOf(pageTotals[0] as string);
    expect(firstPageTotal).toBe(4 + PAGE_SIZE - 4);
  });

  it("breaks on the card number with an account total", () => {
    const accountTotals = report.filter((line) => line.startsWith("Account Total"));
    const cards = new Set(sorted.map((transaction) => transaction.tranCardNum));
    // The break is written when a new card starts, so the last card has none.
    expect(accountTotals).toHaveLength(cards.size - 1);
  });

  it("carries page totals into the grand total at end of file", () => {
    const pageTotals = report
      .filter((line) => line.startsWith("Page Total"))
      .map((line) => Number(line.slice(97, 112).replace(/[ ,]/g, "")));
    const grandTotal = report.at(-1) as string;
    expect(grandTotal.startsWith("Grand Total")).toBe(true);

    const expected = pageTotals.reduce((total, page) => total + page, 0);
    expect(grandTotal.slice(97, 112)).toBe(formatEdited("+ZZZ,ZZZ,ZZZ.ZZ", expected));
    expect(grandTotal.slice(97, 112)).toBe("+    105,567.20");
  });

  it("renders the last page of the report", () => {
    expect(report.slice(-6).join("\n")).toMatchSnapshot();
  });
});

describe("DATEPARM selection", () => {
  const transaction = (id: string, procTs: string): TransactionRecord => ({
    ...(sorted[0] as TransactionRecord),
    tranId: id,
    tranProcTs: procTs,
  });

  it("keeps transactions inside the range, end points included", () => {
    const selection = selectTransactions(
      [
        transaction("A", "2022-01-01 00:00:00.000000"),
        transaction("B", "2022-07-06 23:59:59.000000"),
      ],
      defaultDateParm,
    );
    expect(selection.transactions.map((record) => record.tranId)).toEqual(["A", "B"]);
    expect(selection.stoppedEarly).toBe(false);
  });

  it("leaves the read loop on the first transaction outside the range", () => {
    const selection = selectTransactions(
      [
        transaction("A", "2022-06-10 19:27:53.000000"),
        transaction("B", "2022-08-01 19:27:53.000000"),
        transaction("C", "2022-06-11 19:27:53.000000"),
      ],
      defaultDateParm,
    );
    expect(selection.transactions.map((record) => record.tranId)).toEqual(["A"]);
    expect(selection.stoppedEarly).toBe(true);
  });
});
