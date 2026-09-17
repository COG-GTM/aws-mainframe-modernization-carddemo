import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import type {
  AccountRecord,
  CardRecord,
  CardXrefRecord,
  CustomerRecord,
  TransactionRecord,
} from "@carddemo/domain";
import { afterAll, describe, expect, it } from "vitest";

import { EXPORT_BRANCH_ID, cbexport, formatExportTimestamp, writeExportFile } from "./cbexport.js";
import { UNKNOWN_RECORD_TYPE_MESSAGE, cbimport, importExportFile } from "./cbimport.js";
import {
  EXPORT_RECORD_LENGTH,
  decodeExportHeader,
  encodeExportRecord,
} from "./export-records.js";

const customer: CustomerRecord = {
  custId: 123456789,
  custFirstName: "ALICE",
  custMiddleName: "B",
  custLastName: "MORGAN",
  custAddrLine1: "1 HIGH STREET",
  custAddrLine2: "SUITE 5",
  custAddrLine3: "LONDON",
  custAddrStateCd: "NY",
  custAddrCountryCd: "USA",
  custAddrZip: "10001",
  custPhoneNum1: "(212)555-0100",
  custPhoneNum2: "(212)555-0101",
  custSsn: 111223333,
  custGovtIssuedId: "NY-DL-99231",
  custDobYyyyMmDd: "1980-04-17",
  custEftAccountId: "EFT0000001",
  custPriCardHolderInd: "Y",
  custFicoCreditScore: 742,
};

const account: AccountRecord = {
  acctId: 10000000001,
  acctActiveStatus: "Y",
  acctCurrBal: -1234.56,
  acctCreditLimit: 5000,
  acctCashCreditLimit: 1500.75,
  acctOpenDate: "2015-06-01",
  acctExpiraionDate: "2027-05-31",
  acctReissueDate: "2023-06-01",
  acctCurrCycCredit: 250.5,
  acctCurrCycDebit: -99.25,
  acctAddrZip: "10001",
  acctGroupId: "GROUP01",
};

const xref: CardXrefRecord = {
  xrefCardNum: "4111111111111111",
  xrefCustId: 123456789,
  xrefAcctId: 10000000001,
};

const transaction: TransactionRecord = {
  tranId: "TRAN000000000001",
  tranTypeCd: "01",
  tranCatCd: 5411,
  tranSource: "POS",
  tranDesc: "GROCERY PURCHASE",
  tranAmt: -84.33,
  tranMerchantId: 900000123,
  tranMerchantName: "CORNER MARKET",
  tranMerchantCity: "NEW YORK",
  tranMerchantZip: "10001",
  tranCardNum: "4111111111111111",
  tranOrigTs: "2025-01-15 10:30:00.000000",
  tranProcTs: "2025-01-15 23:59:59.000000",
};

const card: CardRecord = {
  cardNum: "4111111111111111",
  cardAcctId: 10000000001,
  cardCvvCd: 123,
  cardEmbossedName: "ALICE B MORGAN",
  cardExpiraionDate: "2027-05-31",
  cardActiveStatus: "Y",
};

const input = {
  customers: [customer],
  accounts: [account],
  xrefs: [xref],
  transactions: [transaction],
  cards: [card],
};

const timestamp = "2025-01-15 10:30:00.00";
const tempDir = mkdtempSync(join(tmpdir(), "carddemo-export-"));

afterAll(() => {
  rmSync(tempDir, { recursive: true, force: true });
});

describe("cbexport", () => {
  it("writes one 500 byte record per input record, in program order", () => {
    const result = cbexport(input, { timestamp });

    expect(result.lines).toHaveLength(5);
    for (const line of result.lines) {
      expect(line).toHaveLength(EXPORT_RECORD_LENGTH);
    }
    expect(result.lines.map((line) => decodeExportHeader(line).recType)).toEqual([
      "C",
      "A",
      "X",
      "T",
      "D",
    ]);
    expect(result.lines.map((line) => decodeExportHeader(line).sequenceNum)).toEqual([
      1, 2, 3, 4, 5,
    ]);
    expect(decodeExportHeader(result.lines[0] as string)).toEqual({
      recType: "C",
      timestamp,
      sequenceNum: 1,
      branchId: EXPORT_BRANCH_ID,
      regionCode: "NORTH",
    });
    expect(result.statistics).toEqual({
      customersExported: 1,
      accountsExported: 1,
      xrefsExported: 1,
      transactionsExported: 1,
      cardsExported: 1,
      totalExported: 5,
    });
  });

  it("formats the timestamp the way 1050-GENERATE-TIMESTAMP does", () => {
    expect(formatExportTimestamp(new Date(2025, 0, 5, 9, 8, 7))).toBe(
      "2025-01-05 09:08:07.00".padEnd(26, " "),
    );
  });
});

describe("cbimport", () => {
  it("round-trips every record type through the export layout", () => {
    const result = cbimport(cbexport(input, { timestamp }).lines);

    expect(result.customers).toEqual([customer]);
    expect(result.accounts).toEqual([account]);
    expect(result.xrefs).toEqual([xref]);
    expect(result.transactions).toEqual([transaction]);
    expect(result.cards).toEqual([card]);
    expect(result.statistics.totalRecordsRead).toBe(5);
    expect(result.errors).toHaveLength(0);
  });

  it("writes an error record for an unknown record type", () => {
    const unknown = encodeExportRecord({
      recType: "unknown",
      header: { recType: "Z", timestamp, sequenceNum: 42, branchId: "0001", regionCode: "NORTH" },
    });
    const result = cbimport([unknown], { timestamp });

    expect(result.statistics.unknownRecordTypeCount).toBe(1);
    expect(result.errors[0]).toHaveLength(132);
    expect(result.errors[0]).toContain(`|Z|0000042|${UNKNOWN_RECORD_TYPE_MESSAGE}`);
  });

  it("reads a RECFM=F export data set", () => {
    const path = join(tempDir, "export.dat");
    writeExportFile(path, cbexport(input, { timestamp }));

    expect(importExportFile(path).statistics.totalRecordsRead).toBe(5);
  });
});
