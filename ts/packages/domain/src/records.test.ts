import { readFileSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

import {
  accountCodec,
  cardCodec,
  cardXrefCodec,
  customerCodec,
  dailyTransactionCodec,
  discGroupCodec,
  tranCatBalCodec,
  tranCatCodec,
  tranTypeCodec,
  type RecordCodec,
} from "./records.js";

const repoRoot = resolve(dirname(fileURLToPath(import.meta.url)), "..", "..", "..", "..");
const dataDir = join(repoRoot, "app", "data", "ASCII");

function readLines(file: string): string[] {
  return readFileSync(join(dataDir, file), "latin1")
    .split("\n")
    .filter((line) => line.trim().length > 0);
}

const cases: ReadonlyArray<[string, string, RecordCodec<never>]> = [
  ["acctdata.txt", "ACCOUNT-RECORD", accountCodec as RecordCodec<never>],
  ["custdata.txt", "CUSTOMER-RECORD", customerCodec as RecordCodec<never>],
  ["carddata.txt", "CARD-RECORD", cardCodec as RecordCodec<never>],
  ["cardxref.txt", "CARD-XREF-RECORD", cardXrefCodec as RecordCodec<never>],
  ["dailytran.txt", "DALYTRAN-RECORD", dailyTransactionCodec as RecordCodec<never>],
  ["discgrp.txt", "DIS-GROUP-RECORD", discGroupCodec as RecordCodec<never>],
  ["tcatbal.txt", "TRAN-CAT-BAL-RECORD", tranCatBalCodec as RecordCodec<never>],
  ["trancatg.txt", "TRAN-CAT-RECORD", tranCatCodec as RecordCodec<never>],
  ["trantype.txt", "TRAN-TYPE-RECORD", tranTypeCodec as RecordCodec<never>],
];

describe.each(cases)("%s", (file, layoutName, codec) => {
  it(`decodes every record with the ${layoutName} layout`, () => {
    const lines = readLines(file);
    expect(lines.length).toBeGreaterThan(0);
    for (const line of lines) {
      expect(() => codec.decode(line)).not.toThrow();
    }
  });

  it("re-encodes records byte-for-byte up to the trailing filler", () => {
    const named = codec.layout.fields.filter((candidate) => candidate.name !== undefined);
    const last = named.at(-1)!;
    const dataLength = last.offset + last.width;

    for (const line of readLines(file)) {
      const encoded = codec.encode(codec.decode(line));
      expect(encoded).toHaveLength(codec.layout.recordLength);
      expect(encoded.slice(0, dataLength)).toBe(line.padEnd(dataLength, " ").slice(0, dataLength));
    }
  });
});

describe("account records", () => {
  it("reads signed money fields from the sample master file", () => {
    const [first] = readLines("acctdata.txt");
    const account = accountCodec.decode(first as string);
    expect(account.acctId).toBe(1);
    expect(account.acctActiveStatus).toBe("Y");
    expect(account.acctCurrBal).toBe(194);
    expect(account.acctCreditLimit).toBe(2020);
    expect(account.acctOpenDate).toBe("2014-11-20");
    expect(account.acctExpiraionDate).toBe("2025-05-20");
  });
});
