import { mkdtempSync, copyFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { describe, expect, it } from "vitest";

import { dataFiles, openAccountFile, openCardXrefFile, openDailyTransactionFile } from "./datasets.js";
import { FileStatus } from "./status.js";

describe("Ksds", () => {
  it("reads a record by key and browses in key order", () => {
    const accounts = openAccountFile();
    expect(accounts.openFile()).toBe(FileStatus.ok);

    const found = accounts.read("00000000001");
    expect(found.status).toBe(FileStatus.ok);
    expect(found.record?.acctActiveStatus).toBe("Y");

    expect(accounts.read("99999999999").status).toBe(FileStatus.notFound);

    expect(accounts.startBrowse("00000000003")).toBe(FileStatus.ok);
    const next = accounts.readNext();
    expect(next.record?.acctId).toBe(3);

    const keys = accounts.toArray().map((record) => record.acctId);
    expect(keys).toEqual([...keys].sort((left, right) => left - right));
  });

  it("returns end of file after the last record", () => {
    const xrefs = openCardXrefFile();
    xrefs.openFile();
    xrefs.startBrowse();
    let count = 0;
    while (xrefs.readNext().status === FileStatus.ok) {
      count += 1;
    }
    expect(count).toBe(xrefs.toArray().length);
    expect(xrefs.readNext().status).toBe(FileStatus.endOfFile);
  });

  it("rejects duplicate keys and persists updates", () => {
    const scratch = join(mkdtempSync(join(tmpdir(), "carddemo-")), "acctdata.txt");
    copyFileSync(dataFiles.acctdata, scratch);

    const accounts = openAccountFile(scratch);
    accounts.openFile();
    const existing = accounts.read("00000000001").record!;
    expect(accounts.write(existing)).toBe(FileStatus.duplicateKey);
    expect(accounts.rewrite({ ...existing, acctCurrBal: 123.45 })).toBe(FileStatus.ok);
    accounts.save();

    const reloaded = openAccountFile(scratch);
    reloaded.openFile();
    expect(reloaded.read("00000000001").record?.acctCurrBal).toBe(123.45);
  });

  it("browses backwards from the positioned record", () => {
    const accounts = openAccountFile();
    accounts.openFile();
    const keys = accounts.toArray().map((record) => record.acctId);

    expect(accounts.startBrowse("00000000005")).toBe(FileStatus.ok);
    expect(accounts.readPrev().record?.acctId).toBe(keys[4]);
    expect(accounts.readPrev().record?.acctId).toBe(keys[3]);
    expect(accounts.readPrev().record?.acctId).toBe(keys[2]);
  });

  it("re-reads the current record when the browse changes direction", () => {
    const accounts = openAccountFile();
    accounts.openFile();
    const keys = accounts.toArray().map((record) => record.acctId);

    accounts.startBrowse();
    expect(accounts.readNext().record?.acctId).toBe(keys[0]);
    expect(accounts.readNext().record?.acctId).toBe(keys[1]);
    expect(accounts.readPrev().record?.acctId).toBe(keys[1]);
    expect(accounts.readPrev().record?.acctId).toBe(keys[0]);
    expect(accounts.readPrev().status).toBe(FileStatus.endOfFile);
    expect(accounts.readNext().record?.acctId).toBe(keys[0]);
  });

  it("reports file status 42 before the file is opened", () => {
    expect(openAccountFile().readPrev().status).toBe(FileStatus.fileNotOpen);
    expect(openAccountFile().read("00000000001").status).toBe(FileStatus.fileNotOpen);
  });
});

describe("SequentialReader", () => {
  it("walks the daily transaction file in arrival order", () => {
    const dailyTran = openDailyTransactionFile();
    expect(dailyTran.openFile()).toBe(FileStatus.ok);
    const first = dailyTran.readNext();
    expect(first.status).toBe(FileStatus.ok);
    expect(first.record?.dalytranId).toHaveLength(16);
    expect(dailyTran.toArray()).toHaveLength(300);
  });
});
