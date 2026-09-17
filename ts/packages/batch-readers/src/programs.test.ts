import { mkdtempSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { describe, expect, it } from "vitest";

import { runCbact01c } from "./cbact01c.js";
import { runCbact02c } from "./cbact02c.js";
import { runCbact03c } from "./cbact03c.js";
import { runCbcus01c } from "./cbcus01c.js";
import { formatIoStatus } from "./io-status.js";
import { AbendError, type ProgramResult } from "./program.js";

interface Run {
  readonly result: ProgramResult;
  readonly lines: string[];
}

function capture(
  run: (options: { path?: string; display: (line: string) => void }) => ProgramResult,
  path?: string,
): Run {
  const lines: string[] = [];
  const display = (line: string): void => {
    lines.push(line);
  };
  const result = path === undefined ? run({ display }) : run({ path, display });
  return { result, lines };
}

describe("CBACT01C", () => {
  it("displays every account in key order with its field breakdown", () => {
    const { result, lines } = capture(runCbact01c);

    expect(result.recordsRead).toBe(50);
    expect(lines[0]).toBe("START OF EXECUTION OF PROGRAM CBACT01C");
    expect(lines.at(-1)).toBe("END OF EXECUTION OF PROGRAM CBACT01C");
    expect(lines.slice(1, 16)).toEqual([
      "ACCT-ID                 :00000000001",
      "ACCT-ACTIVE-STATUS      :Y",
      "ACCT-CURR-BAL           :00000001940{",
      "ACCT-CREDIT-LIMIT       :00000020200{",
      "ACCT-CASH-CREDIT-LIMIT  :00000010200{",
      "ACCT-OPEN-DATE          :2014-11-20",
      "ACCT-EXPIRAION-DATE     :2025-05-20",
      "ACCT-REISSUE-DATE       :2025-05-20",
      "ACCT-CURR-CYC-CREDIT    :00000000000{",
      "ACCT-CURR-CYC-DEBIT     :00000000000{",
      "ACCT-GROUP-ID           :          ",
      "-------------------------------------------------",
      "VBRC-REC1:00000000001Y",
      "VBRC-REC2:0000000000100000001940{00000020200{2025",
      "00000000001Y00000001940{00000020200{00000010200{2014-11-202025-05-202025-05-2000000000000{00000000000{A000000000".padEnd(
        300,
        " ",
      ),
    ]);

    const groupRecords = lines.filter((line) => line.length === 300);
    expect(groupRecords).toHaveLength(50);
    expect(groupRecords.at(-1)?.slice(0, 12)).toBe("00000000050Y");
  });
});

describe("CBACT02C", () => {
  it("displays one line per card record", () => {
    const { result, lines } = capture(runCbact02c);

    expect(result.recordsRead).toBe(50);
    expect(lines).toHaveLength(52);
    expect(lines[0]).toBe("START OF EXECUTION OF PROGRAM CBACT02C");
    expect(lines[1]).toHaveLength(150);
    expect(lines[1]?.slice(0, 16)).toBe("0500024453765740");
    expect(lines[1]?.slice(16, 27)).toBe("00000000050");
    expect(lines[1]?.slice(27, 30)).toBe("747");
    expect(lines.at(-1)).toBe("END OF EXECUTION OF PROGRAM CBACT02C");
  });
});

describe("CBACT03C", () => {
  it("displays each cross reference record twice", () => {
    const { result, lines } = capture(runCbact03c);

    expect(result.recordsRead).toBe(50);
    expect(lines).toHaveLength(102);
    expect(lines[1]).toBe(lines[2]);
    expect(lines[1]).toHaveLength(50);
    expect(lines[1]?.slice(0, 16)).toBe("0500024453765740");
    expect(lines[1]?.slice(16, 25)).toBe("000000050");
    expect(lines[1]?.slice(25, 36)).toBe("00000000050");
  });
});

describe("CBCUS01C", () => {
  it("displays each customer record twice", () => {
    const { result, lines } = capture(runCbcus01c);

    expect(result.recordsRead).toBe(50);
    expect(lines).toHaveLength(102);
    expect(lines[1]).toBe(lines[2]);
    expect(lines[1]).toHaveLength(500);
    expect(lines[1]?.slice(0, 9)).toBe("000000001");
    expect(lines[1]?.slice(9, 34).trimEnd()).toBe("Immanuel");
  });
});

describe("file status handling", () => {
  it("abends when the input dataset cannot be opened", () => {
    const lines: string[] = [];
    const display = (line: string): void => {
      lines.push(line);
    };

    expect(() => runCbact01c({ path: join(tmpdir(), "no-such-acctdata.txt"), display })).toThrow(
      AbendError,
    );
    expect(lines).toEqual([
      "START OF EXECUTION OF PROGRAM CBACT01C",
      "ERROR OPENING ACCTFILE",
      "FILE STATUS IS: NNNN0035",
      "ABENDING PROGRAM",
    ]);
  });

  it("reads an empty dataset and reaches end of file immediately", () => {
    const empty = join(mkdtempSync(join(tmpdir(), "carddemo-")), "custdata.txt");
    writeFileSync(empty, "", "latin1");

    const { result, lines } = capture(runCbcus01c, empty);
    expect(result.recordsRead).toBe(0);
    expect(lines).toEqual([
      "START OF EXECUTION OF PROGRAM CBCUS01C",
      "END OF EXECUTION OF PROGRAM CBCUS01C",
    ]);
  });

  it("renders IO-STATUS-04 the way 9910-DISPLAY-IO-STATUS does", () => {
    expect(formatIoStatus("00")).toBe("FILE STATUS IS: NNNN0000");
    expect(formatIoStatus("23")).toBe("FILE STATUS IS: NNNN0023");
    expect(formatIoStatus("9A")).toBe("FILE STATUS IS: NNNN9065");
  });
});
