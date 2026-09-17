import { describe, expect, it } from "vitest";

import { formatIoStatus } from "./abend.js";
import { addMoney, cycleBalance, toCents, truncateMoney } from "./money.js";
import { db2FormatTimestamp } from "./timestamp.js";

describe("money", () => {
  it("adds at the copybook scale without binary rounding error", () => {
    expect(addMoney(0.1, 0.2)).toBe(0.3);
    expect(addMoney(504.77, -575.54)).toBe(-70.77);
    expect(toCents(-70.77)).toBe(-7077);
  });

  it("truncates toward zero, as COBOL does without ROUNDED", () => {
    expect(truncateMoney(1.239)).toBe(1.23);
    expect(truncateMoney(-1.239)).toBe(-1.23);
  });

  it("computes the cycle balance the credit limit is checked against", () => {
    expect(cycleBalance(1164.87, -70.77, 715.44)).toBe(1951.08);
  });
});

describe("io status", () => {
  it("renders IO-STATUS-04 for numeric and 9x statuses", () => {
    expect(formatIoStatus("35")).toBe("0035");
    expect(formatIoStatus("9\u0004")).toBe("9004");
  });
});

describe("db2FormatTimestamp", () => {
  it("formats CURRENT-DATE as the 26 character DB2 timestamp", () => {
    const timestamp = db2FormatTimestamp(new Date(2022, 5, 11, 1, 2, 3, 40));
    expect(timestamp).toBe("2022-06-11-01.02.03.040000");
    expect(timestamp).toHaveLength(26);
  });
});
