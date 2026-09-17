import { describe, expect, it } from "vitest";

import { csutldtc, feedbackCodes, validateDate } from "./csutldtc.js";
import { LAST_LILIAN_DAY, lilianDay } from "./lilian.js";

const systemDate = new Date(2025, 0, 15);

describe("lilian day numbers", () => {
  it("anchors on the first day of the Gregorian calendar", () => {
    expect(lilianDay({ year: 1582, month: 10, day: 15 })).toBe(1);
    expect(lilianDay({ year: 9999, month: 12, day: 31 })).toBe(LAST_LILIAN_DAY);
  });
});

describe("validateDate", () => {
  it("accepts a valid date and reports severity 0", () => {
    const result = validateDate("20240229", "YYYYMMDD");
    expect(result.valid).toBe(true);
    expect(result.severity).toBe(0);
    expect(result.returnCode).toBe(0);
    expect(result.msgNo).toBe(0);
    expect(result.resultText).toBe("Date is valid");
    expect(result.date).toEqual({ year: 2024, month: 2, day: 29 });
    expect(result.lilianDate).toBe(lilianDay({ year: 2024, month: 2, day: 29 }));
  });

  it("accepts delimited picture strings", () => {
    expect(validateDate("12/31/1999", "MM/DD/YYYY").valid).toBe(true);
    expect(validateDate("1999-12-31", "YYYY-MM-DD").valid).toBe(true);
  });

  it("reports a mismatched delimiter as a date value error", () => {
    const result = validateDate("1999/12/31", "YYYY-MM-DD");
    expect(result.condition).toBe("badDateValue");
    expect(result.msgNo).toBe(2508);
  });

  describe("severity codes the COBOL callers branch on", () => {
    it.each([
      ["20250230", "YYYYMMDD", "badDateValue", 2508],
      ["20251301", "YYYYMMDD", "invalidMonth", 2517],
      ["2025011", "YYYYMMDD", "insufficientData", 2507],
      ["2025AB01", "YYYYMMDD", "nonNumericData", 2520],
      ["00000101", "YYYYMMDD", "yearInEraZero", 2521],
      ["15821014", "YYYYMMDD", "unsupportedRange", 2513],
      ["20250101", "YYYYMMQQ", "badPicString", 2518],
    ])("%s / %s is %s", (date, format, condition, msgNo) => {
      const result = validateDate(date, format);
      expect(result.valid).toBe(false);
      expect(result.condition).toBe(condition);
      expect(result.msgNo).toBe(msgNo);
      expect(result.severity).toBe(3);
      expect(result.returnCode).toBe(3);
    });
  });

  describe("leap years", () => {
    it.each([
      ["20240229", true],
      ["20230229", false],
      ["20000229", true],
      ["19000229", false],
      ["21000229", false],
    ])("%s is %s", (date, valid) => {
      expect(validateDate(date, "YYYYMMDD").valid).toBe(valid);
    });
  });

  describe("month ends", () => {
    it.each([
      ["20250430", true],
      ["20250431", false],
      ["20250131", true],
      ["20250132", false],
      ["20250100", false],
    ])("%s is %s", (date, valid) => {
      expect(validateDate(date, "YYYYMMDD").valid).toBe(valid);
    });
  });

  describe("century window", () => {
    it("places two digit years in the 100 years starting 80 years back", () => {
      expect(validateDate("450101", "YYMMDD", { systemDate }).date?.year).toBe(1945);
      expect(validateDate("440101", "YYMMDD", { systemDate }).date?.year).toBe(2044);
      expect(validateDate("250101", "YYMMDD", { systemDate }).date?.year).toBe(2025);
    });

    it("moves the window with the system date", () => {
      const later = new Date(2085, 0, 1);
      expect(validateDate("050101", "YYMMDD", { systemDate: later }).date?.year).toBe(2005);
      expect(validateDate("040101", "YYMMDD", { systemDate: later }).date?.year).toBe(2104);
    });
  });

  it("fills the 80 character result area CSUTLDTC returns", () => {
    const result = csutldtc("20250101", "YYYYMMDD");
    expect(result.message).toHaveLength(80);
    expect(result.message.slice(0, 4)).toBe("0000");
    expect(result.message).toContain("Mesg Code:");
    expect(result.message).toContain("TstDate: 20250101");
    expect(result.message).toContain("Mask used:YYYYMMDD");
  });

  it("keeps the feedback table aligned with the COBOL condition tokens", () => {
    expect(feedbackCodes.ok.severity).toBe(0);
    expect(Object.values(feedbackCodes).filter((code) => code.severity === 3)).toHaveLength(8);
  });
});
