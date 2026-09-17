import { describe, expect, it } from "vitest";

import { editBirthDateCcyymmdd, editDateCcyymmdd, editDateOfBirth } from "./date-edits.js";

const options = { variableName: "Open Date", systemDate: new Date(2025, 0, 15) };

describe("editDateCcyymmdd", () => {
  it("accepts a valid date", () => {
    const result = editDateCcyymmdd("20240229", options);
    expect(result.valid).toBe(true);
    expect(result.message).toBe("");
    expect(result.flags).toEqual({ year: "valid", month: "valid", day: "valid" });
    expect(result.validation?.severity).toBe(0);
  });

  it.each([
    ["        ", "Open Date : Year must be supplied.", "blank"],
    ["20XX0101", "Open Date must be 4 digit number.", "invalid"],
    ["18990101", "Open Date : Century is not valid.", "invalid"],
  ])("rejects %s", (date, message, yearFlag) => {
    const result = editDateCcyymmdd(date, options);
    expect(result.valid).toBe(false);
    expect(result.message).toBe(message);
    expect(result.flags.year).toBe(yearFlag);
  });

  it("reports the first failure only", () => {
    const result = editDateCcyymmdd("1899  99", options);
    expect(result.message).toBe("Open Date : Century is not valid.");
    expect(result.flags.month).toBe("blank");
  });

  it.each([
    ["2025  01", "Open Date : Month must be supplied."],
    ["20251301", "Open Date: Month must be a number between 1 and 12."],
    ["202501  ", "Open Date : Day must be supplied."],
    ["20250132", "Open Date:day must be a number between 1 and 31."],
  ])("rejects %s", (date, message) => {
    const result = editDateCcyymmdd(date, options);
    expect(result.valid).toBe(false);
    expect(result.message).toBe(message);
  });

  it("rejects month and day combinations the component edits cannot see", () => {
    expect(editDateCcyymmdd("20250431", options).message).toBe(
      "Open Date:Cannot have 31 days in this month.",
    );
    expect(editDateCcyymmdd("20250230", options).message).toBe(
      "Open Date:Cannot have 30 days in this month.",
    );
    expect(editDateCcyymmdd("20250229", options).message).toBe(
      "Open Date:Not a leap year.Cannot have 29 days in this month.",
    );
  });

  it("applies the century rule of the copybook to 29 February", () => {
    expect(editDateCcyymmdd("20000229", options).valid).toBe(true);
    expect(editDateCcyymmdd("19000229", options).valid).toBe(false);
    expect(editDateCcyymmdd("20240229", options).valid).toBe(true);
  });

  it("does not run the LE check once an edit has failed", () => {
    expect(editDateCcyymmdd("20250230", options).validation).toBeUndefined();
  });
});

describe("editDateOfBirth", () => {
  it("rejects today and future dates", () => {
    expect(editDateOfBirth("20250115", options).valid).toBe(false);
    expect(editDateOfBirth("20260101", options).message).toBe(
      "Open Date:cannot be in the future ",
    );
  });

  it("accepts a past date", () => {
    expect(editDateOfBirth("19800101", options).valid).toBe(true);
  });
});

describe("editBirthDateCcyymmdd", () => {
  it("runs the edits before the reasonableness check", () => {
    expect(editBirthDateCcyymmdd("20250230", options).message).toBe(
      "Open Date:Cannot have 30 days in this month.",
    );
    expect(editBirthDateCcyymmdd("19800101", options).valid).toBe(true);
  });
});
