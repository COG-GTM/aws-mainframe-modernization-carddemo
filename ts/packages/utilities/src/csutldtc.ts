/**
 * `CSUTLDTC` — date validation.
 *
 * The COBOL program hands the date and its picture string to the Language
 * Environment service `CEEDAYS` and reports what came back: the condition
 * token severity (also returned as the program's `RETURN-CODE`), the LE
 * message number, and an 80-character result area that callers such as
 * `CSUTLDPY` display. This port reimplements the `CEEDAYS` validation
 * semantics and keeps those codes identical.
 */

import {
  LAST_LILIAN_DAY,
  daysInMonth,
  lilianDay,
  type CalendarDate,
} from "./lilian.js";

export type FeedbackCondition =
  | "ok"
  | "insufficientData"
  | "badDateValue"
  | "invalidEra"
  | "unsupportedRange"
  | "invalidMonth"
  | "badPicString"
  | "nonNumericData"
  | "yearInEraZero";

export interface FeedbackCode {
  /** Condition token severity; `0` for a valid date, `3` for every error. */
  readonly severity: number;
  /** Language Environment message number, e.g. `2517` for `CEE2517W`. */
  readonly msgNo: number;
  /** The 15-character text `CSUTLDTC` moves into `WS-RESULT`. */
  readonly result: string;
}

/**
 * The condition tokens the COBOL `EVALUATE` branches on. `ok` is the zero
 * token `FC-INVALID-DATE`, which `CSUTLDTC` reports as `Date is valid`.
 */
export const feedbackCodes: Readonly<Record<FeedbackCondition, FeedbackCode>> = {
  ok: { severity: 0, msgNo: 0, result: "Date is valid" },
  insufficientData: { severity: 3, msgNo: 2507, result: "Insufficient" },
  badDateValue: { severity: 3, msgNo: 2508, result: "Datevalue error" },
  invalidEra: { severity: 3, msgNo: 2509, result: "Invalid Era" },
  unsupportedRange: { severity: 3, msgNo: 2513, result: "Unsupp. Range" },
  invalidMonth: { severity: 3, msgNo: 2517, result: "Invalid month" },
  badPicString: { severity: 3, msgNo: 2518, result: "Bad Pic String" },
  nonNumericData: { severity: 3, msgNo: 2520, result: "Nonnumeric data" },
  yearInEraZero: { severity: 3, msgNo: 2521, result: "YearInEra is 0" },
};

export interface DateValidationOptions {
  /**
   * System date used for the two-digit year century window. `CEEDAYS` places
   * two-digit years in the 100-year range starting 80 years before this date.
   */
  readonly systemDate?: Date;
}

export interface DateValidationResult {
  readonly valid: boolean;
  readonly condition: FeedbackCondition;
  readonly severity: number;
  readonly msgNo: number;
  readonly resultText: string;
  /** `CSUTLDTC` moves the severity into `RETURN-CODE`. */
  readonly returnCode: number;
  /** The 80-character `LS-RESULT` area the COBOL program fills in. */
  readonly message: string;
  /** Lilian day number of the date; only present when it is valid. */
  readonly lilianDate?: number;
  /** Date resolved to a full year; only present when it is valid. */
  readonly date?: CalendarDate;
}

const MESSAGE_LENGTH = 80;
const CENTURY_WINDOW_OFFSET = 80;

type TokenKind = "year4" | "year2" | "month" | "day" | "literal";

interface MaskToken {
  readonly kind: TokenKind;
  readonly width: number;
  readonly text: string;
}

function tokenizeMask(mask: string): readonly MaskToken[] | undefined {
  const tokens: MaskToken[] = [];
  let index = 0;

  while (index < mask.length) {
    const rest = mask.slice(index);
    if (rest.startsWith("YYYY")) {
      tokens.push({ kind: "year4", width: 4, text: "YYYY" });
      index += 4;
    } else if (rest.startsWith("YY")) {
      tokens.push({ kind: "year2", width: 2, text: "YY" });
      index += 2;
    } else if (rest.startsWith("MM")) {
      tokens.push({ kind: "month", width: 2, text: "MM" });
      index += 2;
    } else if (rest.startsWith("DD")) {
      tokens.push({ kind: "day", width: 2, text: "DD" });
      index += 2;
    } else if (/^[YMD]/.test(rest)) {
      return undefined;
    } else {
      tokens.push({ kind: "literal", width: 1, text: rest.charAt(0) });
      index += 1;
    }
  }

  const counts = { year: 0, month: 0, day: 0 };
  for (const token of tokens) {
    if (token.kind === "year4" || token.kind === "year2") {
      counts.year += 1;
    } else if (token.kind === "month") {
      counts.month += 1;
    } else if (token.kind === "day") {
      counts.day += 1;
    }
  }

  if (counts.year !== 1 || counts.month !== 1 || counts.day !== 1) {
    return undefined;
  }
  return tokens;
}

function expandTwoDigitYear(year: number, systemDate: Date): number {
  const windowStart = systemDate.getFullYear() - CENTURY_WINDOW_OFFSET;
  const candidate = Math.floor(windowStart / 100) * 100 + year;
  return candidate < windowStart ? candidate + 100 : candidate;
}

function outcome(
  condition: FeedbackCondition,
  date: string,
  format: string,
  resolved?: CalendarDate,
): DateValidationResult {
  const code = feedbackCodes[condition];
  const base = {
    valid: condition === "ok",
    condition,
    severity: code.severity,
    msgNo: code.msgNo,
    resultText: code.result,
    returnCode: code.severity,
    message: formatValidationMessage(code, date, format),
  };

  return resolved === undefined
    ? base
    : { ...base, date: resolved, lilianDate: lilianDay(resolved) };
}

/** Builds the 80-character `WS-MESSAGE` / `LS-RESULT` area of `CSUTLDTC`. */
export function formatValidationMessage(
  code: FeedbackCode,
  date: string,
  format: string,
): string {
  const message =
    String(code.severity).padStart(4, "0") +
    "Mesg Code:".padEnd(11, " ") +
    String(code.msgNo).padStart(4, "0") +
    " " +
    code.result.padEnd(15, " ") +
    " " +
    "TstDate:".padEnd(9, " ") +
    date.padEnd(10, " ").slice(0, 10) +
    " " +
    "Mask used:" +
    format.padEnd(10, " ").slice(0, 10);
  return message.padEnd(MESSAGE_LENGTH, " ");
}

/**
 * Validates a date against a `CEEDAYS` picture string.
 *
 * Supported picture strings combine exactly one year (`YYYY` or `YY`), one
 * `MM` and one `DD` token with literal separators, for example `YYYYMMDD` or
 * `MM/DD/YYYY`; anything else is reported as `Bad Pic String`.
 */
export function validateDate(
  date: string,
  format = "YYYYMMDD",
  options: DateValidationOptions = {},
): DateValidationResult {
  const mask = format.trimEnd();
  const value = date.trimEnd();
  const tokens = tokenizeMask(mask);

  if (mask.length === 0 || tokens === undefined) {
    return outcome("badPicString", date, format);
  }

  const expectedLength = tokens.reduce((total, token) => total + token.width, 0);
  if (value.length < expectedLength) {
    return outcome("insufficientData", date, format);
  }

  let cursor = 0;
  let year: number | undefined;
  let twoDigitYear = false;
  let month: number | undefined;
  let day: number | undefined;

  for (const token of tokens) {
    const part = value.slice(cursor, cursor + token.width);
    cursor += token.width;

    if (token.kind === "literal") {
      if (part !== token.text) {
        return outcome("badDateValue", date, format);
      }
      continue;
    }

    if (!/^\d+$/.test(part)) {
      return outcome("nonNumericData", date, format);
    }

    const numeric = Number(part);
    if (token.kind === "month") {
      month = numeric;
    } else if (token.kind === "day") {
      day = numeric;
    } else {
      year = numeric;
      twoDigitYear = token.kind === "year2";
    }
  }

  if (year === undefined || month === undefined || day === undefined) {
    return outcome("badPicString", date, format);
  }

  if (year === 0) {
    return outcome("yearInEraZero", date, format);
  }
  if (twoDigitYear) {
    year = expandTwoDigitYear(year, options.systemDate ?? new Date());
  }

  if (month < 1 || month > 12) {
    return outcome("invalidMonth", date, format);
  }
  if (day < 1 || day > daysInMonth(year, month)) {
    return outcome("badDateValue", date, format);
  }

  const resolved: CalendarDate = { year, month, day };
  const lilian = lilianDay(resolved);
  if (lilian < 1 || lilian > LAST_LILIAN_DAY) {
    return outcome("unsupportedRange", date, format);
  }

  return outcome("ok", date, format, resolved);
}

/**
 * The `CSUTLDTC` callable interface: `CALL 'CSUTLDTC' USING LS-DATE,
 * LS-DATE-FORMAT, LS-RESULT`. `LS-RESULT` is the `message` of the returned
 * object and the COBOL `RETURN-CODE` is its `returnCode`.
 */
export function csutldtc(
  lsDate: string,
  lsDateFormat: string,
  options: DateValidationOptions = {},
): DateValidationResult {
  return validateDate(lsDate, lsDateFormat, options);
}
