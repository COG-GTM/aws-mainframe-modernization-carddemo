/**
 * `CSUTLDPY` / `CSUTLDWY` — the shared date edit logic the online programs
 * copy into their procedure division.
 *
 * The COBOL paragraphs edit the year, month and day of a `CCYYMMDD` field in
 * turn, then check the day/month/year combination, then hand the date to
 * `CSUTLDTC` for a final Language Environment check. Each paragraph sets a
 * per-component flag and, for the first failure only, the return message the
 * screen displays.
 */

import { csutldtc, type DateValidationResult } from "./csutldtc.js";
import { lilianDay } from "./lilian.js";

export type FieldFlag = "valid" | "invalid" | "blank";

export interface DateEditFlags {
  readonly year: FieldFlag;
  readonly month: FieldFlag;
  readonly day: FieldFlag;
}

export interface DateEditResult {
  /** `false` when the COBOL code would have set `INPUT-ERROR`. */
  readonly valid: boolean;
  readonly flags: DateEditFlags;
  /** `WS-RETURN-MSG`; empty when the date passed every edit. */
  readonly message: string;
  /** Outcome of the `CSUTLDTC` call, when the edits got that far. */
  readonly validation?: DateValidationResult;
}

export interface DateEditOptions {
  /** `WS-EDIT-VARIABLE-NAME`, used to prefix the return message. */
  readonly variableName?: string;
  /** System date used for the date of birth check and the century window. */
  readonly systemDate?: Date;
}

const THIRTY_ONE_DAY_MONTHS = [1, 3, 5, 7, 8, 10, 12];

class EditState {
  year: FieldFlag = "valid";
  month: FieldFlag = "valid";
  day: FieldFlag = "valid";
  inputError = false;
  message = "";

  constructor(private readonly variableName: string) {}

  fail(message: string): void {
    this.inputError = true;
    if (this.message.length === 0) {
      this.message = `${this.variableName.trim()}${message}`;
    }
  }

  get allFlagsValid(): boolean {
    return this.year === "valid" && this.month === "valid" && this.day === "valid";
  }

  result(validation?: DateValidationResult): DateEditResult {
    const base = {
      valid: !this.inputError,
      flags: { year: this.year, month: this.month, day: this.day },
      message: this.message,
    };
    return validation === undefined ? base : { ...base, validation };
  }
}

interface DateParts {
  readonly century: string;
  readonly year: string;
  readonly month: string;
  readonly day: string;
}

function split(date: string): DateParts {
  const padded = date.padEnd(8, " ");
  return {
    century: padded.slice(0, 2),
    year: padded.slice(2, 4),
    month: padded.slice(4, 6),
    day: padded.slice(6, 8),
  };
}

function isBlank(value: string): boolean {
  return value.trim().length === 0 || value.includes("\u0000");
}

function editYear(state: EditState, parts: DateParts): void {
  state.year = "invalid";
  const ccyy = `${parts.century}${parts.year}`;

  if (isBlank(ccyy)) {
    state.year = "blank";
    state.fail(" : Year must be supplied.");
    return;
  }
  if (!/^\d{4}$/.test(ccyy)) {
    state.fail(" must be 4 digit number.");
    return;
  }
  if (parts.century !== "19" && parts.century !== "20") {
    state.fail(" : Century is not valid.");
    return;
  }
  state.year = "valid";
}

function editMonth(state: EditState, parts: DateParts): void {
  state.month = "invalid";

  if (isBlank(parts.month)) {
    state.month = "blank";
    state.fail(" : Month must be supplied.");
    return;
  }
  if (!/^\d{2}$/.test(parts.month) || Number(parts.month) < 1 || Number(parts.month) > 12) {
    state.fail(": Month must be a number between 1 and 12.");
    return;
  }
  state.month = "valid";
}

function editDay(state: EditState, parts: DateParts): void {
  state.day = "valid";

  if (isBlank(parts.day)) {
    state.day = "blank";
    state.fail(" : Day must be supplied.");
    return;
  }
  if (!/^\d{2}$/.test(parts.day) || Number(parts.day) < 1 || Number(parts.day) > 31) {
    state.day = "invalid";
    state.fail(":day must be a number between 1 and 31.");
    return;
  }
  state.day = "valid";
}

/** `EDIT-DAY-MONTH-YEAR`: the combinations the per-component edits cannot see. */
function editDayMonthYear(state: EditState, parts: DateParts): boolean {
  if (!/^\d{2}$/.test(parts.month) || !/^\d{2}$/.test(parts.day)) {
    return false;
  }

  const month = Number(parts.month);
  const day = Number(parts.day);

  if (!THIRTY_ONE_DAY_MONTHS.includes(month) && day === 31) {
    state.day = "invalid";
    state.month = "invalid";
    state.fail(":Cannot have 31 days in this month.");
    return false;
  }

  if (month === 2 && day === 30) {
    state.day = "invalid";
    state.month = "invalid";
    state.fail(":Cannot have 30 days in this month.");
    return false;
  }

  if (month === 2 && day === 29) {
    const divisor = Number(parts.year) === 0 ? 400 : 4;
    if (Number(`${parts.century}${parts.year}`) % divisor !== 0) {
      state.day = "invalid";
      state.month = "invalid";
      state.year = "invalid";
      state.fail(":Not a leap year.Cannot have 29 days in this month.");
      return false;
    }
  }

  return state.allFlagsValid;
}

/** `EDIT-DATE-LE`: the belt-and-braces `CSUTLDTC` call. */
function editDateLe(
  state: EditState,
  date: string,
  options: DateEditOptions,
): DateValidationResult {
  const validation = csutldtc(
    date,
    "YYYYMMDD",
    options.systemDate === undefined ? {} : { systemDate: options.systemDate },
  );

  if (validation.severity !== 0) {
    state.year = "invalid";
    state.month = "invalid";
    state.day = "invalid";
    state.fail(
      ` validation error Sev code: ${String(validation.severity).padStart(4, "0")}` +
        ` Message code: ${String(validation.msgNo).padStart(4, "0")}`,
    );
  }

  return validation;
}

/**
 * `PERFORM EDIT-DATE-CCYYMMDD THRU EDIT-DATE-CCYYMMDD-EXIT`: validates a
 * `CCYYMMDD` date, component by component and then as a whole.
 */
export function editDateCcyymmdd(date: string, options: DateEditOptions = {}): DateEditResult {
  const state = new EditState(options.variableName ?? "");
  const parts = split(date);

  editYear(state, parts);
  editMonth(state, parts);
  editDay(state, parts);

  if (!editDayMonthYear(state, parts)) {
    return state.result();
  }

  return state.result(editDateLe(state, date, options));
}

/**
 * `EDIT-DATE-OF-BIRTH`: a date of birth has to be in the past. Callers run it
 * only once {@link editDateCcyymmdd} has accepted the date.
 */
export function editDateOfBirth(date: string, options: DateEditOptions = {}): DateEditResult {
  const state = new EditState(options.variableName ?? "");
  const parts = split(date);
  const today = options.systemDate ?? new Date();

  const birth = lilianDay({
    year: Number(`${parts.century}${parts.year}`),
    month: Number(parts.month),
    day: Number(parts.day),
  });
  const current = lilianDay({
    year: today.getFullYear(),
    month: today.getMonth() + 1,
    day: today.getDate(),
  });

  if (current <= birth) {
    state.year = "invalid";
    state.month = "invalid";
    state.day = "invalid";
    state.fail(":cannot be in the future ");
  }

  return state.result();
}

/** The caller-side sequence: edit the date, then apply the birth date check. */
export function editBirthDateCcyymmdd(
  date: string,
  options: DateEditOptions = {},
): DateEditResult {
  const edited = editDateCcyymmdd(date, options);
  if (!edited.valid) {
    return edited;
  }
  return editDateOfBirth(date, options);
}
