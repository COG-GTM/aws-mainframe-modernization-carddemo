/**
 * The COBOL verbs the transaction programs lean on when they edit screen
 * fields: `IS NUMERIC`, `FUNCTION NUMVAL`/`NUMVAL-C`, the `MOVE` of a
 * fixed-length alphanumeric field and the numeric edit pictures the maps use.
 */

import { csutldtc } from "@carddemo/utilities";

/** A `MOVE` of `value` into a `PIC X(n)` field: truncate, then pad with spaces. */
export function moveAlphanumeric(value: string, length: number): string {
  return value.slice(0, length).padEnd(length, " ");
}

/** `IF field = SPACES OR LOW-VALUES`; both arrive as an empty/blank string. */
export function isBlank(value: string): boolean {
  return value.trim().length === 0;
}

/**
 * `IF field IS NUMERIC` on a display field: every character must be a digit,
 * so an embedded blank, sign or point fails the test.
 */
export function isNumericField(value: string): boolean {
  return value.length > 0 && /^[0-9]+$/.test(value);
}

/** `FUNCTION NUMVAL`, restricted to the digit strings the maps allow. */
export function numval(value: string): number {
  const trimmed = value.trim();
  return trimmed.length === 0 ? 0 : Number(trimmed);
}

/**
 * `FUNCTION NUMVAL-C` of an amount typed as `-99999999.99`: strip the
 * currency edit characters and keep the sign and the two decimals.
 */
export function numvalC(value: string): number {
  const trimmed = value.trim();
  if (trimmed.length === 0) {
    return 0;
  }
  const negative = trimmed.startsWith("-") || trimmed.endsWith("-");
  const digits = trimmed.replace(/[^0-9.]/g, "");
  const magnitude = digits.length === 0 ? 0 : Number(digits);
  return negative ? -magnitude : magnitude;
}

/**
 * A numeric `MOVE` into a `PIC 9(n)` display field: absolute value, truncated
 * to the low order `n` digits and zero filled.
 */
export function moveNumeric(value: number, digits: number): string {
  const truncated = Math.trunc(Math.abs(value)) % 10 ** digits;
  return String(truncated).padStart(digits, "0");
}

/**
 * A `MOVE` into an edited field such as `PIC +99999999.99`: the sign is always
 * shown, the integer part is zero filled to `intDigits` and the value is
 * rounded half away from zero, the COBOL default for `ROUNDED`.
 */
export function formatSignedAmount(value: number, intDigits: number): string {
  const cents = Math.round(Math.abs(value) * 100);
  const units = Math.trunc(cents / 100);
  const fraction = cents % 100;
  const sign = value < 0 ? "-" : "+";
  return `${sign}${String(units).padStart(intDigits, "0")}.${String(fraction).padStart(2, "0")}`;
}

/** `TRAN-AMT` on the transaction maps: `PIC +99999999.99`, 12 characters. */
export const formatTranAmount = (value: number): string => formatSignedAmount(value, 8);

/** `CURBAL` on `COBIL00`: `PIC +9999999999.99`, 14 characters. */
export const formatBalance = (value: number): string => formatSignedAmount(value, 10);

/**
 * `POPULATE-TRAN-DATA` of `COTRN00C`: `TRAN-ORIG-TS` (`YYYY-MM-DD-...`) is
 * reshuffled into the `MM/DD/YY` date column of the list.
 */
export function listDate(origTs: string): string {
  const year = origTs.slice(2, 4);
  const month = origTs.slice(5, 7);
  const day = origTs.slice(8, 10);
  return `${month}/${day}/${year}`;
}

/** `WS-DATE-FORMAT` passed to `CSUTLDTC` by the online programs. */
export const DATE_FORMAT = "YYYY-MM-DD";

/** `CSUTLDTC` message number the programs deliberately ignore. */
export const IGNORED_DATE_MSG_NO = 2513;

/** The `CALL 'CSUTLDTC'` check: severity `0000`, ignoring message `2513`. */
export function isValidDate(value: string): boolean {
  const result = csutldtc(moveAlphanumeric(value, 10), DATE_FORMAT);
  return result.severity === 0 || result.msgNo === IGNORED_DATE_MSG_NO;
}

/** Rounds to the two decimals of a `PIC S9(n)V99` field before it is stored. */
export function toStoredAmount(value: number): number {
  return Math.round(value * 100) / 100;
}
