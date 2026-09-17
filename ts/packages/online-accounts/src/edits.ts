/**
 * The generic field edit paragraphs `COACTUPC` performs (`1215-EDIT-MANDATORY`
 * through `1280-EDIT-US-STATE-ZIP-CD`) and the numeric pictures the maps use.
 *
 * Each edit returns the COBOL flag it sets plus the message it would have moved
 * into `WS-RETURN-MSG`; the caller keeps the first message, as the
 * `IF WS-RETURN-MSG-OFF` guard does.
 */

import type { FieldFlag } from "@carddemo/utilities";

import {
  generalPurposeAreaCodes,
  usStateCodes,
  usStateZipCombinations,
} from "./lookups.js";
import { RETURN_MESSAGE_LENGTH } from "./messages.js";

export interface EditResult {
  readonly flag: FieldFlag;
  readonly message: string;
}

export interface PhoneParts {
  readonly areaCode: string;
  readonly prefix: string;
  readonly lineNumber: string;
}

export interface PhoneEditResult {
  readonly areaCode: FieldFlag;
  readonly prefix: FieldFlag;
  readonly lineNumber: FieldFlag;
  readonly messages: readonly string[];
}

export interface SsnParts {
  readonly part1: string;
  readonly part2: string;
  readonly part3: string;
}

export interface SsnEditResult {
  readonly part1: FieldFlag;
  readonly part2: FieldFlag;
  readonly part3: FieldFlag;
  readonly messages: readonly string[];
}

const valid: EditResult = { flag: "valid", message: "" };

/** `LOW-VALUES`, `SPACES` or a value that trims away to nothing. */
export function isBlank(value: string): boolean {
  return value.replaceAll("\u0000", "").trim().length === 0;
}

/** COBOL `IS NUMERIC` over a fixed length alphanumeric field. */
export function isNumericField(value: string, length: number): boolean {
  return new RegExp(`^\\d{${length}}$`).test(value);
}

/** `1215-EDIT-MANDATORY`. */
export function editMandatory(name: string, value: string): EditResult {
  if (isBlank(value)) {
    return { flag: "blank", message: `${name.trim()} must be supplied.` };
  }
  return valid;
}

/** `1220-EDIT-YESNO`. */
export function editYesNo(name: string, value: string): EditResult {
  if (isBlank(value) || value.trim() === "0") {
    return { flag: "blank", message: `${name.trim()} must be supplied.` };
  }
  if (value.trim() !== "Y" && value.trim() !== "N") {
    return { flag: "invalid", message: `${name.trim()} must be Y or N.` };
  }
  return valid;
}

/** `1225-EDIT-ALPHA-REQD`. */
export function editAlphaRequired(name: string, value: string): EditResult {
  if (isBlank(value)) {
    return { flag: "blank", message: `${name.trim()} must be supplied.` };
  }
  return editAlphaOptional(name, value);
}

/** `1235-EDIT-ALPHA-OPT`. */
export function editAlphaOptional(name: string, value: string): EditResult {
  if (isBlank(value)) {
    return valid;
  }
  if (!/^[A-Za-z ]*$/.test(value.replaceAll("\u0000", " "))) {
    return { flag: "invalid", message: `${name.trim()} can have alphabets only.` };
  }
  return valid;
}

/** `1245-EDIT-NUM-REQD`. */
export function editNumRequired(name: string, value: string, length: number): EditResult {
  if (isBlank(value)) {
    return { flag: "blank", message: `${name.trim()} must be supplied.` };
  }
  if (!isNumericField(value, length)) {
    return { flag: "invalid", message: `${name.trim()} must be all numeric.` };
  }
  if (Number(value) === 0) {
    return { flag: "invalid", message: `${name.trim()} must not be zero.` };
  }
  return valid;
}

/** `FUNCTION TEST-NUMVAL-C`: zero when the text is a valid numeric literal. */
export function testNumvalC(value: string): number {
  return /^ *[+-]? *\$? *(\d{1,3}(,\d{3})*|\d*)(\.\d*)? *(CR|DB)? *[+-]? *$/.test(value) &&
    /\d/.test(value)
    ? 0
    : 1;
}

/** `FUNCTION NUMVAL-C`. */
export function numvalC(value: string): number {
  const negative = /-/.test(value) || /\b(CR|DB)\b/.test(value);
  const digits = value.replace(/[^0-9.]/g, "");
  const magnitude = digits.length === 0 ? 0 : Number(digits);
  return negative ? -magnitude : magnitude;
}

/** `1250-EDIT-SIGNED-9V2`. */
export function editSigned9v2(name: string, value: string): EditResult {
  if (isBlank(value)) {
    return { flag: "blank", message: `${name.trim()} must be supplied.` };
  }
  if (testNumvalC(value) !== 0) {
    return { flag: "invalid", message: `${name.trim()} is not valid` };
  }
  return valid;
}

/** `1260-EDIT-US-PHONE-NUM`, including its three component paragraphs. */
export function editUsPhone(name: string, phone: PhoneParts): PhoneEditResult {
  const label = name.trim();
  const messages: string[] = [];

  if (isBlank(phone.areaCode) && isBlank(phone.prefix) && isBlank(phone.lineNumber)) {
    return { areaCode: "valid", prefix: "valid", lineNumber: "valid", messages };
  }

  let areaCode: FieldFlag = "valid";
  if (isBlank(phone.areaCode)) {
    areaCode = "blank";
    messages.push(`${label}: Area code must be supplied.`);
  } else if (!isNumericField(phone.areaCode, 3)) {
    areaCode = "invalid";
    messages.push(`${label}: Area code must be A 3 digit number.`);
  } else if (Number(phone.areaCode) === 0) {
    areaCode = "invalid";
    messages.push(`${label}: Area code cannot be zero`);
  } else if (!generalPurposeAreaCodes.has(phone.areaCode)) {
    areaCode = "invalid";
    messages.push(`${label}: Not valid North America general purpose area code`);
  }

  let prefix: FieldFlag = "valid";
  if (isBlank(phone.prefix)) {
    prefix = "blank";
    messages.push(`${label}: Prefix code must be supplied.`);
  } else if (!isNumericField(phone.prefix, 3)) {
    prefix = "invalid";
    messages.push(`${label}: Prefix code must be A 3 digit number.`);
  } else if (Number(phone.prefix) === 0) {
    prefix = "invalid";
    messages.push(`${label}: Prefix code cannot be zero`);
  }

  let lineNumber: FieldFlag = "valid";
  if (isBlank(phone.lineNumber)) {
    lineNumber = "blank";
    messages.push(`${label}: Line number code must be supplied.`);
  } else if (!isNumericField(phone.lineNumber, 4)) {
    lineNumber = "invalid";
    messages.push(`${label}: Line number code must be A 4 digit number.`);
  } else if (Number(phone.lineNumber) === 0) {
    lineNumber = "invalid";
    messages.push(`${label}: Line number code cannot be zero`);
  }

  return { areaCode, prefix, lineNumber, messages };
}

/** `1265-EDIT-US-SSN`. */
export function editUsSsn(ssn: SsnParts): SsnEditResult {
  const messages: string[] = [];

  const first = editNumRequired("SSN: First 3 chars", ssn.part1, 3);
  let part1 = first.flag;
  if (first.message.length > 0) {
    messages.push(first.message);
  } else if (
    Number(ssn.part1) === 0 ||
    Number(ssn.part1) === 666 ||
    Number(ssn.part1) >= 900
  ) {
    part1 = "invalid";
    messages.push("SSN: First 3 chars: should not be 000, 666, or between 900 and 999");
  }

  const second = editNumRequired("SSN 4th & 5th chars", ssn.part2, 2);
  if (second.message.length > 0) {
    messages.push(second.message);
  }

  const third = editNumRequired("SSN Last 4 chars", ssn.part3, 4);
  if (third.message.length > 0) {
    messages.push(third.message);
  }

  return { part1, part2: second.flag, part3: third.flag, messages };
}

/** `1270-EDIT-US-STATE-CD`. */
export function editUsStateCode(name: string, state: string): EditResult {
  if (usStateCodes.has(state.trim().toUpperCase())) {
    return valid;
  }
  return { flag: "invalid", message: `${name.trim()}: is not a valid state code` };
}

/** `1275-EDIT-FICO-SCORE`. */
export function editFicoScore(name: string, score: string): EditResult {
  const value = Number(score);
  if (value >= 300 && value <= 850) {
    return valid;
  }
  return { flag: "invalid", message: `${name.trim()}: should be between 300 and 850` };
}

/** `1280-EDIT-US-STATE-ZIP-CD`. */
export function editStateZip(state: string, zip: string): EditResult {
  const combination = `${state.trim().toUpperCase().padEnd(2, " ")}${zip.slice(0, 2)}`;
  if (usStateZipCombinations.has(combination)) {
    return valid;
  }
  return { flag: "invalid", message: "Invalid zip code for state" };
}

/** `PIC +ZZZ,ZZZ,ZZZ.99`, the currency picture both maps display. */
export function formatCurrency(value: number): string {
  const rounded = Math.round(Math.abs(value) * 100);
  const units = Math.trunc(rounded / 100);
  const cents = String(rounded % 100).padStart(2, "0");
  const grouped = units.toLocaleString("en-US");
  return `${value < 0 ? "-" : "+"}${grouped.padStart(11, " ")}.${cents}`;
}

/** Keeps the first non-empty message, as `IF WS-RETURN-MSG-OFF` does. */
export class ReturnMessage {
  private text = "";

  set(message: string): void {
    if (this.text.length === 0 && message.length > 0) {
      this.text = message.slice(0, RETURN_MESSAGE_LENGTH);
    }
  }

  setAll(messages: readonly string[]): void {
    for (const message of messages) {
      this.set(message);
    }
  }

  get value(): string {
    return this.text;
  }

  get isOff(): boolean {
    return this.text.length === 0;
  }
}
