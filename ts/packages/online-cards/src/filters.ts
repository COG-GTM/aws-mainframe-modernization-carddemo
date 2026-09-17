/**
 * The account and card number edits the three card programs share
 * (`2210-EDIT-ACCOUNT` / `2220-EDIT-CARD` and their `12xx` twins in
 * `COCRDUPC`).
 */

import { ACCT_ID_LENGTH, CARD_NUM_LENGTH, ZERO_ACCT_ID, ZERO_CARD_NUM } from "./commarea.js";

/** `WS-EDIT-ACCT-FLAG` / `WS-EDIT-CARD-FLAG`. */
export type FilterFlag = "blank" | "valid" | "notOk";

export interface FilterEdit {
  readonly flag: FilterFlag;
  /** The value as stored in the commarea: zeros unless the edit passed. */
  readonly value: string;
}

/**
 * `MOVE X TO PIC X(n)`: a map field is space filled to its map length, so a
 * short entry is not numeric as far as the COBOL edits are concerned.
 */
const fixed = (value: string, length: number): string => value.slice(0, length).padEnd(length);

const isBlank = (value: string, length: number): boolean =>
  value.trim().length === 0 || /^0+$/.test(value.trim().padStart(length, "0"));

const isNumeric = (value: string, length: number): boolean => /^\d+$/.test(fixed(value, length));

/** `2210-EDIT-ACCOUNT`; `*` and spaces both mean "no filter". */
export const editAccountFilter = (raw: string): FilterEdit => {
  const value = raw === "*" ? "" : raw;
  if (isBlank(value, ACCT_ID_LENGTH)) {
    return { flag: "blank", value: ZERO_ACCT_ID };
  }
  if (!isNumeric(value, ACCT_ID_LENGTH)) {
    return { flag: "notOk", value: ZERO_ACCT_ID };
  }
  return { flag: "valid", value: fixed(value, ACCT_ID_LENGTH) };
};

/** `2220-EDIT-CARD`. */
export const editCardFilter = (raw: string): FilterEdit => {
  const value = raw === "*" ? "" : raw;
  if (isBlank(value, CARD_NUM_LENGTH)) {
    return { flag: "blank", value: ZERO_CARD_NUM };
  }
  if (!isNumeric(value, CARD_NUM_LENGTH)) {
    return { flag: "notOk", value: ZERO_CARD_NUM };
  }
  return { flag: "valid", value: fixed(value, CARD_NUM_LENGTH) };
};
