/**
 * The information and error messages the card screens display, copied from the
 * `88` levels of `WS-INFO-MSG` / `WS-ERROR-MSG` / `WS-RETURN-MSG` in
 * `COCRDLIC`, `COCRDSLC` and `COCRDUPC`.
 */

import type { FileStatusCode } from "@carddemo/vsam";

export const listMessages = {
  exit: "PF03 PRESSED.EXITING",
  noRecordsFound: "NO RECORDS FOUND FOR THIS SEARCH CONDITION.",
  moreThanOneAction: "PLEASE SELECT ONLY ONE RECORD TO VIEW OR UPDATE",
  invalidActionCode: "INVALID ACTION CODE",
  acctFilterNotNumeric: "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER",
  cardFilterNotNumeric: "CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER",
  noMoreRecords: "NO MORE RECORDS TO SHOW",
  noPreviousPages: "NO PREVIOUS PAGES TO DISPLAY",
  noMorePages: "NO MORE PAGES TO DISPLAY",
  informRecordActions: "TYPE S FOR DETAIL, U TO UPDATE ANY RECORD",
} as const;

export const detailMessages = {
  displayingDetails: "   Displaying requested details",
  promptForInput: "Please enter Account and Card Number",
  exit: "PF03 pressed.Exiting",
  promptForAcct: "Account number not provided",
  promptForCard: "Card number not provided",
  noSearchCriteria: "No input received",
  acctFilterNotNumeric: "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER",
  cardFilterNotNumeric: "CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER",
  didNotFindAcctCardCombo: "Did not find cards for this search condition",
} as const;

export const updateMessages = {
  foundCards: "Details of selected card shown above",
  promptForSearchKeys: "Please enter Account and Card Number",
  promptForChanges: "Update card details presented above.",
  promptForConfirmation: "Changes validated.Press F5 to save",
  confirmUpdateSuccess: "Changes committed to database",
  informFailure: "Changes unsuccessful. Please try again",
  promptForAcct: "Account number not provided",
  promptForCard: "Card number not provided",
  promptForName: "Card name not provided",
  nameMustBeAlpha: "Card name can only contain alphabets and spaces",
  noSearchCriteria: "No input received",
  noChangesDetected: "No change detected with respect to values fetched.",
  acctFilterNotNumeric: "ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER",
  cardFilterNotNumeric: "CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER",
  cardStatusMustBeYesNo: "Card Active Status must be Y or N",
  cardExpiryMonthNotValid: "Card expiry month must be between 1 and 12",
  cardExpiryYearNotValid: "Invalid card expiry year",
  didNotFindAcctCardCombo: "Did not find cards for this search condition",
  couldNotLockForUpdate: "Could not lock record for update",
  dataWasChangedBeforeUpdate: "Record changed by some one else. Please review",
  lockedButUpdateFailed: "Update of record failed",
} as const;

/**
 * `WS-FILE-ERROR-MESSAGE`. The CICS programs report the `RESP`/`RESP2` pair;
 * `@carddemo/vsam` reports COBOL file status codes, so the status takes the
 * `RESP` position and `RESP2` stays blank.
 */
export const fileErrorMessage = (
  operation: string,
  file: string,
  status: FileStatusCode,
): string =>
  `File Error: ${operation.padEnd(8)} on ${file.padEnd(9)} returned RESP ${status.padEnd(10)},RESP2 ${"".padEnd(10)}`.trimEnd();
