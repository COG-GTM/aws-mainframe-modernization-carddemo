/**
 * The `WS-INFO-MSG` and `WS-RETURN-MSG` condition names declared by
 * `COACTVWC` and `COACTUPC`.
 */

/** `COACTVWC` `WS-INFO-MSG` / `WS-RETURN-MSG` values. */
export const viewMessages = {
  /** `WS-PROMPT-FOR-INPUT`. */
  promptForInput: "Enter or update id of account to display",
  /**
   * `WS-INFORM-OUTPUT`. Declared by the program but never set: the map always
   * carries the prompt, so this port keeps it as a constant only.
   */
  informOutput: "Displaying details of given Account",
  /** `WS-EXIT-MESSAGE`. */
  exit: "PF03 pressed.Exiting",
  /** `WS-PROMPT-FOR-ACCT`. */
  promptForAcct: "Account number not provided",
  /** `NO-SEARCH-CRITERIA-RECEIVED`. */
  noSearchCriteria: "No input received",
  /** `SEARCHED-ACCT-ZEROES` / `SEARCHED-ACCT-NOT-NUMERIC`. */
  acctNotElevenDigits: "Account number must be a non zero 11 digit number",
  /** `DID-NOT-FIND-ACCT-IN-CARDXREF`. */
  acctNotInCardXref: "Did not find this account in account card xref file",
  /** `DID-NOT-FIND-ACCT-IN-ACCTDAT`. */
  acctNotInMaster: "Did not find this account in account master file",
  /** `DID-NOT-FIND-CUST-IN-CUSTDAT`. */
  custNotInMaster: "Did not find associated customer in master file",
  /** `XREF-READ-ERROR`. */
  xrefReadError: "Error reading account card xref File",
} as const;

/** `COACTUPC` `WS-INFO-MSG` / `WS-RETURN-MSG` values. */
export const updateMessages = {
  /** `FOUND-ACCOUNT-DATA`. */
  foundAccountData: "Details of selected account shown above",
  /** `PROMPT-FOR-SEARCH-KEYS`. */
  promptForSearchKeys: "Enter or update id of account to update",
  /** `PROMPT-FOR-CHANGES`. */
  promptForChanges: "Update account details presented above.",
  /** `PROMPT-FOR-CONFIRMATION`. */
  promptForConfirmation: "Changes validated.Press F5 to save",
  /** `CONFIRM-UPDATE-SUCCESS`. */
  updateSuccess: "Changes committed to database",
  /** `INFORM-FAILURE`. */
  updateFailure: "Changes unsuccessful. Please try again",
  /** `WS-EXIT-MESSAGE`. */
  exit: "PF03 pressed.Exiting",
  /** `WS-PROMPT-FOR-ACCT`. */
  promptForAcct: "Account number not provided",
  /** `WS-PROMPT-FOR-LASTNAME`. */
  promptForLastName: "Last name not provided",
  /** `WS-NAME-MUST-BE-ALPHA`. */
  nameMustBeAlpha: "Name can only contain alphabets and spaces",
  /** `NO-SEARCH-CRITERIA-RECEIVED`. */
  noSearchCriteria: "No input received",
  /** `NO-CHANGES-DETECTED`. */
  noChangesDetected: "No change detected with respect to values fetched.",
  /**
   * `SEARCHED-ACCT-ZEROES` / `SEARCHED-ACCT-NOT-NUMERIC`. Declared but not
   * used: `1210-EDIT-ACCOUNT` builds `acctNotElevenDigitsNonZero` instead.
   */
  acctNotElevenDigits: "Account number must be a non zero 11 digit number",
  /** The message `1210-EDIT-ACCOUNT` builds for a bad account number. */
  acctNotElevenDigitsNonZero:
    "Account Number if supplied must be a 11 digit Non-Zero Number",
  /** `DID-NOT-FIND-ACCT-IN-CARDXREF`. */
  acctNotInCardXref: "Did not find this account in account card xref file",
  /** `DID-NOT-FIND-ACCT-IN-ACCTDAT`. */
  acctNotInMaster: "Did not find this account in account master file",
  /** `DID-NOT-FIND-CUST-IN-CUSTDAT`. */
  custNotInMaster: "Did not find associated customer in master file",
  /** `ACCT-STATUS-MUST-BE-YES-NO`. */
  acctStatusMustBeYesNo: "Account Active Status must be Y or N",
  /** `CRED-LIMIT-IS-BLANK`. */
  creditLimitBlank: "Credit Limit must be supplied",
  /** `CRED-LIMIT-IS-NOT-VALID`. */
  creditLimitNotValid: "Credit Limit is not valid",
  /** `THIS-MONTH-NOT-VALID`. */
  expiryMonthNotValid: "Card expiry month must be between 1 and 12",
  /** `THIS-YEAR-NOT-VALID`. */
  expiryYearNotValid: "Invalid card expiry year",
  /** The second `DID-NOT-FIND-ACCT-IN-CARDXREF` value. */
  acctNotInCardsDatabase: "Did not find this account in cards database",
  /** `DID-NOT-FIND-ACCTCARD-COMBO`. */
  noCardsForSearch: "Did not find cards for this search condition",
  /** `COULD-NOT-LOCK-ACCT-FOR-UPDATE`. */
  couldNotLockAccount: "Could not lock account record for update",
  /** `COULD-NOT-LOCK-CUST-FOR-UPDATE`. */
  couldNotLockCustomer: "Could not lock customer record for update",
  /** `DATA-WAS-CHANGED-BEFORE-UPDATE`. */
  recordChangedByAnotherUser: "Record changed by some one else. Please review",
  /** `LOCKED-BUT-UPDATE-FAILED`. */
  updateOfRecordFailed: "Update of record failed",
  /** `XREF-READ-ERROR`. */
  xrefReadError: "Error reading Card Data File",
} as const;

/** `WS-RETURN-MSG` is `PIC X(75)`, so longer messages are truncated. */
export const RETURN_MESSAGE_LENGTH = 75;

/** `ERROR-RESP` / `ERROR-RESP2`: a response code in a `PIC X(10)` field. */
function respCode(code: number): string {
  return String(code).padStart(9, "0").padEnd(10, " ");
}

/** CICS `RESP` value for `DFHRESP(NOTFND)`. */
export const RESP_NOTFND = 13;

/** `STRING ... INTO WS-RETURN-MSG` truncates at the receiving field's size. */
function returnMsg(text: string): string {
  return text.slice(0, RETURN_MESSAGE_LENGTH);
}

/** `9200-GETCARDXREF-BYACCT`, `DFHRESP(NOTFND)`. */
export function xrefAccountNotFound(acctId: string, resp = RESP_NOTFND, reas = 0): string {
  return returnMsg(
    `Account:${acctId} not found in Cross ref file.  Resp:${respCode(resp)} Reas:${respCode(reas)}`,
  );
}

/** `9300-GETACCTDATA-BYACCT`, `DFHRESP(NOTFND)`. */
export function accountNotFound(acctId: string, resp = RESP_NOTFND, reas = 0): string {
  return returnMsg(
    `Account:${acctId} not found in Acct Master file.Resp:${respCode(resp)} Reas:${respCode(reas)}`,
  );
}

/** `9400-GETCUSTDATA-BYCUST`, `DFHRESP(NOTFND)`. */
export function customerNotFound(custId: string, resp = RESP_NOTFND, reas = 0): string {
  return returnMsg(
    `CustId:${custId} not found in customer master.Resp: ${respCode(resp)} REAS:${respCode(reas)}`,
  );
}

/** `WS-FILE-ERROR-MESSAGE`. */
export function fileErrorMessage(
  operation: string,
  file: string,
  resp: number,
  reas: number,
): string {
  return returnMsg(
    `File Error: ${operation.padEnd(8, " ")} on ${file.padEnd(9, " ")} returned RESP ${respCode(resp)},RESP2 ${respCode(reas)}`,
  );
}
