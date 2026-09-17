/**
 * The `WS-MESSAGE` literals of the online transaction programs, kept verbatim
 * so the screens read exactly as they do on the mainframe.
 */

/** `CCDA-MSG-INVALID-KEY` of `CSMSG01Y`. */
export const INVALID_KEY = "Invalid key pressed. Please see below...";

/** `COTRN00C`. */
export const listMessages = {
  tranIdNotNumeric: "Tran ID must be Numeric ...",
  alreadyTopOfPage: "You are already at the top of the page...",
  alreadyBottomOfPage: "You are already at the bottom of the page...",
  topOfPage: "You are at the top of the page...",
  bottomReached: "You have reached the bottom of the page...",
  topReached: "You have reached the top of the page...",
  lookupFailed: "Unable to lookup transaction...",
  invalidSelection: "Invalid selection. Valid value is S",
} as const;

/** `COTRN01C`. */
export const viewMessages = {
  tranIdEmpty: "Tran ID can NOT be empty...",
  tranIdNotFound: "Transaction ID NOT found...",
  lookupFailed: "Unable to lookup Transaction...",
} as const;

/** `COTRN02C`. */
export const addMessages = {
  acctIdNotNumeric: "Account ID must be Numeric...",
  cardNumNotNumeric: "Card Number must be Numeric...",
  keyRequired: "Account or Card Number must be entered...",
  acctIdNotFound: "Account ID NOT found...",
  cardNumNotFound: "Card Number NOT found...",
  acctXrefLookupFailed: "Unable to lookup Acct in XREF AIX file...",
  cardXrefLookupFailed: "Unable to lookup Card # in XREF file...",
  typeCdEmpty: "Type CD can NOT be empty...",
  catCdEmpty: "Category CD can NOT be empty...",
  sourceEmpty: "Source can NOT be empty...",
  descEmpty: "Description can NOT be empty...",
  amountEmpty: "Amount can NOT be empty...",
  origDateEmpty: "Orig Date can NOT be empty...",
  procDateEmpty: "Proc Date can NOT be empty...",
  merchantIdEmpty: "Merchant ID can NOT be empty...",
  merchantNameEmpty: "Merchant Name can NOT be empty...",
  merchantCityEmpty: "Merchant City can NOT be empty...",
  merchantZipEmpty: "Merchant Zip can NOT be empty...",
  typeCdNotNumeric: "Type CD must be Numeric...",
  catCdNotNumeric: "Category CD must be Numeric...",
  amountFormat: "Amount should be in format -99999999.99",
  origDateFormat: "Orig Date should be in format YYYY-MM-DD",
  procDateFormat: "Proc Date should be in format YYYY-MM-DD",
  origDateInvalid: "Orig Date - Not a valid date...",
  procDateInvalid: "Proc Date - Not a valid date...",
  merchantIdNotNumeric: "Merchant ID must be Numeric...",
  confirmAdd: "Confirm to add this transaction...",
  invalidConfirm: "Invalid value. Valid values are (Y/N)...",
  tranIdExists: "Tran ID already exist...",
  addFailed: "Unable to Add Transaction...",
  tranIdNotFound: "Transaction ID NOT found...",
  lookupFailed: "Unable to lookup Transaction...",
} as const;

/** `WRITE-TRANSACT-FILE` of `COTRN02C`; `TRAN-ID` is `DELIMITED BY SPACE`. */
export const transactionAdded = (tranId: string): string =>
  `Transaction added successfully.  Your Tran ID is ${tranId.trimEnd()}.`;

/** `COBIL00C`. */
export const billPayMessages = {
  acctIdEmpty: "Acct ID can NOT be empty...",
  acctIdNotFound: "Account ID NOT found...",
  acctLookupFailed: "Unable to lookup Account...",
  xrefLookupFailed: "Unable to lookup XREF AIX file...",
  nothingToPay: "You have nothing to pay...",
  confirmPayment: "Confirm to make a bill payment...",
  invalidConfirm: "Invalid value. Valid values are (Y/N)...",
  tranIdExists: "Tran ID already exist...",
  addFailed: "Unable to Add Bill pay Transaction...",
  updateFailed: "Unable to Update Account...",
  tranIdNotFound: "Transaction ID NOT found...",
  lookupFailed: "Unable to lookup Transaction...",
} as const;

/** `WRITE-TRANSACT-FILE` of `COBIL00C`. */
export const paymentSuccessful = (tranId: string): string =>
  `Payment successful.  Your Transaction ID is ${tranId.trimEnd()}.`;

/** `CORPT00C`. */
export const reportMessages = {
  selectReportType: "Select a report type to print report...",
  startMonthEmpty: "Start Date - Month can NOT be empty...",
  startDayEmpty: "Start Date - Day can NOT be empty...",
  startYearEmpty: "Start Date - Year can NOT be empty...",
  endMonthEmpty: "End Date - Month can NOT be empty...",
  endDayEmpty: "End Date - Day can NOT be empty...",
  endYearEmpty: "End Date - Year can NOT be empty...",
  startMonthInvalid: "Start Date - Not a valid Month...",
  startDayInvalid: "Start Date - Not a valid Day...",
  startYearInvalid: "Start Date - Not a valid Year...",
  endMonthInvalid: "End Date - Not a valid Month...",
  endDayInvalid: "End Date - Not a valid Day...",
  endYearInvalid: "End Date - Not a valid Year...",
  startDateInvalid: "Start Date - Not a valid date...",
  endDateInvalid: "End Date - Not a valid date...",
  writeQueueFailed: "Unable to Write TDQ (JOBS)...",
} as const;

/** `SUBMIT-JOB-TO-INTRDR`; the report name is `DELIMITED BY SPACE`. */
export const confirmReport = (reportName: string): string =>
  `Please confirm to print the ${reportName.trimEnd()} report...`;

/** `SUBMIT-JOB-TO-INTRDR`; the typed value is `DELIMITED BY SPACE`. */
export const invalidConfirmValue = (value: string): string =>
  `"${value.trimEnd()}" is not a valid value to confirm...`;

/** `PROCESS-ENTER-KEY` of `CORPT00C`. */
export const reportSubmitted = (reportName: string): string =>
  `${reportName.trimEnd()} report submitted for printing ...`;
