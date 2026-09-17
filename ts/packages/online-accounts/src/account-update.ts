/**
 * `COACTUPC` — update an account: fetch the account and its customer, edit
 * every field the user can change, ask for confirmation, and rewrite both
 * records only when nobody else changed them in the meantime.
 */

import type { AccountRecord, CustomerRecord } from "@carddemo/domain";
import { editBirthDateCcyymmdd, editDateCcyymmdd, type FieldFlag } from "@carddemo/utilities";
import { FileStatus, type FileStatusCode } from "@carddemo/vsam";

import {
  emptyCommarea,
  exitTarget,
  MENU_PROGRAM,
  type AidKey,
  type CardDemoCommarea,
  type ScreenControl,
} from "./commarea.js";
import {
  ACCT_FILE_NAME,
  CUST_FILE_NAME,
  readAccount,
  readCardXrefByAccount,
  readCustomer,
  rewriteAccount,
  rewriteCustomer,
  XREF_ACCT_PATH_NAME,
  type AccountFiles,
} from "./data.js";
import {
  editAlphaOptional,
  editAlphaRequired,
  editFicoScore,
  editMandatory,
  editNumRequired,
  editSigned9v2,
  editStateZip,
  editUsPhone,
  editUsSsn,
  editUsStateCode,
  editYesNo,
  formatCurrency,
  isBlank,
  isNumericField,
  numvalC,
  ReturnMessage,
  testNumvalC,
} from "./edits.js";
import {
  accountNotFound,
  customerNotFound,
  fileErrorMessage,
  updateMessages,
  xrefAccountNotFound,
} from "./messages.js";

/** `WS-FILE-ERROR-MESSAGE` for a read that failed for a reason other than NOTFND. */
function readErrorMessage(file: string, status: FileStatusCode): string {
  return fileErrorMessage("READ", file, Number(status), 0);
}

export const UPDATE_PROGRAM = "COACTUPC";
export const UPDATE_TRANID = "CAUP";
export const UPDATE_MAPSET = "COACTUP";
export const UPDATE_MAP = "CACTUPA";

/** `ACUP-CHANGE-ACTION`. */
export type AccountUpdateAction =
  /** `ACUP-DETAILS-NOT-FETCHED`, `LOW-VALUES`. */
  | "DETAILS-NOT-FETCHED"
  /** `ACUP-SHOW-DETAILS`, `'S'`. */
  | "SHOW-DETAILS"
  /** `ACUP-CHANGES-NOT-OK`, `'E'`. */
  | "CHANGES-NOT-OK"
  /** `ACUP-CHANGES-OK-NOT-CONFIRMED`, `'N'`. */
  | "CHANGES-OK-NOT-CONFIRMED"
  /** `ACUP-CHANGES-OKAYED-AND-DONE`, `'C'`. */
  | "CHANGES-OKAYED-AND-DONE"
  /** `ACUP-CHANGES-OKAYED-LOCK-ERROR`, `'L'`. */
  | "CHANGES-OKAYED-LOCK-ERROR"
  /** `ACUP-CHANGES-OKAYED-BUT-FAILED`, `'F'`. */
  | "CHANGES-OKAYED-BUT-FAILED";

/** `ACUP-OLD-DETAILS`: the image of the records as they were fetched. */
export interface AccountUpdateOldDetails {
  readonly account: AccountRecord;
  readonly customer: CustomerRecord;
}

/** `WS-THIS-PROGCOMMAREA`. */
export interface AccountUpdateState {
  readonly action: AccountUpdateAction;
  readonly old: AccountUpdateOldDetails | null;
}

export const initialAccountUpdateState: AccountUpdateState = {
  action: "DETAILS-NOT-FETCHED",
  old: null,
};

/** The unprotected fields of `CACTUPA`. */
export interface AccountUpdateScreenInput {
  readonly acctsid: string;
  readonly acsttus: string;
  readonly opnyear: string;
  readonly opnmon: string;
  readonly opnday: string;
  readonly acrdlim: string;
  readonly expyear: string;
  readonly expmon: string;
  readonly expday: string;
  readonly acshlim: string;
  readonly risyear: string;
  readonly rismon: string;
  readonly risday: string;
  readonly acurbal: string;
  readonly acrcycr: string;
  readonly aaddgrp: string;
  readonly acrcydb: string;
  readonly acstnum: string;
  readonly actssn1: string;
  readonly actssn2: string;
  readonly actssn3: string;
  readonly dobyear: string;
  readonly dobmon: string;
  readonly dobday: string;
  readonly acstfco: string;
  readonly acsfnam: string;
  readonly acsmnam: string;
  readonly acslnam: string;
  readonly acsadl1: string;
  readonly acsstte: string;
  readonly acsadl2: string;
  readonly acszipc: string;
  readonly acscity: string;
  readonly acsctry: string;
  readonly acsph1a: string;
  readonly acsph1b: string;
  readonly acsph1c: string;
  readonly acsgovt: string;
  readonly acsph2a: string;
  readonly acsph2b: string;
  readonly acsph2c: string;
  readonly acseftc: string;
  readonly acspflg: string;
}

export interface AccountUpdateScreenOutput extends AccountUpdateScreenInput {
  readonly infomsg: string;
  readonly errmsg: string;
}

export interface AccountUpdateRequest {
  readonly aid: AidKey;
  readonly screen: AccountUpdateScreenInput;
  /** `DFHCOMMAREA`; `null` stands for `EIBCALEN = 0`. */
  readonly commarea: CardDemoCommarea | null;
  readonly state: AccountUpdateState;
}

export interface AccountUpdateResponse {
  readonly screen: AccountUpdateScreenOutput | null;
  readonly commarea: CardDemoCommarea;
  readonly state: AccountUpdateState;
  readonly control: ScreenControl;
}

export const emptyAccountUpdateScreen: AccountUpdateScreenInput = {
  acctsid: "",
  acsttus: "",
  opnyear: "",
  opnmon: "",
  opnday: "",
  acrdlim: "",
  expyear: "",
  expmon: "",
  expday: "",
  acshlim: "",
  risyear: "",
  rismon: "",
  risday: "",
  acurbal: "",
  acrcycr: "",
  aaddgrp: "",
  acrcydb: "",
  acstnum: "",
  actssn1: "",
  actssn2: "",
  actssn3: "",
  dobyear: "",
  dobmon: "",
  dobday: "",
  acstfco: "",
  acsfnam: "",
  acsmnam: "",
  acslnam: "",
  acsadl1: "",
  acsstte: "",
  acsadl2: "",
  acszipc: "",
  acscity: "",
  acsctry: "",
  acsph1a: "",
  acsph1b: "",
  acsph1c: "",
  acsgovt: "",
  acsph2a: "",
  acsph2b: "",
  acsph2c: "",
  acseftc: "",
  acspflg: "",
};

/** A money field of `ACUP-NEW-DETAILS`: the keyed text plus its `NUMVAL-C`. */
interface MoneyInput {
  readonly text: string;
  readonly value: number | null;
}

/** `ACUP-NEW-DETAILS`, as received from the map. */
interface NewDetails {
  readonly acctId: string;
  readonly activeStatus: string;
  readonly currBal: MoneyInput;
  readonly creditLimit: MoneyInput;
  readonly cashCreditLimit: MoneyInput;
  readonly currCycCredit: MoneyInput;
  readonly currCycDebit: MoneyInput;
  readonly openDate: string;
  readonly expiryDate: string;
  readonly reissueDate: string;
  readonly groupId: string;
  readonly custId: string;
  readonly firstName: string;
  readonly middleName: string;
  readonly lastName: string;
  readonly addrLine1: string;
  readonly addrLine2: string;
  readonly addrLine3: string;
  readonly stateCd: string;
  readonly countryCd: string;
  readonly zip: string;
  readonly phone1a: string;
  readonly phone1b: string;
  readonly phone1c: string;
  readonly phone2a: string;
  readonly phone2b: string;
  readonly phone2c: string;
  readonly ssn1: string;
  readonly ssn2: string;
  readonly ssn3: string;
  readonly govtIssuedId: string;
  readonly dob: string;
  readonly eftAccountId: string;
  readonly priHolderInd: string;
  readonly ficoScore: string;
}

/** The map returns `'*'` and spaces as `LOW-VALUES`. */
function received(value: string): string {
  return value.trim() === "*" || isBlank(value) ? "" : value;
}

function money(value: string): MoneyInput {
  const text = received(value);
  return { text, value: text.length > 0 && testNumvalC(text) === 0 ? numvalC(text) : null };
}

/** `CCYY-MM-DD`, the form both records store dates in. */
function joinDate(year: string, month: string, day: string): string {
  if (year.length === 0 && month.length === 0 && day.length === 0) {
    return "";
  }
  return `${year}-${month}-${day}`;
}

function dateDigits(date: string): string {
  return date.replace(/-/g, "");
}

function phoneParts(phone: string): { a: string; b: string; c: string } {
  const match = /^\((\d{3})\)(\d{3})-(\d{4})/.exec(phone.trim());
  if (match === null) {
    return { a: "", b: "", c: "" };
  }
  return { a: match[1] as string, b: match[2] as string, c: match[3] as string };
}

function same(left: string, right: string): boolean {
  return left.trim().toUpperCase() === right.trim().toUpperCase();
}

/** `1100-RECEIVE-MAP`. */
function receiveMap(screen: AccountUpdateScreenInput): NewDetails {
  return {
    acctId: received(screen.acctsid),
    activeStatus: received(screen.acsttus),
    currBal: money(screen.acurbal),
    creditLimit: money(screen.acrdlim),
    cashCreditLimit: money(screen.acshlim),
    currCycCredit: money(screen.acrcycr),
    currCycDebit: money(screen.acrcydb),
    openDate: joinDate(received(screen.opnyear), received(screen.opnmon), received(screen.opnday)),
    expiryDate: joinDate(received(screen.expyear), received(screen.expmon), received(screen.expday)),
    reissueDate: joinDate(
      received(screen.risyear),
      received(screen.rismon),
      received(screen.risday),
    ),
    groupId: received(screen.aaddgrp),
    custId: received(screen.acstnum),
    firstName: received(screen.acsfnam),
    middleName: received(screen.acsmnam),
    lastName: received(screen.acslnam),
    addrLine1: received(screen.acsadl1),
    addrLine2: received(screen.acsadl2),
    addrLine3: received(screen.acscity),
    stateCd: received(screen.acsstte),
    countryCd: received(screen.acsctry),
    zip: received(screen.acszipc),
    phone1a: received(screen.acsph1a),
    phone1b: received(screen.acsph1b),
    phone1c: received(screen.acsph1c),
    phone2a: received(screen.acsph2a),
    phone2b: received(screen.acsph2b),
    phone2c: received(screen.acsph2c),
    ssn1: received(screen.actssn1),
    ssn2: received(screen.actssn2),
    ssn3: received(screen.actssn3),
    govtIssuedId: received(screen.acsgovt),
    dob: joinDate(received(screen.dobyear), received(screen.dobmon), received(screen.dobday)),
    eftAccountId: received(screen.acseftc),
    priHolderInd: received(screen.acspflg),
    ficoScore: received(screen.acstfco),
  };
}

/** `1205-COMPARE-OLD-NEW`. */
function changeHasOccurred(details: NewDetails, old: AccountUpdateOldDetails): boolean {
  const account = old.account;
  const customer = old.customer;
  const phone1 = phoneParts(customer.custPhoneNum1);
  const phone2 = phoneParts(customer.custPhoneNum2);
  const ssn = String(customer.custSsn).padStart(9, "0");

  const unchanged =
    details.acctId === String(account.acctId).padStart(11, "0") &&
    same(details.activeStatus, account.acctActiveStatus) &&
    details.currBal.value === account.acctCurrBal &&
    details.creditLimit.value === account.acctCreditLimit &&
    details.cashCreditLimit.value === account.acctCashCreditLimit &&
    details.currCycCredit.value === account.acctCurrCycCredit &&
    details.currCycDebit.value === account.acctCurrCycDebit &&
    details.openDate === account.acctOpenDate.trim() &&
    details.expiryDate === account.acctExpiraionDate.trim() &&
    details.reissueDate === account.acctReissueDate.trim() &&
    same(details.groupId, account.acctGroupId) &&
    details.custId === String(customer.custId).padStart(9, "0") &&
    same(details.firstName, customer.custFirstName) &&
    same(details.middleName, customer.custMiddleName) &&
    same(details.lastName, customer.custLastName) &&
    same(details.addrLine1, customer.custAddrLine1) &&
    same(details.addrLine2, customer.custAddrLine2) &&
    same(details.addrLine3, customer.custAddrLine3) &&
    same(details.stateCd, customer.custAddrStateCd) &&
    same(details.countryCd, customer.custAddrCountryCd) &&
    same(details.zip, customer.custAddrZip) &&
    details.phone1a === phone1.a &&
    details.phone1b === phone1.b &&
    details.phone1c === phone1.c &&
    details.phone2a === phone2.a &&
    details.phone2b === phone2.b &&
    details.phone2c === phone2.c &&
    `${details.ssn1}${details.ssn2}${details.ssn3}` === ssn &&
    same(details.govtIssuedId, customer.custGovtIssuedId) &&
    details.dob === customer.custDobYyyyMmDd.trim() &&
    details.eftAccountId === customer.custEftAccountId.trim() &&
    same(details.priHolderInd, customer.custPriCardHolderInd) &&
    details.ficoScore === String(customer.custFicoCreditScore).padStart(3, "0");

  return !unchanged;
}

interface EditOutcome {
  readonly inputError: boolean;
  readonly message: string;
}

/** `1200-EDIT-MAP-INPUTS` for the detail fields. */
function editDetails(details: NewDetails, systemDate: Date | undefined): EditOutcome {
  const message = new ReturnMessage();
  let inputError = false;

  const record = (result: { flag: FieldFlag; message: string }): FieldFlag => {
    if (result.message.length > 0) {
      inputError = true;
      message.set(result.message);
    }
    return result.flag;
  };

  const dateOptions = (name: string): { variableName: string; systemDate?: Date } =>
    systemDate === undefined ? { variableName: name } : { variableName: name, systemDate };

  record(editYesNo("Account Status", details.activeStatus));

  const openDate = editDateCcyymmdd(dateDigits(details.openDate), dateOptions("Open Date"));
  if (!openDate.valid) {
    inputError = true;
    message.set(openDate.message);
  }

  record(editSigned9v2("Credit Limit", details.creditLimit.text));

  const expiryDate = editDateCcyymmdd(dateDigits(details.expiryDate), dateOptions("Expiry Date"));
  if (!expiryDate.valid) {
    inputError = true;
    message.set(expiryDate.message);
  }

  record(editSigned9v2("Cash Credit Limit", details.cashCreditLimit.text));

  const reissueDate = editDateCcyymmdd(
    dateDigits(details.reissueDate),
    dateOptions("Reissue Date"),
  );
  if (!reissueDate.valid) {
    inputError = true;
    message.set(reissueDate.message);
  }

  record(editSigned9v2("Current Balance", details.currBal.text));
  record(editSigned9v2("Current Cycle Credit Limit", details.currCycCredit.text));
  record(editSigned9v2("Current Cycle Debit Limit", details.currCycDebit.text));

  const ssn = editUsSsn({ part1: details.ssn1, part2: details.ssn2, part3: details.ssn3 });
  if (ssn.messages.length > 0) {
    inputError = true;
    message.setAll(ssn.messages);
  }

  const birthDate = editBirthDateCcyymmdd(dateDigits(details.dob), dateOptions("Date of Birth"));
  if (!birthDate.valid) {
    inputError = true;
    message.set(birthDate.message);
  }

  const fico = record(editNumRequired("FICO Score", details.ficoScore, 3));
  if (fico === "valid") {
    record(editFicoScore("FICO Score", details.ficoScore));
  }

  record(editAlphaRequired("First Name", details.firstName));
  record(editAlphaOptional("Middle Name", details.middleName));
  record(editAlphaRequired("Last Name", details.lastName));
  record(editMandatory("Address Line 1", details.addrLine1));

  const state = record(editAlphaRequired("State", details.stateCd));
  if (state === "valid") {
    record(editUsStateCode("State", details.stateCd));
  }

  const zip = record(editNumRequired("Zip", details.zip, 5));

  record(editAlphaRequired("City", details.addrLine3));
  record(editAlphaRequired("Country", details.countryCd));

  const phone1 = editUsPhone("Phone Number 1", {
    areaCode: details.phone1a,
    prefix: details.phone1b,
    lineNumber: details.phone1c,
  });
  if (phone1.messages.length > 0) {
    inputError = true;
    message.setAll(phone1.messages);
  }

  const phone2 = editUsPhone("Phone Number 2", {
    areaCode: details.phone2a,
    prefix: details.phone2b,
    lineNumber: details.phone2c,
  });
  if (phone2.messages.length > 0) {
    inputError = true;
    message.setAll(phone2.messages);
  }

  record(editNumRequired("EFT Account Id", details.eftAccountId, 10));
  record(editYesNo("Primary Card Holder", details.priHolderInd));

  if (state === "valid" && zip === "valid") {
    record(editStateZip(details.stateCd, details.zip));
  }

  return { inputError, message: message.value };
}

interface ReadOutcome {
  readonly old: AccountUpdateOldDetails | null;
  readonly commarea: CardDemoCommarea;
  readonly message: string;
}

/** `9000-READ-ACCT`. */
function readAccountDetails(
  files: AccountFiles,
  commarea: CardDemoCommarea,
): ReadOutcome {
  const acctKey = String(commarea.acctId).padStart(11, "0");

  const xref = readCardXrefByAccount(files, commarea.acctId);
  if (xref.status !== FileStatus.ok || xref.record === undefined) {
    return {
      old: null,
      commarea,
      message:
        xref.status === FileStatus.notFound
          ? xrefAccountNotFound(acctKey)
          : readErrorMessage(XREF_ACCT_PATH_NAME, xref.status),
    };
  }

  const withXref: CardDemoCommarea = {
    ...commarea,
    custId: xref.record.xrefCustId,
    cardNum: xref.record.xrefCardNum,
  };

  const account = readAccount(files, withXref.acctId);
  if (account.status !== FileStatus.ok || account.record === undefined) {
    return {
      old: null,
      commarea: withXref,
      message:
        account.status === FileStatus.notFound
          ? accountNotFound(acctKey)
          : readErrorMessage(ACCT_FILE_NAME, account.status),
    };
  }

  const customer = readCustomer(files, withXref.custId);
  if (customer.status !== FileStatus.ok || customer.record === undefined) {
    return {
      old: null,
      commarea: withXref,
      message:
        customer.status === FileStatus.notFound
          ? customerNotFound(String(withXref.custId).padStart(9, "0"))
          : readErrorMessage(CUST_FILE_NAME, customer.status),
    };
  }

  return {
    old: { account: account.record, customer: customer.record },
    commarea: {
      ...withXref,
      acctStatus: account.record.acctActiveStatus,
      custFname: customer.record.custFirstName,
      custMname: customer.record.custMiddleName,
      custLname: customer.record.custLastName,
    },
    message: "",
  };
}

interface WriteOutcome {
  readonly message: string;
  readonly committed: boolean;
}

/** `9700-CHECK-CHANGE-IN-REC`. */
function recordChangedSinceFetch(
  current: AccountUpdateOldDetails,
  old: AccountUpdateOldDetails,
): boolean {
  const a = current.account;
  const oa = old.account;
  const c = current.customer;
  const oc = old.customer;

  const accountSame =
    a.acctActiveStatus === oa.acctActiveStatus &&
    a.acctCurrBal === oa.acctCurrBal &&
    a.acctCreditLimit === oa.acctCreditLimit &&
    a.acctCashCreditLimit === oa.acctCashCreditLimit &&
    a.acctCurrCycCredit === oa.acctCurrCycCredit &&
    a.acctCurrCycDebit === oa.acctCurrCycDebit &&
    a.acctOpenDate === oa.acctOpenDate &&
    a.acctExpiraionDate === oa.acctExpiraionDate &&
    a.acctReissueDate === oa.acctReissueDate &&
    a.acctGroupId === oa.acctGroupId;

  const customerSame =
    c.custFirstName === oc.custFirstName &&
    c.custMiddleName === oc.custMiddleName &&
    c.custLastName === oc.custLastName &&
    c.custAddrLine1 === oc.custAddrLine1 &&
    c.custAddrLine2 === oc.custAddrLine2 &&
    c.custAddrLine3 === oc.custAddrLine3 &&
    c.custAddrStateCd === oc.custAddrStateCd &&
    c.custAddrCountryCd === oc.custAddrCountryCd &&
    c.custAddrZip === oc.custAddrZip &&
    c.custPhoneNum1 === oc.custPhoneNum1 &&
    c.custPhoneNum2 === oc.custPhoneNum2 &&
    c.custSsn === oc.custSsn &&
    c.custGovtIssuedId === oc.custGovtIssuedId &&
    c.custDobYyyyMmDd === oc.custDobYyyyMmDd &&
    c.custEftAccountId === oc.custEftAccountId &&
    c.custPriCardHolderInd === oc.custPriCardHolderInd &&
    c.custFicoCreditScore === oc.custFicoCreditScore;

  return !(accountSame && customerSame);
}

/** `9600-WRITE-PROCESSING`. */
function writeProcessing(
  files: AccountFiles,
  details: NewDetails,
  old: AccountUpdateOldDetails,
  commarea: CardDemoCommarea,
): WriteOutcome {
  const account = readAccount(files, commarea.acctId);
  if (account.status !== FileStatus.ok || account.record === undefined) {
    return { message: updateMessages.couldNotLockAccount, committed: false };
  }

  const customer = readCustomer(files, commarea.custId);
  if (customer.status !== FileStatus.ok || customer.record === undefined) {
    return { message: updateMessages.couldNotLockCustomer, committed: false };
  }

  const current: AccountUpdateOldDetails = {
    account: account.record,
    customer: customer.record,
  };
  if (recordChangedSinceFetch(current, old)) {
    return { message: updateMessages.recordChangedByAnotherUser, committed: false };
  }

  const accountUpdate: AccountRecord = {
    acctId: Number(details.acctId),
    acctActiveStatus: details.activeStatus,
    acctCurrBal: details.currBal.value ?? 0,
    acctCreditLimit: details.creditLimit.value ?? 0,
    acctCashCreditLimit: details.cashCreditLimit.value ?? 0,
    acctOpenDate: details.openDate,
    acctExpiraionDate: details.expiryDate,
    acctReissueDate: details.reissueDate,
    acctCurrCycCredit: details.currCycCredit.value ?? 0,
    acctCurrCycDebit: details.currCycDebit.value ?? 0,
    acctAddrZip: account.record.acctAddrZip,
    acctGroupId: details.groupId,
  };

  const customerUpdate: CustomerRecord = {
    custId: Number(details.custId),
    custFirstName: details.firstName,
    custMiddleName: details.middleName,
    custLastName: details.lastName,
    custAddrLine1: details.addrLine1,
    custAddrLine2: details.addrLine2,
    custAddrLine3: details.addrLine3,
    custAddrStateCd: details.stateCd,
    custAddrCountryCd: details.countryCd,
    custAddrZip: details.zip,
    custPhoneNum1: `(${details.phone1a})${details.phone1b}-${details.phone1c}`,
    custPhoneNum2: `(${details.phone2a})${details.phone2b}-${details.phone2c}`,
    custSsn: Number(`${details.ssn1}${details.ssn2}${details.ssn3}`),
    custGovtIssuedId: details.govtIssuedId,
    custDobYyyyMmDd: details.dob,
    custEftAccountId: details.eftAccountId,
    custPriCardHolderInd: details.priHolderInd,
    custFicoCreditScore: Number(details.ficoScore),
  };

  if (rewriteAccount(files, accountUpdate) !== FileStatus.ok) {
    return { message: updateMessages.updateOfRecordFailed, committed: false };
  }

  if (rewriteCustomer(files, customerUpdate) !== FileStatus.ok) {
    // EXEC CICS SYNCPOINT ROLLBACK: undo the account rewrite of this unit of work.
    rewriteAccount(files, account.record);
    return { message: updateMessages.updateOfRecordFailed, committed: false };
  }

  return { message: "", committed: true };
}

export interface AccountUpdateOptions {
  /** System date used by the date of birth edit; defaults to today. */
  readonly systemDate?: Date;
}

export function handleAccountUpdate(
  request: AccountUpdateRequest,
  files: AccountFiles,
  options: AccountUpdateOptions = {},
): AccountUpdateResponse {
  const returnMessage = new ReturnMessage();

  const freshMenuEntry =
    request.commarea !== null &&
    request.commarea.fromProgram.trim() === MENU_PROGRAM &&
    request.commarea.programContext !== "REENTER";
  let commarea: CardDemoCommarea =
    request.commarea === null || freshMenuEntry ? emptyCommarea : request.commarea;
  let state: AccountUpdateState =
    request.commarea === null || freshMenuEntry ? initialAccountUpdateState : request.state;

  // The AID is only honoured in the states that accept it.
  const pfkValid =
    request.aid === "ENTER" ||
    request.aid === "PFK03" ||
    (request.aid === "PFK05" && state.action === "CHANGES-OK-NOT-CONFIRMED") ||
    (request.aid === "PFK12" && state.action !== "DETAILS-NOT-FETCHED");
  const aid: AidKey = pfkValid ? request.aid : "ENTER";

  if (aid === "PFK03") {
    const target = exitTarget(commarea);
    commarea = {
      ...commarea,
      toProgram: target.toProgram,
      toTranid: target.toTranid,
      fromProgram: UPDATE_PROGRAM,
      fromTranid: UPDATE_TRANID,
      userType: "U",
      programContext: "ENTER",
      lastMapset: UPDATE_MAPSET,
      lastMap: UPDATE_MAP,
    };
    return {
      screen: null,
      commarea,
      state,
      control: {
        nextProgram: target.toProgram,
        nextMapset: "",
        nextMap: "",
        errorMessage: "",
        transferControl: true,
      },
    };
  }

  // Fresh entry: ask for the search key.
  if (
    (state.action === "DETAILS-NOT-FETCHED" && commarea.programContext === "ENTER") ||
    freshMenuEntry
  ) {
    state = initialAccountUpdateState;
    return sendMap({ commarea, state, details: null, screen: request.screen, returnMessage });
  }

  // The previous interaction finished the update: reset and start over.
  if (
    state.action === "CHANGES-OKAYED-AND-DONE" ||
    state.action === "CHANGES-OKAYED-LOCK-ERROR" ||
    state.action === "CHANGES-OKAYED-BUT-FAILED"
  ) {
    commarea = { ...commarea, acctId: 0, programContext: "ENTER" };
    state = initialAccountUpdateState;
    return sendMap({
      commarea,
      state,
      details: null,
      screen: emptyAccountUpdateScreen,
      returnMessage,
    });
  }

  // 1000-PROCESS-INPUTS
  const details = receiveMap(request.screen);
  let inputError = false;

  if (state.action === "DETAILS-NOT-FETCHED") {
    // 1210-EDIT-ACCOUNT
    if (details.acctId.length === 0) {
      inputError = true;
      commarea = { ...commarea, acctId: 0 };
      returnMessage.set(updateMessages.noSearchCriteria);
    } else if (!isNumericField(details.acctId, 11) || Number(details.acctId) === 0) {
      inputError = true;
      commarea = { ...commarea, acctId: 0 };
      returnMessage.set(updateMessages.acctNotElevenDigitsNonZero);
    } else {
      commarea = { ...commarea, acctId: Number(details.acctId) };
    }
  } else if (state.old !== null) {
    const changed = changeHasOccurred(details, state.old);
    if (!changed) {
      returnMessage.set(updateMessages.noChangesDetected);
    } else if (state.action === "SHOW-DETAILS" || state.action === "CHANGES-NOT-OK") {
      state = { ...state, action: "CHANGES-NOT-OK" };
      const outcome = editDetails(details, options.systemDate);
      inputError = outcome.inputError;
      returnMessage.set(outcome.message);
      if (!inputError) {
        state = { ...state, action: "CHANGES-OK-NOT-CONFIRMED" };
      }
    }
  }

  // 2000-DECIDE-ACTION
  if (state.action === "DETAILS-NOT-FETCHED" || aid === "PFK12") {
    if (commarea.acctId !== 0 && !inputError) {
      const outcome = readAccountDetails(files, commarea);
      commarea = outcome.commarea;
      if (outcome.old === null) {
        returnMessage.set(outcome.message);
      } else {
        state = { action: "SHOW-DETAILS", old: outcome.old };
      }
    }
  } else if (state.action === "CHANGES-OK-NOT-CONFIRMED" && aid === "PFK05" && state.old !== null) {
    const outcome = writeProcessing(files, details, state.old, commarea);
    if (outcome.committed) {
      state = { ...state, action: "CHANGES-OKAYED-AND-DONE" };
    } else if (outcome.message === updateMessages.recordChangedByAnotherUser) {
      returnMessage.set(outcome.message);
      state = { ...state, action: "SHOW-DETAILS" };
    } else if (
      outcome.message === updateMessages.couldNotLockAccount ||
      outcome.message === updateMessages.couldNotLockCustomer
    ) {
      returnMessage.set(outcome.message);
      state = { ...state, action: "CHANGES-OKAYED-LOCK-ERROR" };
    } else {
      returnMessage.set(outcome.message);
      state = { ...state, action: "CHANGES-OKAYED-BUT-FAILED" };
    }
  }

  return sendMap({ commarea, state, details, screen: request.screen, returnMessage });
}

interface SendMapInput {
  readonly commarea: CardDemoCommarea;
  readonly state: AccountUpdateState;
  readonly details: NewDetails | null;
  readonly screen: AccountUpdateScreenInput;
  readonly returnMessage: ReturnMessage;
}

/** `3250-SETUP-INFOMSG`. */
function infoMessage(state: AccountUpdateState): string {
  switch (state.action) {
    case "SHOW-DETAILS":
    case "CHANGES-NOT-OK":
      return updateMessages.promptForChanges;
    case "CHANGES-OK-NOT-CONFIRMED":
      return updateMessages.promptForConfirmation;
    case "CHANGES-OKAYED-AND-DONE":
      return updateMessages.updateSuccess;
    case "CHANGES-OKAYED-LOCK-ERROR":
    case "CHANGES-OKAYED-BUT-FAILED":
      return updateMessages.updateFailure;
    default:
      return updateMessages.promptForSearchKeys;
  }
}

/** Only the input fields of the map, so a response cannot feed its own messages back. */
function fieldsOf(screen: AccountUpdateScreenInput): AccountUpdateScreenInput {
  const fields: { -readonly [K in keyof AccountUpdateScreenInput]: string } = {
    ...emptyAccountUpdateScreen,
  };
  for (const name of Object.keys(fields) as (keyof AccountUpdateScreenInput)[]) {
    fields[name] = screen[name];
  }
  return fields;
}

/** `3000-SEND-MAP`. */
function sendMap(input: SendMapInput): AccountUpdateResponse {
  const old = input.state.old;
  let screen: AccountUpdateScreenOutput = { ...emptyAccountUpdateScreen, infomsg: "", errmsg: "" };

  if (input.state.action === "SHOW-DETAILS" && old !== null) {
    // 3200-SETUP-SCREEN-VARS: show the values as fetched.
    screen = { ...screen, ...screenFromRecords(old) };
  } else if (input.state.action !== "DETAILS-NOT-FETCHED") {
    // Show what the user keyed, so the corrections stay on the screen.
    screen = { ...screen, ...fieldsOf(input.screen) };
  } else {
    screen = { ...screen, acctsid: input.commarea.acctId === 0 ? "" : input.screen.acctsid };
  }

  screen = { ...screen, infomsg: infoMessage(input.state), errmsg: input.returnMessage.value };

  return {
    screen,
    commarea: { ...input.commarea, programContext: "REENTER" },
    state: input.state,
    control: {
      nextProgram: UPDATE_PROGRAM,
      nextMapset: UPDATE_MAPSET,
      nextMap: UPDATE_MAP,
      errorMessage: input.returnMessage.value,
      transferControl: false,
    },
  };
}

/** The account and customer records as the map shows them. */
export function screenFromRecords(details: AccountUpdateOldDetails): AccountUpdateScreenInput {
  const { account, customer } = details;
  const [openYear = "", openMon = "", openDay = ""] = account.acctOpenDate.trim().split("-");
  const [expYear = "", expMon = "", expDay = ""] = account.acctExpiraionDate.trim().split("-");
  const [risYear = "", risMon = "", risDay = ""] = account.acctReissueDate.trim().split("-");
  const [dobYear = "", dobMon = "", dobDay = ""] = customer.custDobYyyyMmDd.trim().split("-");
  const ssn = String(customer.custSsn).padStart(9, "0");
  const phone1 = phoneParts(customer.custPhoneNum1);
  const phone2 = phoneParts(customer.custPhoneNum2);

  return {
    acctsid: String(account.acctId).padStart(11, "0"),
    acsttus: account.acctActiveStatus,
    opnyear: openYear,
    opnmon: openMon,
    opnday: openDay,
    acrdlim: formatCurrency(account.acctCreditLimit),
    expyear: expYear,
    expmon: expMon,
    expday: expDay,
    acshlim: formatCurrency(account.acctCashCreditLimit),
    risyear: risYear,
    rismon: risMon,
    risday: risDay,
    acurbal: formatCurrency(account.acctCurrBal),
    acrcycr: formatCurrency(account.acctCurrCycCredit),
    aaddgrp: account.acctGroupId,
    acrcydb: formatCurrency(account.acctCurrCycDebit),
    acstnum: String(customer.custId).padStart(9, "0"),
    actssn1: ssn.slice(0, 3),
    actssn2: ssn.slice(3, 5),
    actssn3: ssn.slice(5, 9),
    dobyear: dobYear,
    dobmon: dobMon,
    dobday: dobDay,
    acstfco: String(customer.custFicoCreditScore).padStart(3, "0"),
    acsfnam: customer.custFirstName,
    acsmnam: customer.custMiddleName,
    acslnam: customer.custLastName,
    acsadl1: customer.custAddrLine1,
    acsstte: customer.custAddrStateCd,
    acsadl2: customer.custAddrLine2,
    acszipc: customer.custAddrZip,
    acscity: customer.custAddrLine3,
    acsctry: customer.custAddrCountryCd,
    acsph1a: phone1.a,
    acsph1b: phone1.b,
    acsph1c: phone1.c,
    acsgovt: customer.custGovtIssuedId,
    acsph2a: phone2.a,
    acsph2b: phone2.b,
    acsph2c: phone2.c,
    acseftc: customer.custEftAccountId,
    acspflg: customer.custPriCardHolderInd,
  };
}
