/**
 * `COACTVWC` — view an account: read the card cross reference by account, the
 * account master and then the customer master, and paint `CACTVWA`.
 */

import type { AccountRecord, CustomerRecord } from "@carddemo/domain";
import type { FieldFlag } from "@carddemo/utilities";
import { FileStatus, type FileStatusCode } from "@carddemo/vsam";

import {
  emptyCommarea,
  exitTarget,
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
  XREF_ACCT_PATH_NAME,
  type AccountFiles,
} from "./data.js";
import { formatCurrency, isBlank, isNumericField, ReturnMessage } from "./edits.js";
import {
  accountNotFound,
  customerNotFound,
  fileErrorMessage,
  viewMessages,
  xrefAccountNotFound,
} from "./messages.js";

/** `WS-FILE-ERROR-MESSAGE` for a read that failed for a reason other than NOTFND. */
function readErrorMessage(file: string, status: FileStatusCode): string {
  return fileErrorMessage("READ", file, Number(status), 0);
}

export const VIEW_PROGRAM = "COACTVWC";
export const VIEW_TRANID = "CAVW";
export const VIEW_MAPSET = "COACTVW";
export const VIEW_MAP = "CACTVWA";

/** The unprotected fields of `CACTVWA`. */
export interface AccountViewScreenInput {
  /** `ACCTSID`, `PIC X(11)`. */
  readonly acctsid: string;
}

/** Every field `COACTVWC` writes to `CACTVWAO`. */
export interface AccountViewScreenOutput {
  readonly acctsid: string;
  readonly acsttus: string;
  readonly acurbal: string;
  readonly acrdlim: string;
  readonly acshlim: string;
  readonly acrcycr: string;
  readonly acrcydb: string;
  readonly adtopen: string;
  readonly aexpdt: string;
  readonly areisdt: string;
  readonly aaddgrp: string;
  readonly acstnum: string;
  readonly acstssn: string;
  readonly acstfco: string;
  readonly acstdob: string;
  readonly acsfnam: string;
  readonly acsmnam: string;
  readonly acslnam: string;
  readonly acsadl1: string;
  readonly acsadl2: string;
  readonly acscity: string;
  readonly acsstte: string;
  readonly acszipc: string;
  readonly acsctry: string;
  readonly acsphn1: string;
  readonly acsphn2: string;
  readonly acsgovt: string;
  readonly acseftc: string;
  readonly acspflg: string;
  readonly infomsg: string;
  readonly errmsg: string;
}

export interface AccountViewRequest {
  readonly aid: AidKey;
  readonly screen: AccountViewScreenInput;
  /** `DFHCOMMAREA`; `null` stands for `EIBCALEN = 0`. */
  readonly commarea: CardDemoCommarea | null;
}

export interface AccountViewResponse {
  /** The map that was sent, or `null` when the program transferred control. */
  readonly screen: AccountViewScreenOutput | null;
  readonly commarea: CardDemoCommarea;
  readonly control: ScreenControl;
}

const emptyScreen: AccountViewScreenOutput = {
  acctsid: "",
  acsttus: "",
  acurbal: "",
  acrdlim: "",
  acshlim: "",
  acrcycr: "",
  acrcydb: "",
  adtopen: "",
  aexpdt: "",
  areisdt: "",
  aaddgrp: "",
  acstnum: "",
  acstssn: "",
  acstfco: "",
  acstdob: "",
  acsfnam: "",
  acsmnam: "",
  acslnam: "",
  acsadl1: "",
  acsadl2: "",
  acscity: "",
  acsstte: "",
  acszipc: "",
  acsctry: "",
  acsphn1: "",
  acsphn2: "",
  acsgovt: "",
  acseftc: "",
  acspflg: "",
  infomsg: "",
  errmsg: "",
};

/** `CUST-SSN(1:3)'-'CUST-SSN(4:2)'-'CUST-SSN(6:4)`. */
export function formatSsn(ssn: number): string {
  const digits = String(ssn).padStart(9, "0");
  return `${digits.slice(0, 3)}-${digits.slice(3, 5)}-${digits.slice(5, 9)}`;
}

export function handleAccountView(
  request: AccountViewRequest,
  files: AccountFiles,
): AccountViewResponse {
  const returnMessage = new ReturnMessage();

  // 0000-MAIN: initialize the commarea on a cold start or a fresh menu entry.
  const freshEntry =
    request.commarea === null ||
    (request.commarea.fromProgram.trim() === "COMEN01C" &&
      request.commarea.programContext !== "REENTER");
  let commarea: CardDemoCommarea =
    request.commarea === null || freshEntry ? emptyCommarea : request.commarea;

  // Only ENTER and PF03 are valid here; anything else is remapped to ENTER.
  const aid: AidKey = request.aid === "ENTER" || request.aid === "PFK03" ? request.aid : "ENTER";

  if (aid === "PFK03") {
    const target = exitTarget(commarea);
    commarea = {
      ...commarea,
      toProgram: target.toProgram,
      toTranid: target.toTranid,
      fromProgram: VIEW_PROGRAM,
      fromTranid: VIEW_TRANID,
      userType: "U",
      programContext: "ENTER",
      lastMapset: VIEW_MAPSET,
      lastMap: VIEW_MAP,
    };
    return {
      screen: null,
      commarea,
      control: {
        nextProgram: target.toProgram,
        nextMapset: "",
        nextMap: "",
        errorMessage: "",
        transferControl: true,
      },
    };
  }

  if (commarea.programContext === "ENTER") {
    // Coming from another context: gather the selection criteria.
    return sendMap({
      commarea,
      cold: request.commarea === null,
      acctFilter: "blank",
      acctId: "",
      account: null,
      customer: null,
      returnMessage,
    });
  }

  // 2000-PROCESS-INPUTS
  const acctId =
    request.screen.acctsid.trim() === "*" || isBlank(request.screen.acctsid)
      ? ""
      : request.screen.acctsid;

  let acctFilter: FieldFlag = "valid";
  if (isBlank(acctId)) {
    // 2210-EDIT-ACCOUNT sets the prompt, then the cross field edit in
    // 2200-EDIT-MAP-INPUTS overrides it with NO-SEARCH-CRITERIA-RECEIVED.
    acctFilter = "blank";
    commarea = { ...commarea, acctId: 0 };
    returnMessage.set(viewMessages.noSearchCriteria);
  } else if (!isNumericField(acctId, 11) || Number(acctId) === 0) {
    acctFilter = "invalid";
    returnMessage.set(viewMessages.acctNotElevenDigits);
    commarea = { ...commarea, acctId: 0 };
  } else {
    commarea = { ...commarea, acctId: Number(acctId) };
  }

  if (acctFilter !== "valid") {
    return sendMap({
      commarea,
      cold: false,
      acctFilter,
      acctId,
      account: null,
      customer: null,
      returnMessage,
    });
  }

  // 9000-READ-ACCT
  let account: AccountRecord | null = null;
  let customer: CustomerRecord | null = null;

  const acctKey = acctId.padStart(11, "0");
  const custKey = () => String(commarea.custId).padStart(9, "0");

  const xref = readCardXrefByAccount(files, commarea.acctId);
  if (xref.status !== FileStatus.ok || xref.record === undefined) {
    acctFilter = "invalid";
    returnMessage.set(
      xref.status === FileStatus.notFound
        ? xrefAccountNotFound(acctKey)
        : readErrorMessage(XREF_ACCT_PATH_NAME, xref.status),
    );
  } else {
    commarea = { ...commarea, custId: xref.record.xrefCustId, cardNum: xref.record.xrefCardNum };

    const acctRead = readAccount(files, commarea.acctId);
    if (acctRead.status !== FileStatus.ok || acctRead.record === undefined) {
      acctFilter = "invalid";
      returnMessage.set(
        acctRead.status === FileStatus.notFound
          ? accountNotFound(acctKey)
          : readErrorMessage(ACCT_FILE_NAME, acctRead.status),
      );
    } else {
      account = acctRead.record;
      const custRead = readCustomer(files, commarea.custId);
      if (custRead.status !== FileStatus.ok || custRead.record === undefined) {
        acctFilter = "invalid";
        returnMessage.set(
          custRead.status === FileStatus.notFound
            ? customerNotFound(custKey())
            : readErrorMessage(CUST_FILE_NAME, custRead.status),
        );
      } else {
        customer = custRead.record;
      }
    }
  }

  return sendMap({
    commarea,
    cold: false,
    acctFilter,
    acctId,
    account,
    customer,
    returnMessage,
  });
}

interface SendMapInput {
  readonly commarea: CardDemoCommarea;
  readonly cold: boolean;
  readonly acctFilter: FieldFlag;
  readonly acctId: string;
  readonly account: AccountRecord | null;
  readonly customer: CustomerRecord | null;
  readonly returnMessage: ReturnMessage;
}

/** `1000-SEND-MAP`. */
function sendMap(input: SendMapInput): AccountViewResponse {
  let screen: AccountViewScreenOutput = {
    ...emptyScreen,
    infomsg: viewMessages.promptForInput,
    errmsg: input.returnMessage.value,
  };

  if (!input.cold) {
    screen = {
      ...screen,
      // 1300-SETUP-SCREEN-ATTRS flags a blank filter with '*' on re-entry.
      acctsid: input.acctFilter === "blank" ? "*" : input.acctId,
    };

    if (input.account !== null || input.customer !== null) {
      const account = input.account;
      if (account !== null) {
        screen = {
          ...screen,
          acsttus: account.acctActiveStatus,
          acurbal: formatCurrency(account.acctCurrBal),
          acrdlim: formatCurrency(account.acctCreditLimit),
          acshlim: formatCurrency(account.acctCashCreditLimit),
          acrcycr: formatCurrency(account.acctCurrCycCredit),
          acrcydb: formatCurrency(account.acctCurrCycDebit),
          adtopen: account.acctOpenDate,
          aexpdt: account.acctExpiraionDate,
          areisdt: account.acctReissueDate,
          aaddgrp: account.acctGroupId,
        };
      }
    }

    const customer = input.customer;
    if (customer !== null) {
      screen = {
        ...screen,
        acstnum: String(customer.custId).padStart(9, "0"),
        acstssn: formatSsn(customer.custSsn),
        acstfco: String(customer.custFicoCreditScore).padStart(3, "0"),
        acstdob: customer.custDobYyyyMmDd,
        acsfnam: customer.custFirstName,
        acsmnam: customer.custMiddleName,
        acslnam: customer.custLastName,
        acsadl1: customer.custAddrLine1,
        acsadl2: customer.custAddrLine2,
        acscity: customer.custAddrLine3,
        acsstte: customer.custAddrStateCd,
        acszipc: customer.custAddrZip,
        acsctry: customer.custAddrCountryCd,
        acsphn1: customer.custPhoneNum1,
        acsphn2: customer.custPhoneNum2,
        acsgovt: customer.custGovtIssuedId,
        acseftc: customer.custEftAccountId,
        acspflg: customer.custPriCardHolderInd,
      };
    }
  }

  // 1300-SETUP-SCREEN-ATTRS: a blank filter on re-entry is echoed as '*'.
  if (input.acctFilter === "blank" && input.commarea.programContext === "REENTER") {
    screen = { ...screen, acctsid: "*" };
  }

  return {
    screen,
    commarea: { ...input.commarea, programContext: "REENTER" },
    control: {
      nextProgram: VIEW_PROGRAM,
      nextMapset: VIEW_MAPSET,
      nextMap: VIEW_MAP,
      errorMessage: input.returnMessage.value,
      transferControl: false,
    },
  };
}
