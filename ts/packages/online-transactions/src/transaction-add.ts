/**
 * `COTRN02C` — add a transaction (map `COTRN2A` of mapset `COTRN02`).
 *
 * The account id or the card number identifies the card through `CXACAIX` /
 * `CCXREF`, the remaining fields are edited in the order the COBOL checks
 * them, and a confirmed screen writes a new record to `TRANSACT` under the
 * highest existing `TRAN-ID` plus one.
 *
 * `SEND-TRNADD-SCREEN` ends with `EXEC CICS RETURN`, so the first failing
 * edit ends the task: the handler returns on the first message too.
 */

import type { TransactionRecord } from "@carddemo/domain";
import { padKey } from "@carddemo/domain";
import { FileStatus } from "@carddemo/vsam";

import type { CardDemoCommarea } from "./commarea.js";
import { emptyCommarea, isReenter } from "./commarea.js";
import {
  formatTranAmount,
  isBlank,
  isNumericField,
  isValidDate,
  moveAlphanumeric,
  moveNumeric,
  numval,
  numvalC,
  toStoredAmount,
} from "./cobol.js";
import type { OnlineContext } from "./context.js";
import { INVALID_KEY, addMessages, transactionAdded } from "./messages.js";
import type { MessageColor, OnlineRequest, OnlineResponse, ScreenHeader } from "./screen.js";
import { AidKey, headerFor } from "./screen.js";
import { HIGH_VALUES_KEY, MENU_PROGRAM, SIGNON_PROGRAM } from "./transaction-list.js";

export const ADD_PROGRAM = "COTRN02C";
export const ADD_TRANID = "CT02";

export interface TransactionAddScreen {
  /** `ACTIDIN` PIC X(11). */
  readonly acctId: string;
  /** `CARDNIN` PIC X(16). */
  readonly cardNum: string;
  /** `TTYPCD` PIC X(02). */
  readonly typeCd: string;
  /** `TCATCD` PIC X(04). */
  readonly catCd: string;
  /** `TRNSRC` PIC X(10). */
  readonly source: string;
  /** `TDESC` PIC X(60). */
  readonly description: string;
  /** `TRNAMT` PIC X(12), typed as `-99999999.99`. */
  readonly amount: string;
  /** `TORIGDT` PIC X(10), `YYYY-MM-DD`. */
  readonly origDate: string;
  /** `TPROCDT` PIC X(10), `YYYY-MM-DD`. */
  readonly procDate: string;
  /** `MID` PIC X(09). */
  readonly merchantId: string;
  /** `MNAME` PIC X(30). */
  readonly merchantName: string;
  /** `MCITY` PIC X(25). */
  readonly merchantCity: string;
  /** `MZIP` PIC X(10). */
  readonly merchantZip: string;
  /** `CONFIRM` PIC X(01). */
  readonly confirm: string;
  readonly errMsg: string;
}

export const emptyTransactionAddScreen = (): TransactionAddScreen => ({
  acctId: "",
  cardNum: "",
  typeCd: "",
  catCd: "",
  source: "",
  description: "",
  amount: "",
  origDate: "",
  procDate: "",
  merchantId: "",
  merchantName: "",
  merchantCity: "",
  merchantZip: "",
  confirm: "",
  errMsg: "",
});

/** Result of a paragraph that either fails with a message or updates fields. */
interface EditFailure {
  readonly message: string;
  readonly cursor: string;
  readonly screen: TransactionAddScreen;
}

export function handleTransactionAdd(
  request: OnlineRequest<TransactionAddScreen>,
  context: OnlineContext,
): OnlineResponse<TransactionAddScreen> {
  const header = headerFor(ADD_TRANID, ADD_PROGRAM, context.now());

  if (request.commarea === undefined) {
    return transfer(emptyTransactionAddScreen(), emptyCommarea(), SIGNON_PROGRAM, header);
  }

  const commarea = request.commarea;

  if (!isReenter(commarea)) {
    return send(emptyTransactionAddScreen(), commarea, header, "ACTIDIN");
  }

  switch (request.aid) {
    case AidKey.enter:
      return processEnterKey(request.screen, commarea, context, header);
    case AidKey.pf3:
      return transfer(
        request.screen,
        commarea,
        isBlank(commarea.fromProgram) ? MENU_PROGRAM : commarea.fromProgram,
        header,
      );
    case AidKey.pf4:
      return send(emptyTransactionAddScreen(), commarea, header, "ACTIDIN");
    case AidKey.pf5:
      return copyLastTranData(request.screen, commarea, context, header);
    default:
      return send({ ...request.screen, errMsg: INVALID_KEY }, commarea, header, "ACTIDIN");
  }
}

/** `PROCESS-ENTER-KEY`. */
function processEnterKey(
  screen: TransactionAddScreen,
  commarea: CardDemoCommarea,
  context: OnlineContext,
  header: ScreenHeader,
): OnlineResponse<TransactionAddScreen> {
  const keyed = validateInputKeyFields(screen, context);
  if ("message" in keyed) {
    return send({ ...keyed.screen, errMsg: keyed.message }, commarea, header, keyed.cursor);
  }

  const edited = validateInputDataFields(keyed);
  if ("message" in edited) {
    return send({ ...edited.screen, errMsg: edited.message }, commarea, header, edited.cursor);
  }

  const confirm = edited.confirm.trim();
  if (confirm === "Y" || confirm === "y") {
    return addTransaction(edited, commarea, context, header);
  }
  if (confirm === "" || confirm === "N" || confirm === "n") {
    return send({ ...edited, errMsg: addMessages.confirmAdd }, commarea, header, "CONFIRM");
  }
  return send({ ...edited, errMsg: addMessages.invalidConfirm }, commarea, header, "CONFIRM");
}

/** `VALIDATE-INPUT-KEY-FIELDS`. */
function validateInputKeyFields(
  screen: TransactionAddScreen,
  context: OnlineContext,
): TransactionAddScreen | EditFailure {
  const cleared = clearDataFields(screen);

  if (!isBlank(screen.acctId)) {
    if (!isNumericField(moveAlphanumeric(screen.acctId, 11))) {
      return { message: addMessages.acctIdNotNumeric, cursor: "ACTIDIN", screen: cleared };
    }
    const acctId = padKey(numval(screen.acctId), 11);
    const result = context.files.acctXref.read(acctId);
    if (result.status === FileStatus.notFound) {
      return {
        message: addMessages.acctIdNotFound,
        cursor: "ACTIDIN",
        screen: { ...cleared, acctId },
      };
    }
    if (result.status !== FileStatus.ok || result.record === undefined) {
      return {
        message: addMessages.acctXrefLookupFailed,
        cursor: "ACTIDIN",
        screen: { ...cleared, acctId },
      };
    }
    return { ...screen, acctId, cardNum: result.record.xrefCardNum };
  }

  if (!isBlank(screen.cardNum)) {
    if (!isNumericField(moveAlphanumeric(screen.cardNum, 16))) {
      return { message: addMessages.cardNumNotNumeric, cursor: "CARDNIN", screen: cleared };
    }
    const cardNum = padKey(numval(screen.cardNum), 16);
    const result = context.files.cardXref.read(cardNum);
    if (result.status === FileStatus.notFound) {
      return {
        message: addMessages.cardNumNotFound,
        cursor: "CARDNIN",
        screen: { ...cleared, cardNum },
      };
    }
    if (result.status !== FileStatus.ok || result.record === undefined) {
      return {
        message: addMessages.cardXrefLookupFailed,
        cursor: "CARDNIN",
        screen: { ...cleared, cardNum },
      };
    }
    return { ...screen, cardNum, acctId: padKey(result.record.xrefAcctId, 11) };
  }

  return { message: addMessages.keyRequired, cursor: "ACTIDIN", screen: cleared };
}

/** The `IF ERR-FLG-ON` branch at the top of `VALIDATE-INPUT-DATA-FIELDS`. */
const clearDataFields = (screen: TransactionAddScreen): TransactionAddScreen => ({
  ...screen,
  typeCd: "",
  catCd: "",
  source: "",
  amount: "",
  description: "",
  origDate: "",
  procDate: "",
  merchantId: "",
  merchantName: "",
  merchantCity: "",
  merchantZip: "",
});

/** `VALIDATE-INPUT-DATA-FIELDS`. */
function validateInputDataFields(
  screen: TransactionAddScreen,
): TransactionAddScreen | EditFailure {
  const required: ReadonlyArray<readonly [string, string, string]> = [
    [screen.typeCd, addMessages.typeCdEmpty, "TTYPCD"],
    [screen.catCd, addMessages.catCdEmpty, "TCATCD"],
    [screen.source, addMessages.sourceEmpty, "TRNSRC"],
    [screen.description, addMessages.descEmpty, "TDESC"],
    [screen.amount, addMessages.amountEmpty, "TRNAMT"],
    [screen.origDate, addMessages.origDateEmpty, "TORIGDT"],
    [screen.procDate, addMessages.procDateEmpty, "TPROCDT"],
    [screen.merchantId, addMessages.merchantIdEmpty, "MID"],
    [screen.merchantName, addMessages.merchantNameEmpty, "MNAME"],
    [screen.merchantCity, addMessages.merchantCityEmpty, "MCITY"],
    [screen.merchantZip, addMessages.merchantZipEmpty, "MZIP"],
  ];
  for (const [value, message, cursor] of required) {
    if (isBlank(value)) {
      return { message, cursor, screen };
    }
  }

  if (!isNumericField(moveAlphanumeric(screen.typeCd, 2))) {
    return { message: addMessages.typeCdNotNumeric, cursor: "TTYPCD", screen };
  }
  if (!isNumericField(moveAlphanumeric(screen.catCd, 4))) {
    return { message: addMessages.catCdNotNumeric, cursor: "TCATCD", screen };
  }
  if (!isAmountEdited(screen.amount)) {
    return { message: addMessages.amountFormat, cursor: "TRNAMT", screen };
  }
  if (!isDateEdited(screen.origDate)) {
    return { message: addMessages.origDateFormat, cursor: "TORIGDT", screen };
  }
  if (!isDateEdited(screen.procDate)) {
    return { message: addMessages.procDateFormat, cursor: "TPROCDT", screen };
  }

  // `MOVE WS-TRAN-AMT-E TO TRNAMTI`: the amount is redisplayed edited.
  const normalized: TransactionAddScreen = {
    ...screen,
    amount: formatTranAmount(numvalC(screen.amount)),
  };

  if (!isValidDate(screen.origDate)) {
    return { message: addMessages.origDateInvalid, cursor: "TORIGDT", screen: normalized };
  }
  if (!isValidDate(screen.procDate)) {
    return { message: addMessages.procDateInvalid, cursor: "TPROCDT", screen: normalized };
  }
  if (!isNumericField(moveAlphanumeric(screen.merchantId, 9))) {
    return { message: addMessages.merchantIdNotNumeric, cursor: "MID", screen: normalized };
  }

  return normalized;
}

/** `TRNAMTI(1:1)`, `(2:8)`, `(10:1)` and `(11:2)` edits. */
function isAmountEdited(value: string): boolean {
  const field = moveAlphanumeric(value, 12);
  const sign = field.slice(0, 1);
  return (
    (sign === "-" || sign === "+") &&
    isNumericField(field.slice(1, 9)) &&
    field.slice(9, 10) === "." &&
    isNumericField(field.slice(10, 12))
  );
}

/** `TORIGDTI`/`TPROCDTI` `YYYY-MM-DD` edits. */
function isDateEdited(value: string): boolean {
  const field = moveAlphanumeric(value, 10);
  return (
    isNumericField(field.slice(0, 4)) &&
    field.slice(4, 5) === "-" &&
    isNumericField(field.slice(5, 7)) &&
    field.slice(7, 8) === "-" &&
    isNumericField(field.slice(8, 10))
  );
}

/** `ADD-TRANSACTION` and `WRITE-TRANSACT-FILE`. */
function addTransaction(
  screen: TransactionAddScreen,
  commarea: CardDemoCommarea,
  context: OnlineContext,
  header: ScreenHeader,
): OnlineResponse<TransactionAddScreen> {
  const tranId = nextTransactionId(context);
  const record: TransactionRecord = {
    tranId,
    tranTypeCd: moveAlphanumeric(screen.typeCd, 2),
    tranCatCd: numval(screen.catCd),
    tranSource: moveAlphanumeric(screen.source, 10),
    tranDesc: moveAlphanumeric(screen.description, 100),
    tranAmt: toStoredAmount(numvalC(screen.amount)),
    tranMerchantId: numval(screen.merchantId),
    tranMerchantName: moveAlphanumeric(screen.merchantName, 50),
    tranMerchantCity: moveAlphanumeric(screen.merchantCity, 50),
    tranMerchantZip: moveAlphanumeric(screen.merchantZip, 10),
    tranCardNum: moveAlphanumeric(screen.cardNum, 16),
    tranOrigTs: moveAlphanumeric(screen.origDate, 26),
    tranProcTs: moveAlphanumeric(screen.procDate, 26),
  };

  const status = context.files.transactions.write(record);
  if (status === FileStatus.duplicateKey) {
    return send({ ...screen, errMsg: addMessages.tranIdExists }, commarea, header, "ACTIDIN");
  }
  if (status !== FileStatus.ok) {
    return send({ ...screen, errMsg: addMessages.addFailed }, commarea, header, "ACTIDIN");
  }
  if (context.persist) {
    context.files.transactions.save();
  }

  return send(
    { ...emptyTransactionAddScreen(), errMsg: transactionAdded(tranId) },
    commarea,
    header,
    "ACTIDIN",
    "green",
  );
}

/**
 * `MOVE HIGH-VALUES TO TRAN-ID`, `STARTBR`, `READPREV`: the highest key on
 * file plus one. A `STARTBR` past the last key positions the browse at the
 * end of the file, where `READPREV` returns the last record.
 */
export function nextTransactionId(context: OnlineContext): string {
  const last = lastTransaction(context);
  const lastId = last === undefined ? 0 : numval(last.tranId);
  return moveNumeric(lastId + 1, 16);
}

/** The record `READPREV` returns after a browse started at `HIGH-VALUES`. */
export function lastTransaction(context: OnlineContext): TransactionRecord | undefined {
  context.files.transactions.startBrowse(HIGH_VALUES_KEY);
  const result = context.files.transactions.readPrev();
  return result.status === FileStatus.ok ? result.record : undefined;
}

/** `COPY-LAST-TRAN-DATA`, reached with PF5. */
function copyLastTranData(
  screen: TransactionAddScreen,
  commarea: CardDemoCommarea,
  context: OnlineContext,
  header: ScreenHeader,
): OnlineResponse<TransactionAddScreen> {
  const keyed = validateInputKeyFields(screen, context);
  if ("message" in keyed) {
    return send({ ...keyed.screen, errMsg: keyed.message }, commarea, header, keyed.cursor);
  }

  const last = lastTransaction(context);
  const copied: TransactionAddScreen =
    last === undefined
      ? keyed
      : {
          ...keyed,
          typeCd: last.tranTypeCd,
          catCd: moveNumeric(last.tranCatCd, 4),
          source: moveAlphanumeric(last.tranSource, 10).trimEnd(),
          amount: formatTranAmount(last.tranAmt),
          description: moveAlphanumeric(last.tranDesc, 60).trimEnd(),
          origDate: moveAlphanumeric(last.tranOrigTs, 10),
          procDate: moveAlphanumeric(last.tranProcTs, 10),
          merchantId: moveNumeric(last.tranMerchantId, 9),
          merchantName: moveAlphanumeric(last.tranMerchantName, 30).trimEnd(),
          merchantCity: moveAlphanumeric(last.tranMerchantCity, 25).trimEnd(),
          merchantZip: moveAlphanumeric(last.tranMerchantZip, 10).trimEnd(),
        };

  return processEnterKey(copied, commarea, context, header);
}

const send = (
  screen: TransactionAddScreen,
  commarea: CardDemoCommarea,
  header: ScreenHeader,
  cursor: string,
  messageColor: MessageColor = "red",
): OnlineResponse<TransactionAddScreen> => ({
  screen,
  header,
  commarea: { ...commarea, pgmContext: 1 },
  nextProgram: ADD_PROGRAM,
  transfer: false,
  cursor,
  messageColor,
});

const transfer = (
  screen: TransactionAddScreen,
  commarea: CardDemoCommarea,
  toProgram: string,
  header: ScreenHeader,
): OnlineResponse<TransactionAddScreen> => ({
  screen,
  header,
  commarea: {
    ...commarea,
    fromTranId: ADD_TRANID,
    fromProgram: ADD_PROGRAM,
    toProgram,
    pgmContext: 0,
  },
  nextProgram: toProgram,
  transfer: true,
  messageColor: "red",
});
