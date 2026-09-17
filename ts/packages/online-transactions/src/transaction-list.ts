/**
 * `COTRN00C` — list transactions (map `COTRN0A` of mapset `COTRN00`).
 *
 * Ten rows per page browsed over `TRANSACT`, paged with PF7/PF8 and with a
 * row selected by typing `S`, which transfers to `COTRN01C`.
 */

import type { TransactionRecord } from "@carddemo/domain";
import { padKey } from "@carddemo/domain";
import { FileStatus } from "@carddemo/vsam";

import type { CardDemoCommarea, TransactionListState } from "./commarea.js";
import { emptyCommarea, isReenter } from "./commarea.js";
import { isBlank, isNumericField, listDate, formatTranAmount, moveAlphanumeric } from "./cobol.js";
import type { OnlineContext } from "./context.js";
import { INVALID_KEY, listMessages } from "./messages.js";
import type { AidKeyValue, OnlineRequest, OnlineResponse } from "./screen.js";
import { AidKey, headerFor } from "./screen.js";

export const LIST_PROGRAM = "COTRN00C";
export const LIST_TRANID = "CT00";
export const MENU_PROGRAM = "COMEN01C";
export const SIGNON_PROGRAM = "COSGN00C";
export const VIEW_PROGRAM = "COTRN01C";

/** Number of rows on `COTRN0A`. */
export const PAGE_SIZE = 10;

/** `TRAN-ID` set to `HIGH-VALUES` before a browse that walks backwards. */
export const HIGH_VALUES_KEY = "\u00ff".repeat(16);

/** One `SEL000n` / `TRNID0n` / `TDATE0n` / `TDESC0n` / `TAMT00n` row. */
export interface TransactionListRow {
  readonly sel: string;
  readonly tranId: string;
  readonly date: string;
  readonly description: string;
  readonly amount: string;
}

export interface TransactionListScreen {
  /** `TRNIDIN`, the transaction id the browse starts from. */
  readonly tranIdFilter: string;
  /** `PAGENUM`. */
  readonly pageNum: string;
  readonly rows: readonly TransactionListRow[];
  /** `ERRMSG`. */
  readonly errMsg: string;
}

const emptyRow = (): TransactionListRow => ({
  sel: "",
  tranId: "",
  date: "",
  description: "",
  amount: "",
});

export const emptyTransactionListScreen = (): TransactionListScreen => ({
  tranIdFilter: "",
  pageNum: "",
  rows: Array.from({ length: PAGE_SIZE }, emptyRow),
  errMsg: "",
});

interface ListWork {
  rows: TransactionListRow[];
  tranIdFilter: string;
  pageNum: number;
  nextPage: "Y" | "N";
  trnIdFirst: string;
  trnIdLast: string;
  trnSelFlag: string;
  trnSelected: string;
  message: string;
  errFlag: boolean;
  eof: boolean;
}

export function handleTransactionList(
  request: OnlineRequest<TransactionListScreen>,
  context: OnlineContext,
): OnlineResponse<TransactionListScreen> {
  const header = headerFor(LIST_TRANID, LIST_PROGRAM, context.now());

  if (request.commarea === undefined) {
    return returnToPrevScreen(emptyTransactionListScreen(), undefined, SIGNON_PROGRAM, header);
  }

  const commarea = request.commarea;
  const work = workFromRequest(request.screen, commarea.listState);

  if (!isReenter(commarea)) {
    // `MOVE LOW-VALUES TO COTRN0AO` then the initial page.
    const fresh = workFromRequest(emptyTransactionListScreen(), commarea.listState);
    processEnterKey(fresh, context, AidKey.enter);
    return respond(fresh, commarea, header, true);
  }

  switch (request.aid) {
    case AidKey.enter: {
      const transfer = processEnterKey(work, context, AidKey.enter);
      if (transfer) {
        return returnToPrevScreen(
          screenFrom(work),
          withListState(commarea, work),
          VIEW_PROGRAM,
          header,
        );
      }
      break;
    }
    case AidKey.pf3:
      return returnToPrevScreen(
        screenFrom(work),
        withListState(commarea, work),
        MENU_PROGRAM,
        header,
      );
    case AidKey.pf7:
      processPf7Key(work, context);
      break;
    case AidKey.pf8:
      processPf8Key(work, context);
      break;
    default:
      work.errFlag = true;
      work.message = INVALID_KEY;
      break;
  }

  return respond(work, commarea, header, true);
}

const workFromRequest = (
  screen: TransactionListScreen,
  state: TransactionListState,
): ListWork => ({
  rows: Array.from({ length: PAGE_SIZE }, (_, index) => screen.rows[index] ?? emptyRow()),
  tranIdFilter: screen.tranIdFilter,
  pageNum: state.pageNum,
  nextPage: state.nextPageFlag,
  trnIdFirst: state.trnIdFirst,
  trnIdLast: state.trnIdLast,
  trnSelFlag: state.trnSelFlag,
  trnSelected: state.trnSelected,
  message: "",
  errFlag: false,
  eof: false,
});

const screenFrom = (work: ListWork): TransactionListScreen => ({
  tranIdFilter: work.tranIdFilter,
  pageNum: work.pageNum === 0 ? "" : String(work.pageNum),
  rows: work.rows,
  errMsg: work.message,
});

const withListState = (commarea: CardDemoCommarea, work: ListWork): CardDemoCommarea => ({
  ...commarea,
  pgmContext: 1,
  listState: {
    trnIdFirst: work.trnIdFirst,
    trnIdLast: work.trnIdLast,
    pageNum: work.pageNum,
    nextPageFlag: work.nextPage,
    trnSelFlag: work.trnSelFlag,
    trnSelected: work.trnSelected,
  },
});

const respond = (
  work: ListWork,
  commarea: CardDemoCommarea,
  header: ReturnType<typeof headerFor>,
  cursorOnFilter: boolean,
): OnlineResponse<TransactionListScreen> => ({
  screen: screenFrom(work),
  header,
  commarea: withListState(commarea, work),
  nextProgram: LIST_PROGRAM,
  transfer: false,
  ...(cursorOnFilter ? { cursor: "TRNIDIN" } : {}),
  messageColor: "red",
});

const returnToPrevScreen = (
  screen: TransactionListScreen,
  commarea: CardDemoCommarea | undefined,
  toProgram: string,
  header: ReturnType<typeof headerFor>,
): OnlineResponse<TransactionListScreen> => ({
  screen,
  header,
  commarea: {
    ...(commarea ?? emptyCommarea()),
    fromTranId: LIST_TRANID,
    fromProgram: LIST_PROGRAM,
    toProgram,
    pgmContext: 0,
  },
  nextProgram: toProgram,
  transfer: true,
  messageColor: "red",
});

/** `PROCESS-ENTER-KEY`; returns `true` when the row selection transfers out. */
function processEnterKey(work: ListWork, context: OnlineContext, aid: AidKeyValue): boolean {
  const selected = work.rows.find((row) => !isBlank(row.sel));
  if (selected === undefined) {
    work.trnSelFlag = "";
    work.trnSelected = "";
  } else {
    work.trnSelFlag = selected.sel;
    work.trnSelected = selected.tranId;
  }

  if (!isBlank(work.trnSelFlag) && !isBlank(work.trnSelected)) {
    if (work.trnSelFlag === "S" || work.trnSelFlag === "s") {
      return true;
    }
    work.message = listMessages.invalidSelection;
  }

  let startKey: string | undefined;
  if (isBlank(work.tranIdFilter)) {
    startKey = undefined;
  } else if (isNumericField(work.tranIdFilter.trim())) {
    startKey = moveAlphanumeric(work.tranIdFilter.trim(), 16);
  } else {
    work.errFlag = true;
    work.message = listMessages.tranIdNotNumeric;
    return false;
  }

  work.pageNum = 0;
  processPageForward(work, context, aid, startKey);

  if (!work.errFlag) {
    work.tranIdFilter = "";
  }
  return false;
}

/** `PROCESS-PF7-KEY`. */
function processPf7Key(work: ListWork, context: OnlineContext): void {
  const startKey = isBlank(work.trnIdFirst) ? undefined : work.trnIdFirst;
  work.nextPage = "Y";
  if (work.pageNum > 1) {
    processPageBackward(work, context, AidKey.pf7, startKey);
  } else {
    work.message = listMessages.alreadyTopOfPage;
  }
}

/** `PROCESS-PF8-KEY`. */
function processPf8Key(work: ListWork, context: OnlineContext): void {
  const startKey = isBlank(work.trnIdLast) ? HIGH_VALUES_KEY : work.trnIdLast;
  if (work.nextPage === "Y") {
    processPageForward(work, context, AidKey.pf8, startKey);
  } else {
    work.message = listMessages.alreadyBottomOfPage;
  }
}

/** `STARTBR-TRANSACT-FILE`. */
function startBrowse(work: ListWork, context: OnlineContext, key: string | undefined): void {
  const status = context.files.transactions.startBrowse(key);
  if (status === FileStatus.ok) {
    return;
  }
  if (status === FileStatus.notFound) {
    work.eof = true;
    work.message = listMessages.topOfPage;
    return;
  }
  work.errFlag = true;
  work.message = listMessages.lookupFailed;
}

/** `READNEXT-TRANSACT-FILE`. */
function readNext(work: ListWork, context: OnlineContext): TransactionRecord | undefined {
  const result = context.files.transactions.readNext();
  if (result.status === FileStatus.ok) {
    return result.record;
  }
  if (result.status === FileStatus.endOfFile) {
    work.eof = true;
    work.message = listMessages.bottomReached;
    return undefined;
  }
  work.errFlag = true;
  work.message = listMessages.lookupFailed;
  return undefined;
}

/** `READPREV-TRANSACT-FILE`. */
function readPrev(work: ListWork, context: OnlineContext): TransactionRecord | undefined {
  const result = context.files.transactions.readPrev();
  if (result.status === FileStatus.ok) {
    return result.record;
  }
  if (result.status === FileStatus.endOfFile) {
    work.eof = true;
    work.message = listMessages.topReached;
    return undefined;
  }
  work.errFlag = true;
  work.message = listMessages.lookupFailed;
  return undefined;
}

/** `POPULATE-TRAN-DATA` for row `index` (1 based). */
function populateRow(work: ListWork, index: number, record: TransactionRecord): void {
  work.rows[index - 1] = {
    sel: "",
    tranId: record.tranId,
    date: listDate(record.tranOrigTs),
    description: moveAlphanumeric(record.tranDesc, 26).trimEnd(),
    amount: formatTranAmount(record.tranAmt),
  };
  if (index === 1) {
    work.trnIdFirst = record.tranId;
  }
  if (index === PAGE_SIZE) {
    work.trnIdLast = record.tranId;
  }
}

const clearRows = (work: ListWork): void => {
  work.rows = Array.from({ length: PAGE_SIZE }, emptyRow);
};

/** `PROCESS-PAGE-FORWARD`. */
function processPageForward(
  work: ListWork,
  context: OnlineContext,
  aid: AidKeyValue,
  startKey: string | undefined,
): void {
  startBrowse(work, context, startKey);
  if (work.errFlag) {
    return;
  }

  if (aid !== AidKey.enter && aid !== AidKey.pf7 && aid !== AidKey.pf3) {
    readNext(work, context);
  }

  if (!work.eof && !work.errFlag) {
    clearRows(work);
  }

  let index = 1;
  while (index < 11 && !work.eof && !work.errFlag) {
    const record = readNext(work, context);
    if (record !== undefined) {
      populateRow(work, index, record);
      index += 1;
    }
  }

  if (!work.eof && !work.errFlag) {
    work.pageNum += 1;
    const record = readNext(work, context);
    work.nextPage = record !== undefined && !work.errFlag ? "Y" : "N";
  } else {
    work.nextPage = "N";
    if (index > 1) {
      work.pageNum += 1;
    }
  }

  work.tranIdFilter = "";
}

/** `PROCESS-PAGE-BACKWARD`. */
function processPageBackward(
  work: ListWork,
  context: OnlineContext,
  aid: AidKeyValue,
  startKey: string | undefined,
): void {
  startBrowse(work, context, startKey);
  if (work.errFlag) {
    return;
  }

  if (aid !== AidKey.enter && aid !== AidKey.pf8) {
    readPrev(work, context);
  }

  if (!work.eof && !work.errFlag) {
    clearRows(work);
  }

  let index = PAGE_SIZE;
  while (index > 0 && !work.eof && !work.errFlag) {
    const record = readPrev(work, context);
    if (record !== undefined) {
      populateRow(work, index, record);
      index -= 1;
    }
  }

  if (!work.eof && !work.errFlag) {
    readPrev(work, context);
    if (work.nextPage === "Y") {
      work.pageNum = !work.eof && !work.errFlag && work.pageNum > 1 ? work.pageNum - 1 : 1;
    }
  }
}

/** The key `COTRN01C` reads when a row is selected. */
export const selectedTransactionKey = (state: TransactionListState): string =>
  padKey(state.trnSelected, 16);
