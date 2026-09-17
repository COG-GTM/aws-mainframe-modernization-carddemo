/**
 * `COTRN01C` — view a transaction (map `COTRN1A` of mapset `COTRN01`).
 *
 * Reads `TRANSACT` by `TRAN-ID`, either the id typed on the screen or the one
 * `COTRN00C` left in the commarea when a row was selected.
 */

import { padKey } from "@carddemo/domain";
import { FileStatus } from "@carddemo/vsam";

import type { CardDemoCommarea } from "./commarea.js";
import { emptyCommarea, isReenter } from "./commarea.js";
import { formatTranAmount, isBlank, moveAlphanumeric, moveNumeric } from "./cobol.js";
import type { OnlineContext } from "./context.js";
import { INVALID_KEY, viewMessages } from "./messages.js";
import type { OnlineResponse, OnlineRequest, ScreenHeader } from "./screen.js";
import { AidKey, headerFor } from "./screen.js";
import { LIST_PROGRAM, MENU_PROGRAM, SIGNON_PROGRAM } from "./transaction-list.js";

export const VIEW_PROGRAM_NAME = "COTRN01C";
export const VIEW_TRANID = "CT01";

export interface TransactionViewScreen {
  /** `TRNIDIN`, the id to look up. */
  readonly tranIdIn: string;
  readonly tranId: string;
  readonly cardNum: string;
  readonly typeCd: string;
  readonly catCd: string;
  readonly source: string;
  readonly amount: string;
  readonly description: string;
  readonly origDate: string;
  readonly procDate: string;
  readonly merchantId: string;
  readonly merchantName: string;
  readonly merchantCity: string;
  readonly merchantZip: string;
  readonly errMsg: string;
}

export const emptyTransactionViewScreen = (): TransactionViewScreen => ({
  tranIdIn: "",
  tranId: "",
  cardNum: "",
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
  errMsg: "",
});

export function handleTransactionView(
  request: OnlineRequest<TransactionViewScreen>,
  context: OnlineContext,
): OnlineResponse<TransactionViewScreen> {
  const header = headerFor(VIEW_TRANID, VIEW_PROGRAM_NAME, context.now());

  if (request.commarea === undefined) {
    return transfer(emptyTransactionViewScreen(), emptyCommarea(), SIGNON_PROGRAM, header);
  }

  const commarea = request.commarea;

  if (!isReenter(commarea)) {
    const selected = commarea.listState.trnSelected;
    let screen = emptyTransactionViewScreen();
    if (!isBlank(selected)) {
      screen = { ...screen, tranIdIn: selected };
      screen = processEnterKey(screen, context);
    }
    return send(screen, commarea, header);
  }

  switch (request.aid) {
    case AidKey.enter:
      return send(processEnterKey(request.screen, context), commarea, header);
    case AidKey.pf3:
      return transfer(
        request.screen,
        commarea,
        isBlank(commarea.fromProgram) ? MENU_PROGRAM : commarea.fromProgram,
        header,
      );
    case AidKey.pf4:
      return send(emptyTransactionViewScreen(), commarea, header);
    case AidKey.pf5:
      return transfer(request.screen, commarea, LIST_PROGRAM, header);
    default:
      return send({ ...request.screen, errMsg: INVALID_KEY }, commarea, header);
  }
}

/** `PROCESS-ENTER-KEY`. */
function processEnterKey(
  screen: TransactionViewScreen,
  context: OnlineContext,
): TransactionViewScreen {
  if (isBlank(screen.tranIdIn)) {
    return { ...screen, errMsg: viewMessages.tranIdEmpty };
  }

  const cleared: TransactionViewScreen = {
    ...emptyTransactionViewScreen(),
    tranIdIn: screen.tranIdIn,
  };

  const result = context.files.transactions.read(padKey(screen.tranIdIn.trimEnd(), 16));
  if (result.status === FileStatus.notFound) {
    return { ...cleared, errMsg: viewMessages.tranIdNotFound };
  }
  if (result.status !== FileStatus.ok || result.record === undefined) {
    return { ...cleared, errMsg: viewMessages.lookupFailed };
  }

  const record = result.record;
  return {
    ...cleared,
    tranId: record.tranId,
    cardNum: record.tranCardNum,
    typeCd: record.tranTypeCd,
    catCd: moveNumeric(record.tranCatCd, 4),
    source: moveAlphanumeric(record.tranSource, 10).trimEnd(),
    amount: formatTranAmount(record.tranAmt),
    description: moveAlphanumeric(record.tranDesc, 60).trimEnd(),
    origDate: moveAlphanumeric(record.tranOrigTs, 10),
    procDate: moveAlphanumeric(record.tranProcTs, 10),
    merchantId: moveNumeric(record.tranMerchantId, 9),
    merchantName: moveAlphanumeric(record.tranMerchantName, 30).trimEnd(),
    merchantCity: moveAlphanumeric(record.tranMerchantCity, 25).trimEnd(),
    merchantZip: moveAlphanumeric(record.tranMerchantZip, 10).trimEnd(),
  };
}

const send = (
  screen: TransactionViewScreen,
  commarea: CardDemoCommarea,
  header: ScreenHeader,
): OnlineResponse<TransactionViewScreen> => ({
  screen,
  header,
  commarea: { ...commarea, pgmContext: 1 },
  nextProgram: VIEW_PROGRAM_NAME,
  transfer: false,
  cursor: "TRNIDIN",
  messageColor: "red",
});

const transfer = (
  screen: TransactionViewScreen,
  commarea: CardDemoCommarea,
  toProgram: string,
  header: ScreenHeader,
): OnlineResponse<TransactionViewScreen> => ({
  screen,
  header,
  commarea: {
    ...commarea,
    fromTranId: VIEW_TRANID,
    fromProgram: VIEW_PROGRAM_NAME,
    toProgram,
    pgmContext: 0,
  },
  nextProgram: toProgram,
  transfer: true,
  messageColor: "red",
});
