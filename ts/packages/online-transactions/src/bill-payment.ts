/**
 * `COBIL00C` — pay the full account balance (map `COBIL0A` of `COBIL00`).
 *
 * Shows the current balance of an account, and on confirmation writes a
 * `BILL PAYMENT - ONLINE` transaction for that balance against the account's
 * card and rewrites the account with the balance reduced by the amount paid.
 */

import type { AccountRecord, TransactionRecord } from "@carddemo/domain";
import { padKey } from "@carddemo/domain";
import { FileStatus } from "@carddemo/vsam";

import type { CardDemoCommarea } from "./commarea.js";
import { emptyCommarea, isReenter } from "./commarea.js";
import { formatBalance, isBlank, moveNumeric, numval, toStoredAmount } from "./cobol.js";
import type { OnlineContext } from "./context.js";
import { INVALID_KEY, billPayMessages, paymentSuccessful } from "./messages.js";
import type { MessageColor, OnlineRequest, OnlineResponse, ScreenHeader } from "./screen.js";
import { AidKey, headerFor } from "./screen.js";
import { MENU_PROGRAM, SIGNON_PROGRAM } from "./transaction-list.js";
import { lastTransaction } from "./transaction-add.js";

export const BILLPAY_PROGRAM = "COBIL00C";
export const BILLPAY_TRANID = "CB00";

/** Fixed fields of the payment transaction built by `PROCESS-ENTER-KEY`. */
export const BILL_PAYMENT = {
  typeCd: "02",
  catCd: 2,
  source: "POS TERM",
  description: "BILL PAYMENT - ONLINE",
  merchantId: 999999999,
  merchantName: "BILL PAYMENT",
  merchantCity: "N/A",
  merchantZip: "N/A",
} as const;

export interface BillPaymentScreen {
  /** `ACTIDIN` PIC X(11). */
  readonly acctId: string;
  /** `CURBAL` PIC X(14), displayed as `+9999999999.99`. */
  readonly currBal: string;
  /** `CONFIRM` PIC X(01). */
  readonly confirm: string;
  readonly errMsg: string;
}

export const emptyBillPaymentScreen = (): BillPaymentScreen => ({
  acctId: "",
  currBal: "",
  confirm: "",
  errMsg: "",
});

export function handleBillPayment(
  request: OnlineRequest<BillPaymentScreen>,
  context: OnlineContext,
): OnlineResponse<BillPaymentScreen> {
  const header = headerFor(BILLPAY_TRANID, BILLPAY_PROGRAM, context.now());

  if (request.commarea === undefined) {
    return transfer(emptyBillPaymentScreen(), emptyCommarea(), SIGNON_PROGRAM, header);
  }

  const commarea = request.commarea;

  if (!isReenter(commarea)) {
    return send(emptyBillPaymentScreen(), commarea, header, "ACTIDIN");
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
      return send(emptyBillPaymentScreen(), commarea, header, "ACTIDIN");
    default:
      return send({ ...request.screen, errMsg: INVALID_KEY }, commarea, header, "ACTIDIN");
  }
}

/** `PROCESS-ENTER-KEY`. */
function processEnterKey(
  screen: BillPaymentScreen,
  commarea: CardDemoCommarea,
  context: OnlineContext,
  header: ScreenHeader,
): OnlineResponse<BillPaymentScreen> {
  if (isBlank(screen.acctId)) {
    return send({ ...screen, errMsg: billPayMessages.acctIdEmpty }, commarea, header, "ACTIDIN");
  }

  const acctKey = padKey(numval(screen.acctId), 11);
  const confirm = screen.confirm.trim();
  let confirmPay = false;

  if (confirm === "N" || confirm === "n") {
    // `CLEAR-CURRENT-SCREEN` then the error flag: a blank screen, no message.
    return send(emptyBillPaymentScreen(), commarea, header, "ACTIDIN");
  }
  if (confirm === "Y" || confirm === "y") {
    confirmPay = true;
  } else if (confirm !== "") {
    return send(
      { ...screen, errMsg: billPayMessages.invalidConfirm },
      commarea,
      header,
      "CONFIRM",
    );
  }

  const read = context.files.accounts.read(acctKey);
  if (read.status === FileStatus.notFound) {
    return send(
      { ...screen, errMsg: billPayMessages.acctIdNotFound },
      commarea,
      header,
      "ACTIDIN",
    );
  }
  if (read.status !== FileStatus.ok || read.record === undefined) {
    return send(
      { ...screen, errMsg: billPayMessages.acctLookupFailed },
      commarea,
      header,
      "ACTIDIN",
    );
  }

  const account = read.record;
  const withBalance: BillPaymentScreen = { ...screen, currBal: formatBalance(account.acctCurrBal) };

  if (account.acctCurrBal <= 0) {
    return send(
      { ...withBalance, errMsg: billPayMessages.nothingToPay },
      commarea,
      header,
      "ACTIDIN",
    );
  }

  if (!confirmPay) {
    return send(
      { ...withBalance, errMsg: billPayMessages.confirmPayment },
      commarea,
      header,
      "CONFIRM",
    );
  }

  return makePayment(withBalance, account, commarea, context, header);
}

function makePayment(
  screen: BillPaymentScreen,
  account: AccountRecord,
  commarea: CardDemoCommarea,
  context: OnlineContext,
  header: ScreenHeader,
): OnlineResponse<BillPaymentScreen> {
  const xref = context.files.acctXref.read(padKey(account.acctId, 11));
  if (xref.status === FileStatus.notFound) {
    return send(
      { ...screen, errMsg: billPayMessages.acctIdNotFound },
      commarea,
      header,
      "ACTIDIN",
    );
  }
  if (xref.status !== FileStatus.ok || xref.record === undefined) {
    return send(
      { ...screen, errMsg: billPayMessages.xrefLookupFailed },
      commarea,
      header,
      "ACTIDIN",
    );
  }

  const last = lastTransaction(context);
  const tranId = moveNumeric((last === undefined ? 0 : numval(last.tranId)) + 1, 16);
  const amount = toStoredAmount(account.acctCurrBal);
  const timestamp = currentTimestamp(context.now());

  const record: TransactionRecord = {
    tranId,
    tranTypeCd: BILL_PAYMENT.typeCd,
    tranCatCd: BILL_PAYMENT.catCd,
    tranSource: BILL_PAYMENT.source,
    tranDesc: BILL_PAYMENT.description,
    tranAmt: amount,
    tranMerchantId: BILL_PAYMENT.merchantId,
    tranMerchantName: BILL_PAYMENT.merchantName,
    tranMerchantCity: BILL_PAYMENT.merchantCity,
    tranMerchantZip: BILL_PAYMENT.merchantZip,
    tranCardNum: xref.record.xrefCardNum,
    tranOrigTs: timestamp,
    tranProcTs: timestamp,
  };

  const writeStatus = context.files.transactions.write(record);
  if (writeStatus === FileStatus.duplicateKey) {
    return send({ ...screen, errMsg: billPayMessages.tranIdExists }, commarea, header, "ACTIDIN");
  }
  if (writeStatus !== FileStatus.ok) {
    return send({ ...screen, errMsg: billPayMessages.addFailed }, commarea, header, "ACTIDIN");
  }
  if (context.persist) {
    context.files.transactions.save();
  }

  const updated: AccountRecord = {
    ...account,
    acctCurrBal: toStoredAmount(account.acctCurrBal - amount),
  };
  const updateStatus = context.files.accounts.rewrite(updated);
  if (updateStatus !== FileStatus.ok) {
    return send({ ...screen, errMsg: billPayMessages.updateFailed }, commarea, header, "ACTIDIN");
  }
  if (context.persist) {
    context.files.accounts.save();
  }

  return send(
    { ...emptyBillPaymentScreen(), errMsg: paymentSuccessful(tranId) },
    commarea,
    header,
    "ACTIDIN",
    "green",
  );
}

/** `GET-CURRENT-TIMESTAMP`: `YYYY-MM-DD HH:MM:SS.000000`. */
export function currentTimestamp(now: Date): string {
  const pad = (value: number, width = 2): string => String(value).padStart(width, "0");
  const date = `${now.getFullYear()}-${pad(now.getMonth() + 1)}-${pad(now.getDate())}`;
  const time = `${pad(now.getHours())}:${pad(now.getMinutes())}:${pad(now.getSeconds())}`;
  return `${date} ${time}.000000`;
}

const send = (
  screen: BillPaymentScreen,
  commarea: CardDemoCommarea,
  header: ScreenHeader,
  cursor: string,
  messageColor: MessageColor = "red",
): OnlineResponse<BillPaymentScreen> => ({
  screen,
  header,
  commarea: { ...commarea, pgmContext: 1 },
  nextProgram: BILLPAY_PROGRAM,
  transfer: false,
  cursor,
  messageColor,
});

const transfer = (
  screen: BillPaymentScreen,
  commarea: CardDemoCommarea,
  toProgram: string,
  header: ScreenHeader,
): OnlineResponse<BillPaymentScreen> => ({
  screen,
  header,
  commarea: {
    ...commarea,
    fromTranId: BILLPAY_TRANID,
    fromProgram: BILLPAY_PROGRAM,
    toProgram,
    pgmContext: 0,
  },
  nextProgram: toProgram,
  transfer: true,
  messageColor: "red",
});
