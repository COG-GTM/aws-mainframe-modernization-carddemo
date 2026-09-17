/**
 * Express transport: one route per online program, each one a `RECEIVE MAP`
 * (the posted screen, AID key and commarea) followed by the `SEND MAP` the
 * handler produced. All behaviour lives in the handlers.
 */

import express, { type Express, type Request, type Response } from "express";

import type { OnlineContext } from "./context.js";
import type { BillPaymentScreen } from "./bill-payment.js";
import { handleBillPayment } from "./bill-payment.js";
import type { ReportRequestScreen } from "./report-request.js";
import { handleReportRequest } from "./report-request.js";
import type { AidKeyValue, OnlineRequest, OnlineResponse } from "./screen.js";
import { AidKey } from "./screen.js";
import type { TransactionAddScreen } from "./transaction-add.js";
import { handleTransactionAdd } from "./transaction-add.js";
import type { TransactionListScreen } from "./transaction-list.js";
import { handleTransactionList } from "./transaction-list.js";
import type { TransactionViewScreen } from "./transaction-view.js";
import { handleTransactionView } from "./transaction-view.js";

type Handler<TScreen> = (
  request: OnlineRequest<TScreen>,
  context: OnlineContext,
) => OnlineResponse<TScreen>;

const aidKeys = new Set<string>(Object.values(AidKey));

const isAidKey = (value: unknown): value is AidKeyValue =>
  typeof value === "string" && aidKeys.has(value);

function route<TScreen>(
  handler: Handler<TScreen>,
  context: OnlineContext,
): (req: Request, res: Response) => void {
  return (req, res) => {
    const body = req.body as Partial<OnlineRequest<TScreen>> | undefined;
    if (body?.screen === undefined || !isAidKey(body.aid)) {
      res.status(400).json({ error: "screen and a valid aid key are required" });
      return;
    }
    const request: OnlineRequest<TScreen> = {
      screen: body.screen,
      aid: body.aid,
      ...(body.commarea === undefined ? {} : { commarea: body.commarea }),
    };
    res.json(handler(request, context));
  };
}

export function createApp(context: OnlineContext): Express {
  const app = express();
  app.use(express.json());

  app.post("/transactions/list", route<TransactionListScreen>(handleTransactionList, context));
  app.post("/transactions/view", route<TransactionViewScreen>(handleTransactionView, context));
  app.post("/transactions/add", route<TransactionAddScreen>(handleTransactionAdd, context));
  app.post("/bill-payments", route<BillPaymentScreen>(handleBillPayment, context));
  app.post("/reports", route<ReportRequestScreen>(handleReportRequest, context));

  return app;
}
