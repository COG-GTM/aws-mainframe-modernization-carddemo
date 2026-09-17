/**
 * The CICS transaction dispatcher as HTTP: each route only decodes the
 * request, calls the handler and encodes the response. No business logic
 * lives here.
 */

import express, { type Express, type Request, type Response } from "express";

import {
  handleAccountUpdate,
  initialAccountUpdateState,
  type AccountUpdateRequest,
  type AccountUpdateState,
} from "./account-update.js";
import { handleAccountView, type AccountViewRequest } from "./account-view.js";
import type { AccountFiles } from "./data.js";

/** The state is the client's copy of `WS-THIS-PROGCOMMAREA`; a first call omits it. */
type AccountUpdateBody = Omit<AccountUpdateRequest, "state"> & {
  readonly state?: AccountUpdateState;
};

export function createAccountsApi(files: AccountFiles): Express {
  const api = express();
  api.use(express.json());

  api.post("/cics/CAVW", (request: Request, response: Response) => {
    const body = request.body as AccountViewRequest;
    response.json(handleAccountView(body, files));
  });

  api.post("/cics/CAUP", (request: Request, response: Response) => {
    const body = request.body as AccountUpdateBody;
    response.json(
      handleAccountUpdate({ ...body, state: body.state ?? initialAccountUpdateState }, files),
    );
  });

  return api;
}
