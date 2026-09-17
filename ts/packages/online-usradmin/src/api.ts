/**
 * Express transport for the user administration handlers.
 *
 * The transport only decodes the request body, calls the handler and serialises
 * the response; every rule lives in the handlers, as it does in the COBOL.
 */

import type { UserSecurityRecord } from "@carddemo/domain";
import type { Ksds } from "@carddemo/vsam";
import express, { Router, type Express, type Request, type Response } from "express";

import { emptyCommarea, type CardDemoCommarea } from "./commarea.js";
import { handleUserList, emptyUserListScreen, type UserListScreen } from "./cousr00.js";
import { handleUserAdd, emptyUserAddScreen, type UserAddScreen } from "./cousr01.js";
import { handleUserUpdate, emptyUserUpdateScreen, type UserUpdateScreen } from "./cousr02.js";
import { handleUserDelete, emptyUserDeleteScreen, type UserDeleteScreen } from "./cousr03.js";
import {
  AidKey,
  type AidKeyCode,
  type HandlerOptions,
  type HandlerRequest,
  type HandlerResponse,
} from "./screen.js";

type Handler<TScreen> = (
  request: HandlerRequest<TScreen>,
  file: Ksds<UserSecurityRecord>,
  options?: HandlerOptions,
) => HandlerResponse<TScreen>;

interface TransactionBody<TScreen> {
  readonly aid?: string;
  readonly screen?: Partial<TScreen>;
  readonly commarea?: Partial<CardDemoCommarea>;
}

const aidCodes = new Set<string>(Object.values(AidKey));

const parseAid = (aid: string | undefined): AidKeyCode => {
  if (aid === undefined) {
    return AidKey.enter;
  }
  const upper = aid.toUpperCase();
  return aidCodes.has(upper) ? (upper as AidKeyCode) : AidKey.other;
};

function post<TScreen>(
  handler: Handler<TScreen>,
  emptyScreen: () => TScreen,
  file: Ksds<UserSecurityRecord>,
  options: HandlerOptions,
) {
  return (request: Request, response: Response): void => {
    const body = (request.body ?? {}) as TransactionBody<TScreen>;
    const result = handler(
      {
        aid: parseAid(body.aid),
        screen: { ...emptyScreen(), ...body.screen },
        commarea: { ...emptyCommarea(), ...body.commarea },
      },
      file,
      options,
    );
    response.json(result);
  };
}

export function createUserAdminRouter(
  file: Ksds<UserSecurityRecord>,
  options: HandlerOptions = {},
): Router {
  const router = Router();
  router.use(express.json());
  router.post(
    "/users/list",
    post<UserListScreen>(handleUserList, emptyUserListScreen, file, options),
  );
  router.post("/users/add", post<UserAddScreen>(handleUserAdd, emptyUserAddScreen, file, options));
  router.post(
    "/users/update",
    post<UserUpdateScreen>(handleUserUpdate, emptyUserUpdateScreen, file, options),
  );
  router.post(
    "/users/delete",
    post<UserDeleteScreen>(handleUserDelete, emptyUserDeleteScreen, file, options),
  );
  return router;
}

export function createUserAdminApp(
  file: Ksds<UserSecurityRecord>,
  options: HandlerOptions = {},
): Express {
  const app = express();
  app.use(createUserAdminRouter(file, options));
  return app;
}
