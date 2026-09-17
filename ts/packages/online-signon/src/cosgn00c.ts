/**
 * `COSGN00C` — the CardDemo signon screen.
 *
 * Reads `USRSEC` with the typed user id, compares the password, and transfers
 * to the admin menu or the main menu depending on `SEC-USR-TYPE`.
 */

import { createCommarea, PgmContext, UserType, type CardDemoCommarea } from "./commarea.js";
import {
  AidKey,
  buildHeader,
  CommonMessage,
  errorMessageLength,
  messageLength,
  toFixed,
  type HandlerRequest,
  type HandlerResponse,
  type ScreenHeaderFields,
} from "./screen.js";
import {
  adminMenuProgram,
  mainMenuProgram,
  signonMap,
  signonMapset,
  signonProgram,
  signonTransaction,
} from "./programs.js";
import { CicsResp, userIdLength, type UserSecurityStore } from "./user-security.js";

/** The unprotected fields of `COSGN0A`. */
export interface SignonInputFields {
  readonly userid: string;
  readonly passwd: string;
}

export interface SignonScreenFields extends ScreenHeaderFields, SignonInputFields {
  readonly applid: string;
  readonly sysid: string;
  readonly errmsg: string;
}

export interface SignonOptions {
  readonly users: UserSecurityStore;
  readonly now?: Date;
  /** `EXEC CICS ASSIGN APPLID` / `SYSID`. */
  readonly applid?: string;
  readonly sysid?: string;
}

/** Messages `COSGN00C` sends to `ERRMSG`. */
export const SignonMessage = {
  missingUserId: "Please enter User ID ...",
  missingPassword: "Please enter Password ...",
  wrongPassword: "Wrong Password. Try again ...",
  userNotFound: "User not found. Try again ...",
  unableToVerify: "Unable to verify the User ...",
} as const;

export const passwordLength = 8;

interface SignonState {
  readonly message: string;
  readonly cursor?: string;
  readonly fields: SignonInputFields;
  readonly commarea: CardDemoCommarea;
}

function sendSignonScreen(
  state: SignonState,
  options: SignonOptions,
): HandlerResponse<SignonScreenFields> {
  const now = options.now ?? new Date();
  const screen: SignonScreenFields = {
    ...buildHeader(signonTransaction, signonProgram, now),
    applid: options.applid ?? "",
    sysid: options.sysid ?? "",
    userid: state.fields.userid,
    passwd: state.fields.passwd,
    errmsg: toFixed(toFixed(state.message, messageLength), errorMessageLength),
  };
  const response = {
    kind: "map",
    map: signonMap,
    mapset: signonMapset,
    transactionId: signonTransaction,
    fields: screen,
    commarea: state.commarea,
  } as const;
  return state.cursor === undefined ? response : { ...response, cursor: state.cursor };
}

function processEnterKey(
  fields: SignonInputFields,
  options: SignonOptions,
): HandlerResponse<SignonScreenFields> {
  const commarea = createCommarea();

  if (fields.userid.trim().length === 0) {
    return sendSignonScreen(
      { message: SignonMessage.missingUserId, cursor: "userid", fields, commarea },
      options,
    );
  }
  if (fields.passwd.trim().length === 0) {
    return sendSignonScreen(
      { message: SignonMessage.missingPassword, cursor: "passwd", fields, commarea },
      options,
    );
  }

  const userId = toFixed(fields.userid.toUpperCase(), userIdLength);
  const password = toFixed(fields.passwd.toUpperCase(), passwordLength);
  const signedOn = createCommarea({ userId: userId.trim() });
  const result = options.users.read(userId);

  if (result.resp === CicsResp.normal && result.record !== undefined) {
    if (toFixed(result.record.secUsrPwd, passwordLength) !== password) {
      return sendSignonScreen(
        {
          message: SignonMessage.wrongPassword,
          cursor: "passwd",
          fields,
          commarea: signedOn,
        },
        options,
      );
    }
    const userType = result.record.secUsrType.trim();
    return {
      kind: "transfer",
      program: userType === UserType.admin ? adminMenuProgram : mainMenuProgram,
      commarea: createCommarea({
        fromTranid: signonTransaction,
        fromProgram: signonProgram,
        userId: userId.trim(),
        userType,
        pgmContext: PgmContext.enter,
      }),
    };
  }

  const message =
    result.resp === CicsResp.notFound
      ? SignonMessage.userNotFound
      : SignonMessage.unableToVerify;
  return sendSignonScreen({ message, cursor: "userid", fields, commarea: signedOn }, options);
}

export function signon(
  request: HandlerRequest<SignonInputFields>,
  options: SignonOptions,
): HandlerResponse<SignonScreenFields> {
  const fields: SignonInputFields = {
    userid: request.fields?.userid ?? "",
    passwd: request.fields?.passwd ?? "",
  };

  if (request.commarea === undefined) {
    return sendSignonScreen(
      { message: "", cursor: "userid", fields: { userid: "", passwd: "" }, commarea: createCommarea() },
      options,
    );
  }

  switch (request.aid) {
    case AidKey.enter:
      return processEnterKey(fields, options);
    case AidKey.pf3:
      return { kind: "text", text: toFixed(CommonMessage.thankYou, messageLength) };
    default:
      return sendSignonScreen(
        { message: CommonMessage.invalidKey, fields, commarea: createCommarea() },
        options,
      );
  }
}
