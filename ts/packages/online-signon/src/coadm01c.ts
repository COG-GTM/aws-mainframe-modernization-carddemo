/**
 * `COADM01C` — the CardDemo admin menu.
 *
 * Same shape as the main menu, over the `COADM02Y` option list. The admin
 * options carry no user type, and an option whose program is not installed
 * raises `PGMIDERR`, which the program turns into a message on the screen.
 */

import { isReenter, PgmContext, type CardDemoCommarea } from "./commarea.js";
import { adminMenuOptions, parseOptionNumber } from "./menu-options.js";
import {
  buildOptionLines,
  echoOption,
  invalidOptionMessage,
  type MenuInputFields,
  type MenuScreenFields,
  type MessageColor,
} from "./menu-screen.js";
import {
  adminMenuMap,
  adminMenuMapset,
  adminMenuProgram,
  adminMenuTransaction,
  signonProgram,
} from "./programs.js";
import {
  AidKey,
  buildHeader,
  CommonMessage,
  errorMessageLength,
  messageLength,
  toFixed,
  type HandlerRequest,
  type HandlerResponse,
} from "./screen.js";

export const notInstalledMessage = "This option is not installed ...";

export interface AdminMenuOptions {
  readonly now?: Date;
  /**
   * Whether the target program is installed; an `XCTL` to a program that is
   * not installed drives the `PGMIDERR` handler.
   */
  readonly isProgramInstalled?: (program: string) => boolean;
}

interface AdminMenuState {
  readonly message: string;
  readonly color: MessageColor;
  readonly option: string;
  readonly commarea: CardDemoCommarea;
}

function sendMenuScreen(
  state: AdminMenuState,
  options: AdminMenuOptions,
): HandlerResponse<MenuScreenFields> {
  const now = options.now ?? new Date();
  return {
    kind: "map",
    map: adminMenuMap,
    mapset: adminMenuMapset,
    transactionId: adminMenuTransaction,
    commarea: state.commarea,
    fields: {
      ...buildHeader(adminMenuTransaction, adminMenuProgram, now),
      ...buildOptionLines(adminMenuOptions),
      option: state.option,
      errmsg: toFixed(toFixed(state.message, messageLength), errorMessageLength),
      errmsgColor: state.color,
    },
  };
}

function processEnterKey(
  fields: MenuInputFields,
  commarea: CardDemoCommarea,
  options: AdminMenuOptions,
): HandlerResponse<MenuScreenFields> {
  const parsed = parseOptionNumber(fields.option);
  const echoed = echoOption(parsed, fields.option);
  const option =
    parsed === undefined || parsed === 0 || parsed > adminMenuOptions.length
      ? undefined
      : adminMenuOptions[parsed - 1];

  if (option === undefined) {
    return sendMenuScreen(
      { message: invalidOptionMessage, color: "DEFAULT", option: echoed, commarea },
      options,
    );
  }

  const isInstalled = options.isProgramInstalled ?? ((): boolean => true);
  if (!option.pgmName.startsWith("DUMMY") && isInstalled(option.pgmName)) {
    return {
      kind: "transfer",
      program: option.pgmName,
      commarea: {
        ...commarea,
        fromTranid: adminMenuTransaction,
        fromProgram: adminMenuProgram,
        pgmContext: PgmContext.enter,
      },
    };
  }

  return sendMenuScreen(
    { message: notInstalledMessage, color: "GREEN", option: echoed, commarea },
    options,
  );
}

export function adminMenu(
  request: HandlerRequest<MenuInputFields>,
  options: AdminMenuOptions = {},
): HandlerResponse<MenuScreenFields> {
  if (request.commarea === undefined) {
    return { kind: "transfer", program: signonProgram };
  }

  const commarea = request.commarea;
  if (!isReenter(commarea)) {
    return sendMenuScreen(
      {
        message: "",
        color: "DEFAULT",
        option: "",
        commarea: { ...commarea, pgmContext: PgmContext.reenter },
      },
      options,
    );
  }

  const fields: MenuInputFields = { option: request.fields?.option ?? "" };

  switch (request.aid) {
    case AidKey.enter:
      return processEnterKey(fields, commarea, options);
    case AidKey.pf3:
      return { kind: "transfer", program: signonProgram };
    default:
      return sendMenuScreen(
        {
          message: CommonMessage.invalidKey,
          color: "DEFAULT",
          option: fields.option,
          commarea,
        },
        options,
      );
  }
}
