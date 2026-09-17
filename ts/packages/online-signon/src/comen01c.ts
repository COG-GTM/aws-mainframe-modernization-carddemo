/**
 * `COMEN01C` — the CardDemo main menu.
 *
 * Builds the `COMEN02Y` option list, validates the typed option against the
 * option count and the signed-on user type, and transfers to the program the
 * option names.
 */

import { isReenter, PgmContext, UserType, type CardDemoCommarea } from "./commarea.js";
import {
  mainMenuOptions,
  optionNameLength,
  parseOptionNumber,
  type MenuOption,
} from "./menu-options.js";
import {
  buildOptionLines,
  delimitedBy,
  echoOption,
  invalidOptionMessage,
  type MenuInputFields,
  type MenuScreenFields,
  type MessageColor,
} from "./menu-screen.js";
import {
  AidKey,
  buildHeader,
  CommonMessage,
  errorMessageLength,
  messageLength,
  toFixed,
  type HandlerRequest,
  type HandlerResponse,
  type TransferResponse,
} from "./screen.js";
import {
  mainMenuMap,
  mainMenuMapset,
  mainMenuProgram,
  mainMenuTransaction,
  signonProgram,
} from "./programs.js";

/** Option that is routed through `EXEC CICS INQUIRE PROGRAM` before the XCTL. */
export const inquiredProgram = "COPAUS0C";

export const adminOnlyMessage = "No access - Admin Only option... ";

export interface MenuOptions {
  readonly now?: Date;
  /**
   * `EXEC CICS INQUIRE PROGRAM`: whether the target program is installed.
   * Defaults to treating every program as installed.
   */
  readonly isProgramInstalled?: (program: string) => boolean;
}

interface MenuState {
  readonly message: string;
  readonly color: MessageColor;
  readonly option: string;
  readonly commarea: CardDemoCommarea;
}

function sendMenuScreen(
  state: MenuState,
  options: MenuOptions,
): HandlerResponse<MenuScreenFields> {
  const now = options.now ?? new Date();
  return {
    kind: "map",
    map: mainMenuMap,
    mapset: mainMenuMapset,
    transactionId: mainMenuTransaction,
    commarea: state.commarea,
    fields: {
      ...buildHeader(mainMenuTransaction, mainMenuProgram, now),
      ...buildOptionLines(mainMenuOptions),
      option: state.option,
      errmsg: toFixed(toFixed(state.message, messageLength), errorMessageLength),
      errmsgColor: state.color,
    },
  };
}

function transferTo(option: MenuOption, commarea: CardDemoCommarea): TransferResponse {
  return {
    kind: "transfer",
    program: option.pgmName,
    commarea: {
      ...commarea,
      fromTranid: mainMenuTransaction,
      fromProgram: mainMenuProgram,
      pgmContext: PgmContext.enter,
    },
  };
}

function processEnterKey(
  fields: MenuInputFields,
  commarea: CardDemoCommarea,
  options: MenuOptions,
): HandlerResponse<MenuScreenFields> {
  const parsed = parseOptionNumber(fields.option);
  const echoed = echoOption(parsed, fields.option);
  const option =
    parsed === undefined || parsed === 0 || parsed > mainMenuOptions.length
      ? undefined
      : mainMenuOptions[parsed - 1];

  if (option === undefined) {
    return sendMenuScreen(
      { message: invalidOptionMessage, color: "DEFAULT", option: echoed, commarea },
      options,
    );
  }

  if (commarea.userType === UserType.user && option.usrType === UserType.admin) {
    return sendMenuScreen(
      { message: adminOnlyMessage, color: "DEFAULT", option: echoed, commarea },
      options,
    );
  }

  const isInstalled = options.isProgramInstalled ?? ((): boolean => true);
  const name = toFixed(option.name, optionNameLength);

  if (option.pgmName === inquiredProgram) {
    if (isInstalled(option.pgmName)) {
      return transferTo(option, commarea);
    }
    return sendMenuScreen(
      {
        message: `This option ${delimitedBy(name, "  ")} is not installed...`,
        color: "RED",
        option: echoed,
        commarea,
      },
      options,
    );
  }

  if (option.pgmName.startsWith("DUMMY")) {
    return sendMenuScreen(
      {
        message: `This option ${delimitedBy(name, " ")}is coming soon ...`,
        color: "GREEN",
        option: echoed,
        commarea,
      },
      options,
    );
  }

  return transferTo(option, commarea);
}

export function mainMenu(
  request: HandlerRequest<MenuInputFields>,
  options: MenuOptions = {},
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
      return {
        kind: "transfer",
        program: signonProgram,
      };
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
