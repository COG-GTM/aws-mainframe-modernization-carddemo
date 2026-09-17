/**
 * `COUSR03C` — delete a user from `USRSEC`, after the PF5 confirmation.
 */

import type { UserSecurityRecord } from "@carddemo/domain";
import { FileStatus, type Ksds } from "@carddemo/vsam";

import { PgmContext, programs, transactions, type CardDemoCommarea } from "./commarea.js";
import { populateHeaderInfo } from "./header.js";
import { invalidKeyMessage, messages, userDeletedMessage } from "./messages.js";
import {
  AidKey,
  MessageColor,
  type HandlerOptions,
  type HandlerRequest,
  type HandlerResponse,
  type ScreenBase,
} from "./screen.js";

export interface UserDeleteScreen extends ScreenBase {
  usridin: string;
  fname: string;
  lname: string;
  usrtype: string;
}

export function emptyUserDeleteScreen(): UserDeleteScreen {
  return {
    header: populateHeaderInfo(transactions.userDelete, programs.userDelete, new Date(0)),
    errmsg: "",
    errmsgColor: MessageColor.red,
    usridin: "",
    fname: "",
    lname: "",
    usrtype: "",
  };
}

const isBlank = (value: string): boolean => value.trim().length === 0;

export function handleUserDelete(
  request: HandlerRequest<UserDeleteScreen>,
  file: Ksds<UserSecurityRecord>,
  options: HandlerOptions = {},
): HandlerResponse<UserDeleteScreen> {
  const now = options.now ?? new Date();
  const persist = options.persist ?? true;
  const screen: UserDeleteScreen = { ...request.screen, header: { ...request.screen.header } };
  const commarea: CardDemoCommarea = {
    ...request.commarea,
    userSelection: { ...request.commarea.userSelection },
  };

  let errFlg = false;
  let message = "";
  let secUsrId = "";
  let secUser: UserSecurityRecord | undefined;

  screen.errmsg = "";
  screen.errmsgColor = MessageColor.red;

  const send = (): void => {
    screen.header = populateHeaderInfo(transactions.userDelete, programs.userDelete, now);
    screen.errmsg = message;
  };

  const transferTo = (program: string): HandlerResponse<UserDeleteScreen> => {
    commarea.toProgram = isBlank(program) ? programs.signon : program;
    commarea.fromTranid = transactions.userDelete;
    commarea.fromProgram = programs.userDelete;
    commarea.pgmContext = PgmContext.enter;
    return { screen, commarea, nextProgram: commarea.toProgram, transfer: true, erase: true };
  };

  const initializeAllFields = (): void => {
    screen.usridin = "";
    screen.fname = "";
    screen.lname = "";
    screen.usrtype = "";
    message = "";
  };

  /** `EXEC CICS READ ... UPDATE`; the read that shows the PF5 confirmation. */
  const readUserSecFile = (): void => {
    const result = file.read(secUsrId.padEnd(8, " "));
    if (result.status === FileStatus.ok && result.record !== undefined) {
      secUser = result.record;
      message = messages.pressPf5ToDelete;
      screen.errmsgColor = MessageColor.neutral;
      send();
      return;
    }
    errFlg = true;
    message =
      result.status === FileStatus.notFound
        ? messages.userIdNotFound
        : messages.unableToLookupUser;
    send();
  };

  const deleteUserSecFile = (): void => {
    const status = file.delete(secUsrId.padEnd(8, " "));
    if (status === FileStatus.ok) {
      initializeAllFields();
      screen.errmsgColor = MessageColor.green;
      message = userDeletedMessage(secUsrId);
      if (persist) {
        file.save();
      }
      send();
      return;
    }
    errFlg = true;
    message =
      status === FileStatus.notFound ? messages.userIdNotFound : messages.unableToUpdateUser;
    send();
  };

  const processEnterKey = (): void => {
    if (isBlank(screen.usridin)) {
      errFlg = true;
      message = messages.userIdEmpty;
      send();
    }

    if (!errFlg) {
      screen.fname = "";
      screen.lname = "";
      screen.usrtype = "";
      secUsrId = screen.usridin.trim();
      readUserSecFile();
    }

    if (!errFlg && secUser !== undefined) {
      screen.fname = secUser.secUsrFname;
      screen.lname = secUser.secUsrLname;
      screen.usrtype = secUser.secUsrType;
      send();
    }
  };

  const deleteUserInfo = (): void => {
    if (isBlank(screen.usridin)) {
      errFlg = true;
      message = messages.userIdEmpty;
      send();
    }

    if (!errFlg) {
      secUsrId = screen.usridin.trim();
      readUserSecFile();
      deleteUserSecFile();
    }
  };

  if (commarea.pgmContext !== PgmContext.reenter) {
    commarea.pgmContext = PgmContext.reenter;
    if (!isBlank(commarea.userSelection.usrSelected)) {
      screen.usridin = commarea.userSelection.usrSelected;
      processEnterKey();
    }
    send();
  } else {
    switch (request.aid) {
      case AidKey.enter:
        processEnterKey();
        break;
      case AidKey.pf3:
        return transferTo(
          isBlank(commarea.fromProgram) ? programs.adminMenu : commarea.fromProgram,
        );
      case AidKey.pf4:
        initializeAllFields();
        send();
        break;
      case AidKey.pf5:
        deleteUserInfo();
        break;
      case AidKey.pf12:
        return transferTo(programs.adminMenu);
      default:
        errFlg = true;
        message = invalidKeyMessage;
        send();
    }
  }

  return { screen, commarea, nextProgram: programs.userDelete, transfer: false, erase: true };
}
