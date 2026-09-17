/**
 * `COUSR02C` — update a user held on `USRSEC`.
 */

import type { UserSecurityRecord } from "@carddemo/domain";
import { FileStatus, type Ksds } from "@carddemo/vsam";

import { PgmContext, programs, transactions, type CardDemoCommarea } from "./commarea.js";
import { populateHeaderInfo } from "./header.js";
import { invalidKeyMessage, messages, userUpdatedMessage } from "./messages.js";
import {
  AidKey,
  MessageColor,
  type HandlerOptions,
  type HandlerRequest,
  type HandlerResponse,
  type ScreenBase,
} from "./screen.js";

export interface UserUpdateScreen extends ScreenBase {
  usridin: string;
  fname: string;
  lname: string;
  passwd: string;
  usrtype: string;
}

export function emptyUserUpdateScreen(): UserUpdateScreen {
  return {
    header: populateHeaderInfo(transactions.userUpdate, programs.userUpdate, new Date(0)),
    errmsg: "",
    errmsgColor: MessageColor.red,
    usridin: "",
    fname: "",
    lname: "",
    passwd: "",
    usrtype: "",
  };
}

const isBlank = (value: string): boolean => value.trim().length === 0;

/** COBOL compares fixed-length fields, so trailing spaces never differ. */
const sameField = (left: string, right: string): boolean => left.trimEnd() === right.trimEnd();

/** `SEC-USER-DATA` before the first successful read: all spaces. */
const blankUserRecord = (): UserSecurityRecord => ({
  secUsrId: "",
  secUsrFname: "",
  secUsrLname: "",
  secUsrPwd: "",
  secUsrType: "",
});

export function handleUserUpdate(
  request: HandlerRequest<UserUpdateScreen>,
  file: Ksds<UserSecurityRecord>,
  options: HandlerOptions = {},
): HandlerResponse<UserUpdateScreen> {
  const now = options.now ?? new Date();
  const persist = options.persist ?? true;
  const screen: UserUpdateScreen = { ...request.screen, header: { ...request.screen.header } };
  const commarea: CardDemoCommarea = {
    ...request.commarea,
    userSelection: { ...request.commarea.userSelection },
  };

  let errFlg = false;
  let usrModified = false;
  let message = "";
  let secUser = blankUserRecord();

  screen.errmsg = "";
  screen.errmsgColor = MessageColor.red;

  const send = (): void => {
    screen.header = populateHeaderInfo(transactions.userUpdate, programs.userUpdate, now);
    screen.errmsg = message;
  };

  const transferTo = (program: string): HandlerResponse<UserUpdateScreen> => {
    commarea.toProgram = isBlank(program) ? programs.signon : program;
    commarea.fromTranid = transactions.userUpdate;
    commarea.fromProgram = programs.userUpdate;
    commarea.pgmContext = PgmContext.enter;
    return { screen, commarea, nextProgram: commarea.toProgram, transfer: true, erase: true };
  };

  const initializeAllFields = (): void => {
    screen.usridin = "";
    screen.fname = "";
    screen.lname = "";
    screen.passwd = "";
    screen.usrtype = "";
    message = "";
  };

  /** `EXEC CICS READ ... UPDATE`. */
  const readUserSecFile = (): void => {
    const result = file.read(secUser.secUsrId.padEnd(8, " "));
    if (result.status === FileStatus.ok && result.record !== undefined) {
      secUser = { ...result.record };
      message = messages.pressPf5ToSave;
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

  const updateUserSecFile = (): void => {
    const status = file.rewrite(secUser);
    if (status === FileStatus.ok) {
      screen.errmsgColor = MessageColor.green;
      message = userUpdatedMessage(secUser.secUsrId);
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
      screen.passwd = "";
      screen.usrtype = "";
      secUser = { ...secUser, secUsrId: screen.usridin.trim() };
      readUserSecFile();
    }

    if (!errFlg) {
      screen.fname = secUser.secUsrFname;
      screen.lname = secUser.secUsrLname;
      screen.passwd = secUser.secUsrPwd;
      screen.usrtype = secUser.secUsrType;
      send();
    }
  };

  const updateUserInfo = (): void => {
    if (isBlank(screen.usridin)) {
      errFlg = true;
      message = messages.userIdEmpty;
    } else if (isBlank(screen.fname)) {
      errFlg = true;
      message = messages.firstNameEmpty;
    } else if (isBlank(screen.lname)) {
      errFlg = true;
      message = messages.lastNameEmpty;
    } else if (isBlank(screen.passwd)) {
      errFlg = true;
      message = messages.passwordEmpty;
    } else if (isBlank(screen.usrtype)) {
      errFlg = true;
      message = messages.userTypeEmpty;
    }

    if (errFlg) {
      send();
      return;
    }

    secUser = { ...secUser, secUsrId: screen.usridin.trim() };
    readUserSecFile();

    if (!sameField(screen.fname, secUser.secUsrFname)) {
      secUser = { ...secUser, secUsrFname: screen.fname.trimEnd() };
      usrModified = true;
    }
    if (!sameField(screen.lname, secUser.secUsrLname)) {
      secUser = { ...secUser, secUsrLname: screen.lname.trimEnd() };
      usrModified = true;
    }
    if (!sameField(screen.passwd, secUser.secUsrPwd)) {
      secUser = { ...secUser, secUsrPwd: screen.passwd.trimEnd() };
      usrModified = true;
    }
    if (!sameField(screen.usrtype, secUser.secUsrType)) {
      secUser = { ...secUser, secUsrType: screen.usrtype.trimEnd() };
      usrModified = true;
    }

    if (usrModified) {
      updateUserSecFile();
    } else {
      message = messages.modifyToUpdate;
      screen.errmsgColor = MessageColor.red;
      send();
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
      case AidKey.pf3: {
        updateUserInfo();
        const target = isBlank(commarea.fromProgram) ? programs.adminMenu : commarea.fromProgram;
        return transferTo(target);
      }
      case AidKey.pf4:
        initializeAllFields();
        send();
        break;
      case AidKey.pf5:
        updateUserInfo();
        break;
      case AidKey.pf12:
        return transferTo(programs.adminMenu);
      default:
        errFlg = true;
        message = invalidKeyMessage;
        send();
    }
  }

  return { screen, commarea, nextProgram: programs.userUpdate, transfer: false, erase: true };
}
