/**
 * `COUSR01C` — add a user to `USRSEC`.
 */

import type { UserSecurityRecord } from "@carddemo/domain";
import { FileStatus, type Ksds } from "@carddemo/vsam";

import { PgmContext, programs, transactions, type CardDemoCommarea } from "./commarea.js";
import { populateHeaderInfo } from "./header.js";
import { invalidKeyMessage, messages, userAddedMessage } from "./messages.js";
import {
  AidKey,
  MessageColor,
  type HandlerOptions,
  type HandlerRequest,
  type HandlerResponse,
  type ScreenBase,
} from "./screen.js";

export interface UserAddScreen extends ScreenBase {
  fname: string;
  lname: string;
  userid: string;
  passwd: string;
  usrtype: string;
}

export function emptyUserAddScreen(): UserAddScreen {
  return {
    header: populateHeaderInfo(transactions.userAdd, programs.userAdd, new Date(0)),
    errmsg: "",
    errmsgColor: MessageColor.red,
    fname: "",
    lname: "",
    userid: "",
    passwd: "",
    usrtype: "",
  };
}

const isBlank = (value: string): boolean => value.trim().length === 0;

export function handleUserAdd(
  request: HandlerRequest<UserAddScreen>,
  file: Ksds<UserSecurityRecord>,
  options: HandlerOptions = {},
): HandlerResponse<UserAddScreen> {
  const now = options.now ?? new Date();
  const persist = options.persist ?? true;
  const screen: UserAddScreen = { ...request.screen, header: { ...request.screen.header } };
  const commarea: CardDemoCommarea = {
    ...request.commarea,
    userSelection: { ...request.commarea.userSelection },
  };

  let errFlg = false;
  let message = "";

  screen.errmsg = "";
  screen.errmsgColor = MessageColor.red;

  const send = (): void => {
    screen.header = populateHeaderInfo(transactions.userAdd, programs.userAdd, now);
    screen.errmsg = message;
  };

  const transferTo = (program: string): HandlerResponse<UserAddScreen> => {
    commarea.toProgram = isBlank(program) ? programs.signon : program;
    commarea.fromTranid = transactions.userAdd;
    commarea.fromProgram = programs.userAdd;
    commarea.pgmContext = PgmContext.enter;
    return { screen, commarea, nextProgram: commarea.toProgram, transfer: true, erase: true };
  };

  const initializeAllFields = (): void => {
    screen.userid = "";
    screen.fname = "";
    screen.lname = "";
    screen.passwd = "";
    screen.usrtype = "";
    message = "";
  };

  const writeUserSecFile = (record: UserSecurityRecord): void => {
    const status = file.write(record);
    if (status === FileStatus.ok) {
      initializeAllFields();
      screen.errmsgColor = MessageColor.green;
      message = userAddedMessage(record.secUsrId);
      if (persist) {
        file.save();
      }
      send();
      return;
    }
    errFlg = true;
    message =
      status === FileStatus.duplicateKey
        ? messages.userIdAlreadyExists
        : messages.unableToAddUser;
    send();
  };

  const processEnterKey = (): void => {
    if (isBlank(screen.fname)) {
      errFlg = true;
      message = messages.firstNameEmpty;
    } else if (isBlank(screen.lname)) {
      errFlg = true;
      message = messages.lastNameEmpty;
    } else if (isBlank(screen.userid)) {
      errFlg = true;
      message = messages.userIdEmpty;
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

    writeUserSecFile({
      secUsrId: screen.userid,
      secUsrFname: screen.fname,
      secUsrLname: screen.lname,
      secUsrPwd: screen.passwd,
      secUsrType: screen.usrtype,
    });
  };

  if (commarea.pgmContext !== PgmContext.reenter) {
    commarea.pgmContext = PgmContext.reenter;
    initializeAllFields();
    send();
  } else {
    switch (request.aid) {
      case AidKey.enter:
        processEnterKey();
        break;
      case AidKey.pf3:
        return transferTo(programs.adminMenu);
      case AidKey.pf4:
        initializeAllFields();
        send();
        break;
      default:
        errFlg = true;
        message = invalidKeyMessage;
        send();
    }
  }

  return { screen, commarea, nextProgram: programs.userAdd, transfer: false, erase: true };
}
