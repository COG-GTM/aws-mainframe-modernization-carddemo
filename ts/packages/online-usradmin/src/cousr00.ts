/**
 * `COUSR00C` — list the users held on `USRSEC`, ten to a page, and route the
 * `U` and `D` selections to the update and delete programs.
 */

import type { UserSecurityRecord } from "@carddemo/domain";
import { FileStatus, type Ksds } from "@carddemo/vsam";

import {
  PgmContext,
  programs,
  transactions,
  type CardDemoCommarea,
} from "./commarea.js";
import { populateHeaderInfo } from "./header.js";
import { invalidKeyMessage, messages } from "./messages.js";
import {
  AidKey,
  MessageColor,
  type HandlerOptions,
  type HandlerRequest,
  type HandlerResponse,
  type ScreenBase,
} from "./screen.js";
import {
  UserSecurityBrowse,
  highValues,
  lowValues,
} from "./usrsec-file.js";

export const ROWS_PER_PAGE = 10;

/** One `SEL000n` / `USRIDnn` / `FNAMEnn` / `LNAMEnn` / `UTYPEnn` line. */
export interface UserListRow {
  sel: string;
  usrid: string;
  fname: string;
  lname: string;
  utype: string;
}

export interface UserListScreen extends ScreenBase {
  /** `USRIDIN` — the search key, and the field the program blanks after paging. */
  usridin: string;
  /** `PAGENUM`. */
  pagenum: number;
  rows: UserListRow[];
}

const emptyRow = (): UserListRow => ({ sel: "", usrid: "", fname: "", lname: "", utype: "" });

export function emptyUserListScreen(): UserListScreen {
  return {
    header: populateHeaderInfo(transactions.userList, programs.userList, new Date(0)),
    errmsg: "",
    errmsgColor: MessageColor.red,
    usridin: "",
    pagenum: 0,
    rows: Array.from({ length: ROWS_PER_PAGE }, emptyRow),
  };
}

const cloneScreen = (screen: UserListScreen): UserListScreen => ({
  ...screen,
  header: { ...screen.header },
  rows: screen.rows.map((row) => ({ ...row })),
});

const cloneCommarea = (commarea: CardDemoCommarea): CardDemoCommarea => ({
  ...commarea,
  userSelection: { ...commarea.userSelection },
});

const isBlank = (value: string): boolean => value.trim().length === 0;

const browseKey = (userId: string, whenBlank: string): string =>
  isBlank(userId) ? whenBlank : userId.trim().padEnd(8, " ");

export function handleUserList(
  request: HandlerRequest<UserListScreen>,
  file: Ksds<UserSecurityRecord>,
  options: HandlerOptions = {},
): HandlerResponse<UserListScreen> {
  const now = options.now ?? new Date();
  const screen = cloneScreen(request.screen);
  const commarea = cloneCommarea(request.commarea);
  const selection = commarea.userSelection;
  const browse = new UserSecurityBrowse(file);

  let errFlg = false;
  let eof = false;
  let sendErase = true;
  let message = "";
  let erase = true;

  screen.errmsg = "";
  screen.errmsgColor = MessageColor.red;

  const send = (): void => {
    screen.header = populateHeaderInfo(transactions.userList, programs.userList, now);
    screen.errmsg = message;
    erase = sendErase;
  };

  const transferTo = (program: string): HandlerResponse<UserListScreen> => {
    commarea.toProgram = isBlank(program) ? programs.signon : program;
    commarea.fromTranid = transactions.userList;
    commarea.fromProgram = programs.userList;
    commarea.pgmContext = PgmContext.enter;
    return { screen, commarea, nextProgram: commarea.toProgram, transfer: true, erase };
  };

  const populateUserData = (index: number, record: UserSecurityRecord): void => {
    const row = screen.rows[index - 1];
    if (row === undefined) {
      return;
    }
    row.usrid = record.secUsrId;
    row.fname = record.secUsrFname;
    row.lname = record.secUsrLname;
    row.utype = record.secUsrType;
    if (index === 1) {
      selection.usridFirst = record.secUsrId;
    }
    if (index === ROWS_PER_PAGE) {
      selection.usridLast = record.secUsrId;
    }
  };

  const initializeUserData = (): void => {
    for (const row of screen.rows) {
      row.usrid = "";
      row.fname = "";
      row.lname = "";
      row.utype = "";
    }
  };

  const startBrowse = (key: string): void => {
    const status = browse.startBrowse(key);
    if (status === FileStatus.ok) {
      return;
    }
    if (status === FileStatus.notFound) {
      eof = true;
      message = messages.topOfPage;
      send();
      return;
    }
    errFlg = true;
    message = messages.unableToLookupUser;
    send();
  };

  const readSequential = (direction: "next" | "prev"): UserSecurityRecord | undefined => {
    const result = direction === "next" ? browse.readNext() : browse.readPrev();
    if (result.status === FileStatus.ok) {
      return result.record;
    }
    if (result.status === FileStatus.endOfFile) {
      eof = true;
      message =
        direction === "next" ? messages.reachedBottomOfPage : messages.reachedTopOfPage;
      send();
      return undefined;
    }
    errFlg = true;
    message = messages.unableToLookupUser;
    send();
    return undefined;
  };

  const processPageForward = (key: string): void => {
    startBrowse(key);
    if (errFlg) {
      return;
    }

    if (request.aid !== AidKey.enter && request.aid !== AidKey.pf7 && request.aid !== AidKey.pf3) {
      readSequential("next");
    }

    if (!eof && !errFlg) {
      initializeUserData();
    }

    let index = 1;
    while (index <= ROWS_PER_PAGE && !eof && !errFlg) {
      const record = readSequential("next");
      if (record !== undefined) {
        populateUserData(index, record);
        index += 1;
      }
    }

    if (!eof && !errFlg) {
      selection.pageNum += 1;
      readSequential("next");
      selection.nextPageFlg = !eof && !errFlg ? "Y" : "N";
    } else {
      selection.nextPageFlg = "N";
      if (index > 1) {
        selection.pageNum += 1;
      }
    }

    browse.endBrowse();
    screen.pagenum = selection.pageNum;
    screen.usridin = "";
    send();
  };

  const processPageBackward = (key: string): void => {
    startBrowse(key);
    if (errFlg) {
      return;
    }

    if (request.aid !== AidKey.enter && request.aid !== AidKey.pf8) {
      readSequential("prev");
    }

    if (!eof && !errFlg) {
      initializeUserData();
    }

    let index = ROWS_PER_PAGE;
    while (index > 0 && !eof && !errFlg) {
      const record = readSequential("prev");
      if (record !== undefined) {
        populateUserData(index, record);
        index -= 1;
      }
    }

    if (!eof && !errFlg) {
      readSequential("prev");
      if (selection.nextPageFlg === "Y") {
        selection.pageNum = !eof && !errFlg && selection.pageNum > 1 ? selection.pageNum - 1 : 1;
      }
    }

    browse.endBrowse();
    screen.pagenum = selection.pageNum;
    send();
  };

  /** `PROCESS-ENTER-KEY`; returns the program to transfer to, if any. */
  const processEnterKey = (): string | undefined => {
    const selected = screen.rows.find((row) => !isBlank(row.sel));
    if (selected === undefined) {
      selection.usrSelFlg = "";
      selection.usrSelected = "";
    } else {
      selection.usrSelFlg = selected.sel;
      selection.usrSelected = selected.usrid;
    }

    if (!isBlank(selection.usrSelFlg) && !isBlank(selection.usrSelected)) {
      switch (selection.usrSelFlg.trim().toUpperCase()) {
        case "U":
          return programs.userUpdate;
        case "D":
          return programs.userDelete;
        default:
          message = messages.invalidSelection;
      }
    }

    selection.pageNum = 0;
    processPageForward(browseKey(screen.usridin, lowValues));
    return undefined;
  };

  if (commarea.pgmContext !== PgmContext.reenter) {
    commarea.pgmContext = PgmContext.reenter;
    const target = processEnterKey();
    if (target !== undefined) {
      return transferTo(target);
    }
    send();
  } else {
    switch (request.aid) {
      case AidKey.enter: {
        const target = processEnterKey();
        if (target !== undefined) {
          return transferTo(target);
        }
        break;
      }
      case AidKey.pf3:
        return transferTo(programs.adminMenu);
      case AidKey.pf7:
        selection.nextPageFlg = "Y";
        if (selection.pageNum > 1) {
          processPageBackward(browseKey(selection.usridFirst, lowValues));
        } else {
          message = messages.alreadyTopOfPage;
          sendErase = false;
          send();
        }
        break;
      case AidKey.pf8:
        if (selection.nextPageFlg === "Y") {
          processPageForward(browseKey(selection.usridLast, highValues));
        } else {
          message = messages.alreadyBottomOfPage;
          sendErase = false;
          send();
        }
        break;
      default:
        errFlg = true;
        message = invalidKeyMessage;
        send();
    }
  }

  return { screen, commarea, nextProgram: programs.userList, transfer: false, erase };
}
