import { mkdtempSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { FileStatus } from "@carddemo/vsam";
import { beforeEach, describe, expect, it } from "vitest";

import { PgmContext, emptyCommarea, programs, type CardDemoCommarea } from "./commarea.js";
import { emptyUserListScreen, handleUserList, type UserListScreen } from "./cousr00.js";
import { messages } from "./messages.js";
import { AidKey, type AidKeyCode } from "./screen.js";
import { openUserSecurityFile, writeUsrsecAscii } from "./usrsec-file.js";

const openUsrsec = () => {
  const path = writeUsrsecAscii(join(mkdtempSync(join(tmpdir(), "usrsec-")), "usrsec.txt"));
  const file = openUserSecurityFile(path);
  expect(file.openFile()).toBe(FileStatus.ok);
  return file;
};

const signedOn = (): CardDemoCommarea => ({
  ...emptyCommarea(),
  userId: "ADMIN001",
  userType: "A",
  pgmContext: PgmContext.reenter,
});

describe("COUSR00C user list", () => {
  let file: ReturnType<typeof openUsrsec>;

  beforeEach(() => {
    file = openUsrsec();
  });

  const list = (
    aid: AidKeyCode,
    screen: UserListScreen,
    commarea: CardDemoCommarea,
  ): { screen: UserListScreen; commarea: CardDemoCommarea; nextProgram: string } =>
    handleUserList({ aid, screen, commarea }, file, { persist: false });

  it("fills the first page from the first key on entry", () => {
    const result = list(AidKey.enter, emptyUserListScreen(), {
      ...signedOn(),
      pgmContext: PgmContext.enter,
    });

    expect(result.screen.rows[0]?.usrid).toBe("ADMIN001");
    expect(result.screen.rows[9]?.usrid).toBe("USER0005");
    expect(result.screen.pagenum).toBe(1);
    expect(result.commarea.userSelection.usridFirst).toBe("ADMIN001");
    expect(result.commarea.userSelection.usridLast).toBe("USER0005");
    expect(result.commarea.userSelection.nextPageFlg).toBe("N");
    expect(result.screen.errmsg).toBe(messages.reachedBottomOfPage);
  });

  it("starts the browse at the requested user id", () => {
    const screen = { ...emptyUserListScreen(), usridin: "USER0002" };
    const result = list(AidKey.enter, screen, { ...signedOn(), pgmContext: PgmContext.enter });

    expect(result.screen.rows[0]?.usrid).toBe("USER0002");
    expect(result.screen.rows[3]?.usrid).toBe("USER0005");
    expect(result.screen.rows[4]?.usrid).toBe("");
    expect(result.screen.usridin).toBe("");
    expect(result.screen.errmsg).toBe(messages.reachedBottomOfPage);
  });

  it("refuses to page past the last page", () => {
    const first = list(AidKey.enter, emptyUserListScreen(), {
      ...signedOn(),
      pgmContext: PgmContext.enter,
    });

    const second = list(AidKey.pf8, first.screen, first.commarea);
    expect(second.screen.errmsg).toBe(messages.alreadyBottomOfPage);
    expect(second.screen.pagenum).toBe(1);
  });

  it("pages forward and back over a longer file", () => {
    for (const id of ["USER0006", "USER0007", "USER0008"]) {
      expect(
        file.write({
          secUsrId: id,
          secUsrFname: "TEST",
          secUsrLname: "USER",
          secUsrPwd: "PASSWORD",
          secUsrType: "U",
        }),
      ).toBe(FileStatus.ok);
    }

    const page1 = list(AidKey.enter, emptyUserListScreen(), {
      ...signedOn(),
      pgmContext: PgmContext.enter,
    });
    expect(page1.commarea.userSelection.nextPageFlg).toBe("Y");

    const page2 = list(AidKey.pf8, page1.screen, page1.commarea);
    expect(page2.screen.pagenum).toBe(2);
    expect(page2.screen.rows[0]?.usrid).toBe("USER0006");
    expect(page2.screen.rows[2]?.usrid).toBe("USER0008");
    expect(page2.commarea.userSelection.nextPageFlg).toBe("N");

    const back = list(AidKey.pf7, page2.screen, page2.commarea);
    expect(back.screen.pagenum).toBe(1);
    expect(back.screen.rows[0]?.usrid).toBe("ADMIN001");
    expect(back.screen.rows[9]?.usrid).toBe("USER0005");

    const top = list(AidKey.pf7, back.screen, back.commarea);
    expect(top.screen.errmsg).toBe(messages.alreadyTopOfPage);
  });

  it("routes a U selection to the update program and a D selection to delete", () => {
    const page = list(AidKey.enter, emptyUserListScreen(), {
      ...signedOn(),
      pgmContext: PgmContext.enter,
    });

    const update = list(
      AidKey.enter,
      {
        ...page.screen,
        rows: page.screen.rows.map((row, index) => (index === 5 ? { ...row, sel: "U" } : row)),
      },
      page.commarea,
    );
    expect(update.nextProgram).toBe(programs.userUpdate);
    expect(update.commarea.userSelection.usrSelected).toBe("USER0001");

    const remove = list(
      AidKey.enter,
      {
        ...page.screen,
        rows: page.screen.rows.map((row, index) => (index === 0 ? { ...row, sel: "d" } : row)),
      },
      page.commarea,
    );
    expect(remove.nextProgram).toBe(programs.userDelete);
    expect(remove.commarea.userSelection.usrSelected).toBe("ADMIN001");
  });

  it("rejects a selection other than U or D", () => {
    expect(
      file.write({
        secUsrId: "USER0006",
        secUsrFname: "TEST",
        secUsrLname: "USER",
        secUsrPwd: "PASSWORD",
        secUsrType: "U",
      }),
    ).toBe(FileStatus.ok);

    const page = list(AidKey.enter, emptyUserListScreen(), {
      ...signedOn(),
      pgmContext: PgmContext.enter,
    });

    const result = list(
      AidKey.enter,
      {
        ...page.screen,
        rows: page.screen.rows.map((row, index) => (index === 0 ? { ...row, sel: "X" } : row)),
      },
      page.commarea,
    );
    expect(result.screen.errmsg).toBe(messages.invalidSelection);
    expect(result.nextProgram).toBe(programs.userList);
  });

  it("returns to the admin menu on PF3 and rejects any other key", () => {
    const page = list(AidKey.enter, emptyUserListScreen(), {
      ...signedOn(),
      pgmContext: PgmContext.enter,
    });

    expect(list(AidKey.pf3, page.screen, page.commarea).nextProgram).toBe(programs.adminMenu);
    expect(list(AidKey.pf5, page.screen, page.commarea).screen.errmsg).toBe(
      "Invalid key pressed. Please see below...         ",
    );
  });
});
