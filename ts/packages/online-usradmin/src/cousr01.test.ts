import { mkdtempSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { FileStatus } from "@carddemo/vsam";
import { beforeEach, describe, expect, it } from "vitest";

import { PgmContext, emptyCommarea, programs, type CardDemoCommarea } from "./commarea.js";
import { emptyUserAddScreen, handleUserAdd, type UserAddScreen } from "./cousr01.js";
import { messages, userAddedMessage } from "./messages.js";
import { AidKey, MessageColor, type AidKeyCode } from "./screen.js";
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

const filled = (overrides: Partial<UserAddScreen> = {}): UserAddScreen => ({
  ...emptyUserAddScreen(),
  fname: "JANE",
  lname: "DOE",
  userid: "USER0099",
  passwd: "PASSWORD",
  usrtype: "U",
  ...overrides,
});

describe("COUSR01C add user", () => {
  let file: ReturnType<typeof openUsrsec>;

  beforeEach(() => {
    file = openUsrsec();
  });

  const add = (aid: AidKeyCode, screen: UserAddScreen, commarea = signedOn()) =>
    handleUserAdd({ aid, screen, commarea }, file, { persist: false });

  it("writes the user and clears the screen", () => {
    const result = add(AidKey.enter, filled());

    expect(result.screen.errmsg).toBe(userAddedMessage("USER0099"));
    expect(result.screen.errmsgColor).toBe(MessageColor.green);
    expect(result.screen.userid).toBe("");
    expect(file.read("USER0099").record?.secUsrFname).toBe("JANE");
  });

  it("reports a duplicate user id (FILE STATUS 22)", () => {
    const result = add(AidKey.enter, filled({ userid: "ADMIN001" }));

    expect(result.screen.errmsg).toBe(messages.userIdAlreadyExists);
    expect(result.screen.errmsgColor).toBe(MessageColor.red);
    expect(file.read("ADMIN001").record?.secUsrFname).toBe("MARGARET");
  });

  it.each([
    [{ fname: "  " }, messages.firstNameEmpty],
    [{ lname: "" }, messages.lastNameEmpty],
    [{ userid: "" }, messages.userIdEmpty],
    [{ passwd: "" }, messages.passwordEmpty],
    [{ usrtype: "" }, messages.userTypeEmpty],
  ])("rejects %j", (overrides, expected) => {
    const result = add(AidKey.enter, filled(overrides));

    expect(result.screen.errmsg).toBe(expected);
    expect(file.read("USER0099").status).toBe(FileStatus.notFound);
  });

  it("clears the screen on PF4 and returns to the admin menu on PF3", () => {
    const cleared = add(AidKey.pf4, filled());
    expect(cleared.screen.userid).toBe("");
    expect(cleared.transfer).toBe(false);

    const back = add(AidKey.pf3, filled());
    expect(back.transfer).toBe(true);
    expect(back.nextProgram).toBe(programs.adminMenu);
  });

  it("rejects any other key", () => {
    expect(add(AidKey.pf7, filled()).screen.errmsg).toBe(
      "Invalid key pressed. Please see below...         ",
    );
  });
});
