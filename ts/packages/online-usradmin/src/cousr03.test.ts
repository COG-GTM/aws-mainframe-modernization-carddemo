import { mkdtempSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { FileStatus } from "@carddemo/vsam";
import { beforeEach, describe, expect, it } from "vitest";

import { PgmContext, emptyCommarea, programs, type CardDemoCommarea } from "./commarea.js";
import { emptyUserDeleteScreen, handleUserDelete, type UserDeleteScreen } from "./cousr03.js";
import { messages, userDeletedMessage } from "./messages.js";
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

describe("COUSR03C delete user", () => {
  let file: ReturnType<typeof openUsrsec>;

  beforeEach(() => {
    file = openUsrsec();
  });

  const remove = (aid: AidKeyCode, screen: UserDeleteScreen, commarea = signedOn()) =>
    handleUserDelete({ aid, screen, commarea }, file, { persist: false });

  it("shows the user and asks for the PF5 confirmation", () => {
    const result = remove(AidKey.enter, { ...emptyUserDeleteScreen(), usridin: "USER0003" });

    expect(result.screen.fname).toBe("LAURITZ");
    expect(result.screen.lname).toBe("ALME");
    expect(result.screen.usrtype).toBe("U");
    expect(result.screen.errmsg).toBe(messages.pressPf5ToDelete);
    expect(result.screen.errmsgColor).toBe(MessageColor.neutral);
    expect(file.read("USER0003").status).toBe(FileStatus.ok);
  });

  it("deletes the user on PF5", () => {
    const shown = remove(AidKey.enter, { ...emptyUserDeleteScreen(), usridin: "USER0003" });
    const deleted = remove(AidKey.pf5, shown.screen);

    expect(deleted.screen.errmsg).toBe(userDeletedMessage("USER0003"));
    expect(deleted.screen.errmsgColor).toBe(MessageColor.green);
    expect(deleted.screen.usridin).toBe("");
    expect(file.read("USER0003").status).toBe(FileStatus.notFound);
  });

  it("keeps the user when the confirmation is cancelled", () => {
    const shown = remove(AidKey.enter, { ...emptyUserDeleteScreen(), usridin: "USER0003" });

    const cleared = remove(AidKey.pf4, shown.screen);
    expect(cleared.screen.usridin).toBe("");
    expect(file.read("USER0003").status).toBe(FileStatus.ok);

    const left = remove(AidKey.pf12, shown.screen);
    expect(left.transfer).toBe(true);
    expect(left.nextProgram).toBe(programs.adminMenu);
    expect(file.read("USER0003").status).toBe(FileStatus.ok);
  });

  it("reports a missing user id (FILE STATUS 23)", () => {
    const result = remove(AidKey.enter, { ...emptyUserDeleteScreen(), usridin: "NOSUCH01" });

    expect(result.screen.errmsg).toBe(messages.userIdNotFound);
  });

  it("takes the user id from the list selection", () => {
    const result = handleUserDelete(
      {
        aid: AidKey.enter,
        screen: emptyUserDeleteScreen(),
        commarea: {
          ...signedOn(),
          pgmContext: PgmContext.enter,
          userSelection: { ...emptyCommarea().userSelection, usrSelected: "USER0004" },
        },
      },
      file,
      { persist: false },
    );

    expect(result.screen.usridin).toBe("USER0004");
    expect(result.screen.fname).toBe("AVERARDO");
  });
});
