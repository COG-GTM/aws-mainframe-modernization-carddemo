import { mkdtempSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { FileStatus } from "@carddemo/vsam";
import { beforeEach, describe, expect, it } from "vitest";

import { PgmContext, emptyCommarea, programs, type CardDemoCommarea } from "./commarea.js";
import { emptyUserUpdateScreen, handleUserUpdate, type UserUpdateScreen } from "./cousr02.js";
import { messages, userUpdatedMessage } from "./messages.js";
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

describe("COUSR02C update user", () => {
  let file: ReturnType<typeof openUsrsec>;

  beforeEach(() => {
    file = openUsrsec();
  });

  const update = (aid: AidKeyCode, screen: UserUpdateScreen, commarea = signedOn()) =>
    handleUserUpdate({ aid, screen, commarea }, file, { persist: false });

  it("loads the selected user on first entry and asks for PF5", () => {
    const result = handleUserUpdate(
      {
        aid: AidKey.enter,
        screen: emptyUserUpdateScreen(),
        commarea: {
          ...signedOn(),
          pgmContext: PgmContext.enter,
          userSelection: { ...emptyCommarea().userSelection, usrSelected: "USER0001" },
        },
      },
      file,
      { persist: false },
    );

    expect(result.screen.usridin).toBe("USER0001");
    expect(result.screen.fname).toBe("LAWRENCE");
    expect(result.screen.lname).toBe("THOMAS");
    expect(result.screen.errmsg).toBe(messages.pressPf5ToSave);
    expect(result.screen.errmsgColor).toBe(MessageColor.neutral);
  });

  it("saves the modified user on PF5", () => {
    const loaded = update(AidKey.enter, { ...emptyUserUpdateScreen(), usridin: "USER0002" });
    const saved = update(AidKey.pf5, { ...loaded.screen, lname: "SINGH" });

    expect(saved.screen.errmsg).toBe(userUpdatedMessage("USER0002"));
    expect(saved.screen.errmsgColor).toBe(MessageColor.green);
    expect(file.read("USER0002").record?.secUsrLname).toBe("SINGH");
  });

  it("keeps the record when nothing changed", () => {
    const loaded = update(AidKey.enter, { ...emptyUserUpdateScreen(), usridin: "USER0002" });
    const saved = update(AidKey.pf5, loaded.screen);

    expect(saved.screen.errmsg).toBe(messages.modifyToUpdate);
    expect(file.read("USER0002").record?.secUsrLname).toBe("KUMAR");
  });

  it("reports a missing user id (FILE STATUS 23)", () => {
    const result = update(AidKey.enter, { ...emptyUserUpdateScreen(), usridin: "NOSUCH01" });

    expect(result.screen.errmsg).toBe(messages.userIdNotFound);
    expect(result.screen.fname).toBe("");
  });

  it("requires a user id", () => {
    expect(update(AidKey.enter, emptyUserUpdateScreen()).screen.errmsg).toBe(
      messages.userIdEmpty,
    );
  });

  it("clears on PF4 and leaves on PF12", () => {
    const loaded = update(AidKey.enter, { ...emptyUserUpdateScreen(), usridin: "USER0002" });

    expect(update(AidKey.pf4, loaded.screen).screen.usridin).toBe("");

    const left = update(AidKey.pf12, loaded.screen);
    expect(left.transfer).toBe(true);
    expect(left.nextProgram).toBe(programs.adminMenu);
  });
});
