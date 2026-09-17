import { describe, expect, it } from "vitest";

import { createCommarea, PgmContext, UserType } from "./commarea.js";
import { inquiredProgram, mainMenu } from "./comen01c.js";
import { mainMenuOptions } from "./menu-options.js";
import type { MenuScreenFields } from "./menu-screen.js";
import { invalidOptionMessage } from "./menu-screen.js";
import { mainMenuMap, mainMenuProgram, mainMenuTransaction, signonProgram } from "./programs.js";
import { AidKey, CommonMessage, type HandlerResponse } from "./screen.js";

const now = new Date(2024, 4, 17, 9, 8, 7);
const signedOn = createCommarea({
  userId: "USER0001",
  userType: UserType.user,
  pgmContext: PgmContext.reenter,
});

const press = (
  option: string,
  aid = AidKey.enter,
  installed = true,
): HandlerResponse<MenuScreenFields> =>
  mainMenu(
    { aid, commarea: signedOn, fields: { option } },
    { now, isProgramInstalled: () => installed },
  );

const message = (response: HandlerResponse<MenuScreenFields>): string => {
  if (response.kind !== "map") {
    throw new Error(`expected a map response, got ${response.kind}`);
  }
  return response.fields.errmsg.trimEnd();
};

describe("COMEN01C", () => {
  it("returns to signon when there is no commarea", () => {
    expect(mainMenu({ aid: AidKey.enter })).toEqual({ kind: "transfer", program: signonProgram });
  });

  it("sends the menu on first entry and flips the context to re-enter", () => {
    const response = mainMenu({ aid: AidKey.enter, commarea: createCommarea() }, { now });
    expect(response).toMatchObject({
      kind: "map",
      map: mainMenuMap,
      transactionId: mainMenuTransaction,
      commarea: { pgmContext: PgmContext.reenter },
    });
    if (response.kind !== "map") {
      throw new Error("expected a map response");
    }
    expect(response.fields.optn001).toBe(`01. Account View${" ".repeat(24)}`);
    expect(response.fields.optn011.trimEnd()).toBe("11. Pending Authorization View");
    expect(response.fields.optn012).toBe("");
    expect(response.fields.errmsg.trim()).toBe("");
  });

  it.each(mainMenuOptions)("routes option $num to $pgmName", (option) => {
    expect(press(String(option.num))).toEqual({
      kind: "transfer",
      program: option.pgmName,
      commarea: {
        ...signedOn,
        fromTranid: mainMenuTransaction,
        fromProgram: mainMenuProgram,
        pgmContext: PgmContext.enter,
      },
    });
  });

  it("reports the inquired program when it is not installed", () => {
    const response = press("11", AidKey.enter, false);
    expect(message(response)).toBe("This option Pending Authorization View is not installed...");
    expect(response).toMatchObject({ kind: "map", fields: { errmsgColor: "RED", option: "11" } });
    expect(inquiredProgram).toBe("COPAUS0C");
  });

  it.each(["", " ", "0", "12", "99", "AB"])("rejects option %j", (option) => {
    const response = press(option);
    expect(message(response)).toBe(invalidOptionMessage);
  });

  it("returns to signon on PF3", () => {
    expect(press("1", AidKey.pf3)).toEqual({ kind: "transfer", program: signonProgram });
  });

  it("reports any other key as invalid", () => {
    expect(message(press("1", AidKey.pf7))).toBe(CommonMessage.invalidKey);
  });
});
