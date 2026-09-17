import { describe, expect, it } from "vitest";

import { adminMenu, notInstalledMessage } from "./coadm01c.js";
import { createCommarea, PgmContext, UserType } from "./commarea.js";
import { adminMenuOptions } from "./menu-options.js";
import { invalidOptionMessage, type MenuScreenFields } from "./menu-screen.js";
import {
  adminMenuMap,
  adminMenuProgram,
  adminMenuTransaction,
  signonProgram,
} from "./programs.js";
import { AidKey, CommonMessage, type HandlerResponse } from "./screen.js";

const now = new Date(2024, 4, 17, 9, 8, 7);
const signedOn = createCommarea({
  userId: "ADMIN001",
  userType: UserType.admin,
  pgmContext: PgmContext.reenter,
});

const press = (
  option: string,
  aid = AidKey.enter,
  installed = true,
): HandlerResponse<MenuScreenFields> =>
  adminMenu(
    { aid, commarea: signedOn, fields: { option } },
    { now, isProgramInstalled: () => installed },
  );

const message = (response: HandlerResponse<MenuScreenFields>): string => {
  if (response.kind !== "map") {
    throw new Error(`expected a map response, got ${response.kind}`);
  }
  return response.fields.errmsg.trimEnd();
};

describe("COADM01C", () => {
  it("returns to signon when there is no commarea", () => {
    expect(adminMenu({ aid: AidKey.enter })).toEqual({ kind: "transfer", program: signonProgram });
  });

  it("sends the admin menu on first entry", () => {
    const response = adminMenu(
      { aid: AidKey.enter, commarea: createCommarea({ userType: UserType.admin }) },
      { now },
    );
    expect(response).toMatchObject({
      kind: "map",
      map: adminMenuMap,
      transactionId: adminMenuTransaction,
      commarea: { pgmContext: PgmContext.reenter },
    });
    if (response.kind !== "map") {
      throw new Error("expected a map response");
    }
    expect(response.fields.optn001.trimEnd()).toBe("01. User List (Security)");
    expect(response.fields.optn006.trimEnd()).toBe("06. Transaction Type Maintenance (Db2)");
    expect(response.fields.optn007).toBe("");
  });

  it.each(adminMenuOptions)("routes option $num to $pgmName", (option) => {
    expect(press(String(option.num))).toEqual({
      kind: "transfer",
      program: option.pgmName,
      commarea: {
        ...signedOn,
        fromTranid: adminMenuTransaction,
        fromProgram: adminMenuProgram,
        pgmContext: PgmContext.enter,
      },
    });
  });

  it("reports an option whose program is not installed", () => {
    const response = press("1", AidKey.enter, false);
    expect(message(response)).toBe(notInstalledMessage);
    expect(response).toMatchObject({ kind: "map", fields: { errmsgColor: "GREEN" } });
  });

  it.each(["", "0", "7", "XY"])("rejects option %j", (option) => {
    expect(message(press(option))).toBe(invalidOptionMessage);
  });

  it("returns to signon on PF3", () => {
    expect(press("1", AidKey.pf3)).toEqual({ kind: "transfer", program: signonProgram });
  });

  it("reports any other key as invalid", () => {
    expect(message(press("1", AidKey.pf12))).toBe(CommonMessage.invalidKey);
  });
});
