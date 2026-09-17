import { describe, expect, it } from "vitest";

import { createCommarea, PgmContext, UserType } from "./commarea.js";
import { SignonMessage, signon, type SignonOptions, type SignonScreenFields } from "./cosgn00c.js";
import { adminMenuProgram, mainMenuProgram, signonMap, signonTransaction } from "./programs.js";
import {
  AidKey,
  CommonMessage,
  messageLength,
  toFixed,
  type HandlerResponse,
} from "./screen.js";
import { CicsResp, UserSecurityFile } from "./user-security.js";

const options: SignonOptions = {
  users: UserSecurityFile.open(),
  now: new Date(2024, 4, 17, 9, 8, 7),
  applid: "CICSAPP1",
  sysid: "CICS",
};

/** A signed-on-screen round trip: the commarea exists once the map came back. */
const enter = (userid: string, passwd: string): HandlerResponse<SignonScreenFields> =>
  signon({ aid: AidKey.enter, commarea: createCommarea(), fields: { userid, passwd } }, options);

const message = (response: HandlerResponse<SignonScreenFields>): string => {
  if (response.kind !== "map") {
    throw new Error(`expected a map response, got ${response.kind}`);
  }
  return response.fields.errmsg.trimEnd();
};

describe("COSGN00C", () => {
  it("sends an empty signon screen when there is no commarea", () => {
    const response = signon({ aid: AidKey.enter }, options);
    expect(response).toMatchObject({
      kind: "map",
      map: signonMap,
      transactionId: signonTransaction,
      cursor: "userid",
    });
    if (response.kind !== "map") {
      throw new Error("expected a map response");
    }
    expect(response.fields.userid).toBe("");
    expect(response.fields.curdate).toBe("05/17/24");
    expect(response.fields.curtime).toBe("09:08:07");
    expect(response.fields.applid).toBe("CICSAPP1");
  });

  it("signs a regular user on to the main menu", () => {
    expect(enter("user0001", "password")).toEqual({
      kind: "transfer",
      program: mainMenuProgram,
      commarea: expect.objectContaining({
        fromTranid: signonTransaction,
        fromProgram: "COSGN00C",
        userId: "USER0001",
        userType: UserType.user,
        pgmContext: PgmContext.enter,
      }) as unknown,
    });
  });

  it("signs an admin user on to the admin menu", () => {
    expect(enter("ADMIN001", "PASSWORD")).toMatchObject({
      kind: "transfer",
      program: adminMenuProgram,
      commarea: { userType: UserType.admin, userId: "ADMIN001" },
    });
  });

  it("rejects a wrong password and keeps the cursor on the password field", () => {
    const response = enter("USER0001", "BADPASS");
    expect(message(response)).toBe(SignonMessage.wrongPassword);
    expect(response).toMatchObject({ kind: "map", cursor: "passwd" });
  });

  it("rejects an unknown user", () => {
    const response = enter("NOBODY", "PASSWORD");
    expect(message(response)).toBe(SignonMessage.userNotFound);
    expect(response).toMatchObject({ kind: "map", cursor: "userid" });
  });

  it("asks for the user id and then the password", () => {
    expect(message(enter("", ""))).toBe(SignonMessage.missingUserId);
    expect(message(enter("USER0001", "  "))).toBe(SignonMessage.missingPassword);
  });

  it("sends the thank you text on PF3", () => {
    const response = signon({ aid: AidKey.pf3, commarea: createCommarea(), fields: {} }, options);
    expect(response).toEqual({
      kind: "text",
      text: toFixed(CommonMessage.thankYou, messageLength),
    });
  });

  it("reports any other key as invalid", () => {
    const response = signon({ aid: AidKey.pf5, commarea: createCommarea(), fields: {} }, options);
    expect(message(response)).toBe(CommonMessage.invalidKey);
  });

  it("reports a file error as unable to verify", () => {
    const response = signon(
      { aid: AidKey.enter, commarea: createCommarea(), fields: { userid: "USER0001", passwd: "X" } },
      { ...options, users: { read: () => ({ resp: CicsResp.ioError }) } },
    );
    expect(message(response)).toBe(SignonMessage.unableToVerify);
  });
});
