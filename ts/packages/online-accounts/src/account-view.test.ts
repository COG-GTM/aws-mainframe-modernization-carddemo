import { beforeEach, describe, expect, it } from "vitest";

import {
  handleAccountView,
  VIEW_MAP,
  VIEW_MAPSET,
  type AccountViewRequest,
} from "./account-view.js";
import { emptyCommarea, MENU_PROGRAM, type CardDemoCommarea } from "./commarea.js";
import { openAccountFiles, type AccountFiles } from "./data.js";
import {
  accountNotFound,
  customerNotFound,
  viewMessages,
  xrefAccountNotFound,
} from "./messages.js";

const reentry: CardDemoCommarea = { ...emptyCommarea, programContext: "REENTER" };

function request(acctsid: string, overrides: Partial<AccountViewRequest> = {}): AccountViewRequest {
  return {
    aid: "ENTER",
    screen: { acctsid },
    commarea: reentry,
    ...overrides,
  };
}

describe("COACTVWC account view", () => {
  let files: AccountFiles;

  beforeEach(() => {
    files = openAccountFiles();
  });

  it("asks for the account number on a cold start", () => {
    const response = handleAccountView(
      { aid: "ENTER", screen: { acctsid: "" }, commarea: null },
      files,
    );

    expect(response.screen?.infomsg).toBe(viewMessages.promptForInput);
    expect(response.screen?.acctsid).toBe("");
    expect(response.screen?.errmsg).toBe("");
    expect(response.commarea.programContext).toBe("REENTER");
    expect(response.control.nextMapset).toBe(VIEW_MAPSET);
    expect(response.control.nextMap).toBe(VIEW_MAP);
  });

  it("displays an existing account with its customer", () => {
    const response = handleAccountView(request("00000000001"), files);
    const screen = response.screen;

    expect(screen?.errmsg).toBe("");
    expect(screen?.acsttus).toBe("Y");
    expect(screen?.acurbal).toBe("+        194.00");
    expect(screen?.acrdlim).toBe("+      2,020.00");
    expect(screen?.adtopen).toBe("2014-11-20");
    expect(screen?.acstnum).toBe("000000001");
    expect(screen?.acstssn).toBe("020-97-3888");
    expect(screen?.acsfnam).toBe("Immanuel");
    expect(screen?.acslnam).toBe("Kessler");
    expect(screen?.acsstte).toBe("NC");
    expect(screen?.acsphn1).toBe("(908)119-8310");
    expect(response.commarea.acctId).toBe(1);
    expect(response.commarea.custId).toBe(1);
  });

  it("reports an account that is not in the cross reference", () => {
    const response = handleAccountView(request("99999999999"), files);

    expect(response.screen?.errmsg).toBe(xrefAccountNotFound("99999999999"));
    expect(response.screen?.acsttus).toBe("");
  });

  it("reports a missing account master record", () => {
    const xref = files.cardXrefs.toArray()[0];
    expect(xref).toBeDefined();
    files.accounts.delete(String(xref?.xrefAcctId).padStart(11, "0"));

    const response = handleAccountView(
      request(String(xref?.xrefAcctId).padStart(11, "0")),
      files,
    );

    expect(response.screen?.errmsg).toBe(
      accountNotFound(String(xref?.xrefAcctId).padStart(11, "0")),
    );
  });

  it("reports a missing customer master record", () => {
    files.customers.delete("000000001");

    const response = handleAccountView(request("00000000001"), files);

    expect(response.screen?.errmsg).toBe(customerNotFound("000000001"));
  });

  it("treats a blank account number as no input", () => {
    const response = handleAccountView(request("           "), files);

    expect(response.screen?.errmsg).toBe(viewMessages.noSearchCriteria);
    expect(response.screen?.acctsid).toBe("*");
  });

  it("rejects an account number that is not eleven digits", () => {
    const response = handleAccountView(request("1234"), files);

    expect(response.screen?.errmsg).toBe(viewMessages.acctNotElevenDigits);
  });

  it("rejects a zero account number", () => {
    const response = handleAccountView(request("00000000000"), files);

    expect(response.screen?.errmsg).toBe(viewMessages.acctNotElevenDigits);
  });

  it("returns to the caller on PF03", () => {
    const response = handleAccountView(
      request("00000000001", {
        aid: "PFK03",
        commarea: { ...reentry, fromProgram: "COMEN01C", fromTranid: "CM00" },
      }),
      files,
    );

    expect(response.screen).toBeNull();
    expect(response.control.transferControl).toBe(true);
    expect(response.control.nextProgram).toBe(MENU_PROGRAM);
    expect(response.commarea.toTranid).toBe("CM00");
  });

  it("treats an unsupported key as Enter", () => {
    const response = handleAccountView(request("00000000001", { aid: "PFK07" }), files);

    expect(response.control.transferControl).toBe(false);
    expect(response.screen?.acsttus).toBe("Y");
  });
});
