import type { AccountRecord, CustomerRecord } from "@carddemo/domain";
import { beforeEach, describe, expect, it } from "vitest";

import {
  emptyAccountUpdateScreen,
  handleAccountUpdate,
  initialAccountUpdateState,
  screenFromRecords,
  type AccountUpdateResponse,
  type AccountUpdateScreenInput,
  type AccountUpdateState,
} from "./account-update.js";
import { emptyCommarea, MENU_PROGRAM, type AidKey, type CardDemoCommarea } from "./commarea.js";
import { openAccountFiles, type AccountFiles } from "./data.js";
import { updateMessages, xrefAccountNotFound } from "./messages.js";

const reentry: CardDemoCommarea = { ...emptyCommarea, programContext: "REENTER" };

/** The date of birth edit rejects future dates, so pin the clock. */
const systemDate = new Date("2024-01-15T00:00:00Z");

function account(files: AccountFiles): AccountRecord {
  const record = files.accounts.read("00000000001").record;
  if (record === undefined) {
    throw new Error("account 1 missing from acctdata.txt");
  }
  return record;
}

function customer(files: AccountFiles): CustomerRecord {
  const record = files.customers.read("000000001").record;
  if (record === undefined) {
    throw new Error("customer 1 missing from custdata.txt");
  }
  return record;
}

/** Fetch account 1 and return the response that shows its details. */
function fetchDetails(files: AccountFiles): AccountUpdateResponse {
  return handleAccountUpdate(
    {
      aid: "ENTER",
      screen: { ...emptyAccountUpdateScreen, acctsid: "00000000001" },
      commarea: reentry,
      state: initialAccountUpdateState,
    },
    files,
    { systemDate },
  );
}

/**
 * The fetched details with the values the edits reject replaced: the sample
 * customer has a FICO score of 274, a zip that does not belong to its state
 * and a second area code outside the North American numbering plan.
 */
function editableScreen(response: AccountUpdateResponse): AccountUpdateScreenInput {
  const screen = response.screen;
  if (screen === null) {
    throw new Error("expected the map to be sent");
  }
  return { ...screen, acstfco: "700", acszipc: "27601", acsph2a: "336" };
}

function submit(
  files: AccountFiles,
  previous: AccountUpdateResponse,
  screen: AccountUpdateScreenInput,
  aid: AidKey = "ENTER",
): AccountUpdateResponse {
  return handleAccountUpdate(
    { aid, screen, commarea: previous.commarea, state: previous.state },
    files,
    { systemDate },
  );
}

/** The first return message produced by changing one field. */
function errorFor(
  files: AccountFiles,
  overrides: Partial<AccountUpdateScreenInput>,
): string {
  const fetched = fetchDetails(files);
  const screen = { ...editableScreen(fetched), ...overrides };
  return submit(files, fetched, screen).screen?.errmsg ?? "";
}

describe("COACTUPC account update", () => {
  let files: AccountFiles;

  beforeEach(() => {
    files = openAccountFiles();
  });

  describe("search key", () => {
    it("asks for the search key on a cold start", () => {
      const response = handleAccountUpdate(
        {
          aid: "ENTER",
          screen: emptyAccountUpdateScreen,
          commarea: null,
          state: initialAccountUpdateState,
        },
        files,
        { systemDate },
      );

      expect(response.screen?.infomsg).toBe(updateMessages.promptForSearchKeys);
      expect(response.state.action).toBe("DETAILS-NOT-FETCHED");
    });

    it("rejects a blank account number", () => {
      const response = handleAccountUpdate(
        {
          aid: "ENTER",
          screen: emptyAccountUpdateScreen,
          commarea: reentry,
          state: initialAccountUpdateState,
        },
        files,
        { systemDate },
      );

      expect(response.screen?.errmsg).toBe(updateMessages.noSearchCriteria);
    });

    it("rejects an account number that is not eleven non zero digits", () => {
      const response = handleAccountUpdate(
        {
          aid: "ENTER",
          screen: { ...emptyAccountUpdateScreen, acctsid: "00000000000" },
          commarea: reentry,
          state: initialAccountUpdateState,
        },
        files,
        { systemDate },
      );

      expect(response.screen?.errmsg).toBe(updateMessages.acctNotElevenDigitsNonZero);
    });

    it("reports an account that is not in the cross reference", () => {
      const response = handleAccountUpdate(
        {
          aid: "ENTER",
          screen: { ...emptyAccountUpdateScreen, acctsid: "99999999999" },
          commarea: reentry,
          state: initialAccountUpdateState,
        },
        files,
        { systemDate },
      );

      expect(response.screen?.errmsg).toBe(xrefAccountNotFound("99999999999"));
      expect(response.state.action).toBe("DETAILS-NOT-FETCHED");
    });

    it("shows the details of the account it fetched", () => {
      const response = fetchDetails(files);

      expect(response.state.action).toBe("SHOW-DETAILS");
      expect(response.screen?.infomsg).toBe(updateMessages.promptForChanges);
      expect(response.screen).toMatchObject(
        screenFromRecords({ account: account(files), customer: customer(files) }),
      );
    });

    it("rereads the account on PF12", () => {
      const fetched = fetchDetails(files);
      const response = submit(files, fetched, editableScreen(fetched), "PFK12");

      expect(response.state.action).toBe("SHOW-DETAILS");
      expect(response.screen?.acsttus).toBe("Y");
    });

    it("returns to the caller on PF03", () => {
      const response = handleAccountUpdate(
        {
          aid: "PFK03",
          screen: emptyAccountUpdateScreen,
          commarea: reentry,
          state: initialAccountUpdateState,
        },
        files,
        { systemDate },
      );

      expect(response.control.transferControl).toBe(true);
      expect(response.control.nextProgram).toBe(MENU_PROGRAM);
    });
  });

  describe("field edits", () => {
    it("detects that nothing changed", () => {
      const fetched = fetchDetails(files);
      const response = submit(files, fetched, fetched.screen as AccountUpdateScreenInput);

      expect(response.screen?.errmsg).toBe(updateMessages.noChangesDetected);
      expect(response.state.action).toBe("SHOW-DETAILS");
    });

    it("rejects an account status that is not Y or N", () => {
      expect(errorFor(files, { acsttus: "X" })).toBe("Account Status must be Y or N.");
    });

    it("requires an account status", () => {
      expect(errorFor(files, { acsttus: " " })).toBe("Account Status must be supplied.");
    });

    it("requires a credit limit", () => {
      expect(errorFor(files, { acrdlim: "" })).toBe("Credit Limit must be supplied.");
    });

    it("rejects a credit limit that is not a number", () => {
      expect(errorFor(files, { acrdlim: "ten" })).toBe("Credit Limit is not valid");
    });

    it("rejects an invalid open date", () => {
      expect(errorFor(files, { opnmon: "13" })).toContain("Open Date");
    });

    it("rejects an invalid expiry date", () => {
      expect(errorFor(files, { expday: "32" })).toContain("Expiry Date");
    });

    it("rejects an invalid reissue date", () => {
      expect(errorFor(files, { risyear: "abcd" })).toContain("Reissue Date");
    });

    it("rejects an invalid current balance", () => {
      expect(errorFor(files, { acurbal: "12.3.4" })).toBe("Current Balance is not valid");
    });

    it("rejects a reserved SSN area number", () => {
      expect(errorFor(files, { actssn1: "666" })).toBe(
        "SSN: First 3 chars: should not be 000, 666, or between 900 and 999",
      );
    });

    it("rejects a non numeric SSN group number", () => {
      expect(errorFor(files, { actssn2: "ab" })).toBe("SSN 4th & 5th chars must be all numeric.");
    });

    it("rejects a future date of birth", () => {
      expect(errorFor(files, { dobyear: "2030" })).toContain("Date of Birth");
    });

    it("requires a FICO score", () => {
      expect(errorFor(files, { acstfco: "" })).toBe("FICO Score must be supplied.");
    });

    it("rejects a FICO score outside 300 to 850", () => {
      expect(errorFor(files, { acstfco: "900" })).toBe(
        "FICO Score: should be between 300 and 850",
      );
    });

    it("requires a first name", () => {
      expect(errorFor(files, { acsfnam: "" })).toBe("First Name must be supplied.");
    });

    it("rejects a first name that is not alphabetic", () => {
      expect(errorFor(files, { acsfnam: "Imm4nuel" })).toBe("First Name can have alphabets only.");
    });

    it("allows a blank middle name", () => {
      expect(errorFor(files, { acsmnam: "" })).toBe("");
    });

    it("requires a last name", () => {
      expect(errorFor(files, { acslnam: "" })).toBe("Last Name must be supplied.");
    });

    it("requires the first address line", () => {
      expect(errorFor(files, { acsadl1: "" })).toBe("Address Line 1 must be supplied.");
    });

    it("rejects an unknown state code", () => {
      expect(errorFor(files, { acsstte: "XX" })).toBe("State: is not a valid state code");
    });

    it("requires a numeric zip", () => {
      expect(errorFor(files, { acszipc: "abcde" })).toBe("Zip must be all numeric.");
    });

    it("rejects a zip that does not belong to the state", () => {
      expect(errorFor(files, { acszipc: "99950" })).toBe("Invalid zip code for state");
    });

    it("requires a city", () => {
      expect(errorFor(files, { acscity: "" })).toBe("City must be supplied.");
    });

    it("requires a country", () => {
      expect(errorFor(files, { acsctry: "" })).toBe("Country must be supplied.");
    });

    it("rejects an area code that is not in the North American plan", () => {
      expect(errorFor(files, { acsph1a: "999" })).toBe(
        "Phone Number 1: Not valid North America general purpose area code",
      );
    });

    it("requires the prefix when part of a phone number is supplied", () => {
      expect(errorFor(files, { acsph2b: "" })).toBe(
        "Phone Number 2: Prefix code must be supplied.",
      );
    });

    it("rejects a line number that is not four digits", () => {
      expect(errorFor(files, { acsph1c: "12" })).toBe(
        "Phone Number 1: Line number code must be A 4 digit number.",
      );
    });

    it("allows a phone number that is entirely blank", () => {
      expect(errorFor(files, { acsph2a: "", acsph2b: "", acsph2c: "" })).toBe("");
    });

    it("requires an EFT account id", () => {
      expect(errorFor(files, { acseftc: "" })).toBe("EFT Account Id must be supplied.");
    });

    it("rejects a primary card holder flag that is not Y or N", () => {
      expect(errorFor(files, { acspflg: "Z" })).toBe("Primary Card Holder must be Y or N.");
    });
  });

  describe("confirmation and write", () => {
    it("asks for confirmation once the changes validate", () => {
      const fetched = fetchDetails(files);
      const response = submit(files, fetched, editableScreen(fetched));

      expect(response.state.action).toBe("CHANGES-OK-NOT-CONFIRMED");
      expect(response.screen?.infomsg).toBe(updateMessages.promptForConfirmation);
      expect(response.screen?.errmsg).toBe("");
    });

    it("does not write anything before PF5", () => {
      const fetched = fetchDetails(files);
      submit(files, fetched, editableScreen(fetched));

      expect(customer(files).custFicoCreditScore).toBe(274);
    });

    it("rewrites the account and the customer on PF5", () => {
      const fetched = fetchDetails(files);
      const screen = {
        ...editableScreen(fetched),
        acsttus: "N",
        acrdlim: "+      3,500.55",
        acsfnam: "Imogen",
      };
      const validated = submit(files, fetched, screen);
      const committed = submit(files, validated, screen, "PFK05");

      expect(committed.state.action).toBe("CHANGES-OKAYED-AND-DONE");
      expect(committed.screen?.infomsg).toBe(updateMessages.updateSuccess);

      const updatedAccount = account(files);
      expect(updatedAccount.acctActiveStatus).toBe("N");
      expect(updatedAccount.acctCreditLimit).toBe(3500.55);
      expect(updatedAccount.acctCurrBal).toBe(194);
      expect(updatedAccount.acctOpenDate).toBe("2014-11-20");

      const updatedCustomer = customer(files);
      expect(updatedCustomer.custFirstName).toBe("Imogen");
      expect(updatedCustomer.custFicoCreditScore).toBe(700);
      expect(updatedCustomer.custPhoneNum1).toBe("(908)119-8310");
      expect(updatedCustomer.custSsn).toBe(20973888);
    });

    it("starts over on the interaction after a committed update", () => {
      const fetched = fetchDetails(files);
      const screen = { ...editableScreen(fetched), acsfnam: "Imogen" };
      const validated = submit(files, fetched, screen);
      const committed = submit(files, validated, screen, "PFK05");
      const afterwards = submit(files, committed, screen);

      expect(afterwards.state.action).toBe("DETAILS-NOT-FETCHED");
      expect(afterwards.screen?.infomsg).toBe(updateMessages.promptForSearchKeys);
    });

    it("rejects the update when someone else changed the record", () => {
      const fetched = fetchDetails(files);
      const screen = { ...editableScreen(fetched), acsfnam: "Imogen" };
      const validated = submit(files, fetched, screen);

      files.accounts.rewrite({ ...account(files), acctCurrBal: 999.99 });

      const rejected = submit(files, validated, screen, "PFK05");

      expect(rejected.screen?.errmsg).toBe(updateMessages.recordChangedByAnotherUser);
      expect(rejected.state.action).toBe("SHOW-DETAILS");
      expect(customer(files).custFirstName).toBe("Immanuel");
    });

    it("reports a failed update and leaves the account as it was", () => {
      const fetched = fetchDetails(files);
      const screen = { ...editableScreen(fetched), acsfnam: "Imogen" };
      const validated = submit(files, fetched, screen);

      // The customer rewrite fails once the record is no longer in the file.
      files.customers.delete("000000001");

      const failed = submit(files, validated, screen, "PFK05");

      expect(failed.screen?.errmsg).toBe(updateMessages.couldNotLockCustomer);
      expect(failed.state.action).toBe("CHANGES-OKAYED-LOCK-ERROR");
      expect(account(files).acctCurrBal).toBe(194);
    });
  });

  it("keeps the search key state machine consistent across a full round trip", () => {
    const states: AccountUpdateState["action"][] = [];
    const fetched = fetchDetails(files);
    states.push(fetched.state.action);

    const screen = { ...editableScreen(fetched), acsttus: "N" };
    const validated = submit(files, fetched, screen);
    states.push(validated.state.action);

    const committed = submit(files, validated, screen, "PFK05");
    states.push(committed.state.action);

    expect(states).toEqual(["SHOW-DETAILS", "CHANGES-OK-NOT-CONFIRMED", "CHANGES-OKAYED-AND-DONE"]);
  });
});
