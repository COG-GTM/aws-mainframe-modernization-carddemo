/**
 * A stand in for the Express API while the online programs are being ported.
 *
 * It answers the same screen exchange contract as the real endpoint for the
 * signon, the two menus and the transaction list, so the package runs and is
 * testable on its own. Everything else answers the way the menus do when an
 * option is not available yet.
 */

import {
  CARDDEMO_THANK_YOU,
  CARDDEMO_TITLE_01,
  CARDDEMO_TITLE_02,
  formatDate,
  formatTime,
} from "../terminal/format.js";
import type { ScreenClient, ScreenExchangeRequest, ScreenExchangeResponse, ScreenState } from "./client.js";

interface MenuOption {
  readonly option: number;
  readonly name: string;
  readonly program: string;
}

/** `COMEN02Y`: the main menu options. */
export const MAIN_MENU_OPTIONS: readonly MenuOption[] = [
  { option: 1, name: "Account View", program: "COACTVWC" },
  { option: 2, name: "Account Update", program: "COACTUPC" },
  { option: 3, name: "Credit Card List", program: "COCRDLIC" },
  { option: 4, name: "Credit Card View", program: "COCRDSLC" },
  { option: 5, name: "Credit Card Update", program: "COCRDUPC" },
  { option: 6, name: "Transaction List", program: "COTRN00C" },
  { option: 7, name: "Transaction View", program: "COTRN01C" },
  { option: 8, name: "Transaction Add", program: "COTRN02C" },
  { option: 9, name: "Transaction Reports", program: "CORPT00C" },
  { option: 10, name: "Bill Payment", program: "COBIL00C" },
];

/** `COADM02Y`: the admin menu options that have a map in `app/bms`. */
export const ADMIN_MENU_OPTIONS: readonly MenuOption[] = [
  { option: 1, name: "User List (Security)", program: "COUSR00C" },
  { option: 2, name: "User Add (Security)", program: "COUSR01C" },
  { option: 3, name: "User Update (Security)", program: "COUSR02C" },
  { option: 4, name: "User Delete (Security)", program: "COUSR03C" },
];

interface MockTransaction {
  readonly id: string;
  readonly date: string;
  readonly description: string;
  readonly amount: string;
}

const TRANSACTIONS: readonly MockTransaction[] = Array.from({ length: 24 }, (_, index) => {
  const sequence = index + 1;
  return {
    id: `T${String(sequence).padStart(15, "0")}`,
    date: `2022-08-${String((sequence % 28) + 1).padStart(2, "0")}`,
    description: `Purchase at merchant ${String(sequence).padStart(3, "0")}`,
    amount: (sequence * 13.57).toFixed(2).padStart(12, " "),
  };
});

const TRANSACTIONS_PER_PAGE = 10;

const PROGRAM_TRANSACTIONS: Readonly<Record<string, string>> = {
  COSGN00C: "CC00",
  COMEN01C: "CM00",
  COADM01C: "CA00",
  COTRN00C: "CT00",
  COTRN01C: "CT01",
  COTRN02C: "CT02",
  COUSR00C: "CU00",
  COUSR01C: "CU01",
  COUSR02C: "CU02",
  COUSR03C: "CU03",
};

function header(program: string, now: Date): Record<string, string> {
  return {
    TRNNAME: PROGRAM_TRANSACTIONS[program] ?? "CC00",
    PGMNAME: program,
    TITLE01: CARDDEMO_TITLE_01,
    TITLE02: CARDDEMO_TITLE_02,
    CURDATE: formatDate(now),
    CURTIME: formatTime(now),
  };
}

function isAdmin(state: ScreenState | null): boolean {
  return state !== null && state["userType"] === "A";
}

function signonScreen(message: string, now: Date): ScreenExchangeResponse {
  return {
    program: "COSGN00C",
    screenFields: { ...header("COSGN00C", now), USERID: "", PASSWD: "", ERRMSG: message },
    message,
    state: null,
  };
}

function menuScreen(state: ScreenState | null, message: string, now: Date): ScreenExchangeResponse {
  const admin = isAdmin(state);
  const program = admin ? "COADM01C" : "COMEN01C";
  const options = admin ? ADMIN_MENU_OPTIONS : MAIN_MENU_OPTIONS;
  const screenFields: Record<string, string> = { ...header(program, now), OPTION: "", ERRMSG: message };

  for (const { option, name } of options) {
    screenFields[`OPTN${String(option).padStart(3, "0")}`] = `${String(option).padStart(2, "0")}. ${name}`;
  }

  return { program, screenFields, message, state };
}

function transactionListScreen(
  state: ScreenState | null,
  page: number,
  message: string,
  now: Date,
): ScreenExchangeResponse {
  const pages = Math.ceil(TRANSACTIONS.length / TRANSACTIONS_PER_PAGE);
  const current = Math.min(Math.max(page, 1), pages);
  const rows = TRANSACTIONS.slice((current - 1) * TRANSACTIONS_PER_PAGE, current * TRANSACTIONS_PER_PAGE);
  const screenFields: Record<string, string> = {
    ...header("COTRN00C", now),
    TRNIDIN: "",
    PAGENUM: String(current).padStart(8, " "),
    ERRMSG: message,
  };

  rows.forEach((transaction, index) => {
    const suffix = String(index + 1).padStart(2, "0");
    screenFields[`SEL00${suffix}`] = "";
    screenFields[`TRNID${suffix}`] = transaction.id;
    screenFields[`TDATE${suffix}`] = transaction.date;
    screenFields[`TDESC${suffix}`] = transaction.description;
    screenFields[`TAMT0${suffix}`] = transaction.amount;
  });

  return {
    program: "COTRN00C",
    screenFields,
    message,
    state: { ...(state ?? {}), page: current },
  };
}

function statePage(state: ScreenState | null): number {
  const page = state?.["page"];
  return typeof page === "number" ? page : 1;
}

/** The program flow the mock understands, mirroring the CICS `XCTL` chain. */
export function handleScreenExchange(
  request: ScreenExchangeRequest,
  now: Date = new Date(),
): ScreenExchangeResponse {
  const { program, screenFields, aidKey, state } = request;

  if (aidKey === "CLEAR") {
    return signonScreen("", now);
  }

  if (program === "COSGN00C") {
    if (aidKey === "PF3") {
      return signonScreen(CARDDEMO_THANK_YOU.trim(), now);
    }
    const userId = (screenFields["USERID"] ?? "").trim();
    const password = (screenFields["PASSWD"] ?? "").trim();
    if (userId.length === 0) {
      return signonScreen("Please enter your User ID", now);
    }
    if (password.length === 0) {
      return signonScreen("Please enter your Password", now);
    }
    const userType = userId.toUpperCase().startsWith("ADMIN") ? "A" : "U";
    return menuScreen({ userId: userId.toUpperCase(), userType }, "", now);
  }

  if (program === "COMEN01C" || program === "COADM01C") {
    if (aidKey === "PF3") {
      return signonScreen("", now);
    }
    const options = program === "COADM01C" ? ADMIN_MENU_OPTIONS : MAIN_MENU_OPTIONS;
    const selection = Number((screenFields["OPTION"] ?? "").trim());
    const chosen = options.find((option) => option.option === selection);
    if (chosen === undefined) {
      return menuScreen(state, "Please enter a valid option number", now);
    }
    if (chosen.program !== "COTRN00C") {
      return menuScreen(state, `${chosen.name} is not available in the mock backend`, now);
    }
    return transactionListScreen(state, 1, "", now);
  }

  if (program === "COTRN00C") {
    if (aidKey === "PF3") {
      return menuScreen(state, "", now);
    }
    const page = statePage(state);
    if (aidKey === "PF7") {
      return page === 1
        ? transactionListScreen(state, page, "You are already at the top of the page", now)
        : transactionListScreen(state, page - 1, "", now);
    }
    if (aidKey === "PF8") {
      const pages = Math.ceil(TRANSACTIONS.length / TRANSACTIONS_PER_PAGE);
      return page === pages
        ? transactionListScreen(state, page, "You have reached the bottom of the page", now)
        : transactionListScreen(state, page + 1, "", now);
    }
    return transactionListScreen(state, page, "", now);
  }

  return menuScreen(state, `${program} is not available in the mock backend`, now);
}

/** A {@link ScreenClient} backed by {@link handleScreenExchange}. */
export function createMockScreenClient(now: () => Date = () => new Date()): ScreenClient {
  return {
    exchange(request: ScreenExchangeRequest): Promise<ScreenExchangeResponse> {
      return Promise.resolve(handleScreenExchange(request, now()));
    },
  };
}

/** The screen the terminal starts on before the first exchange. */
export function initialScreen(now: Date = new Date()): ScreenExchangeResponse {
  return signonScreen("", now);
}
