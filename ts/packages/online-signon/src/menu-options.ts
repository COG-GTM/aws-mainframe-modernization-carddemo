/**
 * `COMEN02Y` — CARDDEMO-MAIN-MENU-OPTIONS and `COADM02Y` —
 * CARDDEMO-ADMIN-MENU-OPTIONS: the option tables the two menu programs index
 * with the option the user typed.
 */

import { UserType, type UserTypeCode } from "./commarea.js";
import { toFixed } from "./screen.js";

/** `CDEMO-MENU-OPT-NAME` / `CDEMO-ADMIN-OPT-NAME`, `PIC X(35)`. */
export const optionNameLength = 35;

/** `WS-MENU-OPT-TXT` / `WS-ADMIN-OPT-TXT`, `PIC X(40)`. */
export const optionTextLength = 40;

export interface MenuOption {
  readonly num: number;
  readonly name: string;
  readonly pgmName: string;
  /** `CDEMO-MENU-OPT-USRTYPE`; the admin table has no user type column. */
  readonly usrType?: UserTypeCode;
}

/** `COMEN02Y`, used by `COMEN01C`. */
export const mainMenuOptions: readonly MenuOption[] = [
  { num: 1, name: "Account View", pgmName: "COACTVWC", usrType: UserType.user },
  { num: 2, name: "Account Update", pgmName: "COACTUPC", usrType: UserType.user },
  { num: 3, name: "Credit Card List", pgmName: "COCRDLIC", usrType: UserType.user },
  { num: 4, name: "Credit Card View", pgmName: "COCRDSLC", usrType: UserType.user },
  { num: 5, name: "Credit Card Update", pgmName: "COCRDUPC", usrType: UserType.user },
  { num: 6, name: "Transaction List", pgmName: "COTRN00C", usrType: UserType.user },
  { num: 7, name: "Transaction View", pgmName: "COTRN01C", usrType: UserType.user },
  { num: 8, name: "Transaction Add", pgmName: "COTRN02C", usrType: UserType.user },
  { num: 9, name: "Transaction Reports", pgmName: "CORPT00C", usrType: UserType.user },
  { num: 10, name: "Bill Payment", pgmName: "COBIL00C", usrType: UserType.user },
  { num: 11, name: "Pending Authorization View", pgmName: "COPAUS0C", usrType: UserType.user },
];

/** `COADM02Y`, used by `COADM01C`. */
export const adminMenuOptions: readonly MenuOption[] = [
  { num: 1, name: "User List (Security)", pgmName: "COUSR00C" },
  { num: 2, name: "User Add (Security)", pgmName: "COUSR01C" },
  { num: 3, name: "User Update (Security)", pgmName: "COUSR02C" },
  { num: 4, name: "User Delete (Security)", pgmName: "COUSR03C" },
  { num: 5, name: "Transaction Type List/Update (Db2)", pgmName: "COTRTLIC" },
  { num: 6, name: "Transaction Type Maintenance (Db2)", pgmName: "COTRTUPC" },
];

/** `BUILD-MENU-OPTIONS`: `nn. ` followed by the `PIC X(35)` option name. */
export function buildOptionText(option: MenuOption): string {
  const num = String(option.num).padStart(2, "0");
  return toFixed(`${num}. ${toFixed(option.name, optionNameLength)}`, optionTextLength);
}

/**
 * `PROCESS-ENTER-KEY`: the option field is right justified into `PIC X(02)`,
 * blanks become zeros, and the result is read as `PIC 9(02)`. Returns zero for
 * a blank field and `undefined` when the digits are not numeric.
 */
export function parseOptionNumber(option: string): number | undefined {
  const field = toFixed(option, 2);
  const lastNonBlank = field.trimEnd().length;
  const typed = field.slice(0, Math.max(lastNonBlank, 1));
  const justified = typed.padStart(2, " ").replaceAll(" ", "0");
  return /^\d{2}$/.test(justified) ? Number(justified) : undefined;
}
