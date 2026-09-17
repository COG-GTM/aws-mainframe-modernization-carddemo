/**
 * Pieces shared by the two menu maps, `COMEN1A` and `COADM1A`: they have the
 * same twelve option lines, the same `OPTION` input field and the same error
 * line, and their programs build them the same way.
 */

import { buildOptionText, type MenuOption } from "./menu-options.js";
import { toFixed, type ScreenHeaderFields } from "./screen.js";

/** The unprotected field of both menu maps, `PIC X(02)`. */
export interface MenuInputFields {
  readonly option: string;
}

/** `ERRMSGC`; `DFHRED` and `DFHGREEN` are the colours the programs set. */
export type MessageColor = "DEFAULT" | "RED" | "GREEN";

export interface MenuOptionLines {
  readonly optn001: string;
  readonly optn002: string;
  readonly optn003: string;
  readonly optn004: string;
  readonly optn005: string;
  readonly optn006: string;
  readonly optn007: string;
  readonly optn008: string;
  readonly optn009: string;
  readonly optn010: string;
  readonly optn011: string;
  readonly optn012: string;
}

export interface MenuScreenFields extends ScreenHeaderFields, MenuOptionLines, MenuInputFields {
  readonly errmsg: string;
  readonly errmsgColor: MessageColor;
}

/** `BUILD-MENU-OPTIONS`: one `PIC X(40)` line per option, blanks after that. */
export function buildOptionLines(options: readonly MenuOption[]): MenuOptionLines {
  const line = (index: number): string => {
    const option = options[index];
    return option === undefined ? "" : buildOptionText(option);
  };
  return {
    optn001: line(0),
    optn002: line(1),
    optn003: line(2),
    optn004: line(3),
    optn005: line(4),
    optn006: line(5),
    optn007: line(6),
    optn008: line(7),
    optn009: line(8),
    optn010: line(9),
    optn011: line(10),
    optn012: line(11),
  };
}

/** COBOL `STRING ... DELIMITED BY <delimiter>`. */
export function delimitedBy(value: string, delimiter: string): string {
  const end = value.indexOf(delimiter);
  return end < 0 ? value : value.slice(0, end);
}

/** `MOVE WS-OPTION TO OPTIONO`: the parsed option echoed back as `PIC 9(02)`. */
export function echoOption(option: number | undefined, typed: string): string {
  return option === undefined ? toFixed(typed, 2) : String(option).padStart(2, "0");
}

export const invalidOptionMessage = "Please enter a valid option number...";
