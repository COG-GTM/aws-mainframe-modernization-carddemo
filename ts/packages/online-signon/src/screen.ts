/**
 * The pieces every BMS screen in the application shares: the AID keys from
 * `DFHAID`, the titles from `COTTL01Y`, the messages from `CSMSG01Y`, the
 * header fields `POPULATE-HEADER-INFO` fills in from `CSDAT01Y`, and the
 * request/response shapes the CICS programs are modelled as.
 */

import type { CardDemoCommarea } from "./commarea.js";

/** `DFHAID` attention identifiers. */
export const AidKey = {
  enter: "ENTER",
  clear: "CLEAR",
  pa1: "PA1",
  pa2: "PA2",
  pa3: "PA3",
  pf1: "PF1",
  pf2: "PF2",
  pf3: "PF3",
  pf4: "PF4",
  pf5: "PF5",
  pf6: "PF6",
  pf7: "PF7",
  pf8: "PF8",
  pf9: "PF9",
  pf10: "PF10",
  pf11: "PF11",
  pf12: "PF12",
} as const;

export type AidKeyCode = (typeof AidKey)[keyof typeof AidKey];

const aidKeyCodes = new Set<string>(Object.values(AidKey));

export function isAidKey(value: string): value is AidKeyCode {
  return aidKeyCodes.has(value);
}

/** `COTTL01Y` — CCDA-SCREEN-TITLE. */
export const ScreenTitle = {
  title01: "      AWS Mainframe Modernization       ",
  title02: "              CardDemo                  ",
} as const;

/** `CSMSG01Y` — CCDA-COMMON-MESSAGES, without the `PIC X(50)` padding. */
export const CommonMessage = {
  thankYou: "Thank you for using CardDemo application...",
  invalidKey: "Invalid key pressed. Please see below...",
} as const;

/** `WS-MESSAGE`, `PIC X(80)`. */
export const messageLength = 80;

/** `ERRMSG`, the error line on every map. */
export const errorMessageLength = 78;

/** A `MOVE` into a `PIC X(n)` field: pad with spaces on the right, truncate. */
export function toFixed(value: string, length: number): string {
  return value.length >= length ? value.slice(0, length) : value.padEnd(length, " ");
}

/** The header fields `POPULATE-HEADER-INFO` sets on every map. */
export interface ScreenHeaderFields {
  readonly trnname: string;
  readonly title01: string;
  readonly title02: string;
  readonly pgmname: string;
  readonly curdate: string;
  readonly curtime: string;
}

const pad2 = (value: number): string => String(value).padStart(2, "0");

/** `WS-CURDATE-MM-DD-YY`. */
export function formatDate(now: Date): string {
  return `${pad2(now.getMonth() + 1)}/${pad2(now.getDate())}/${pad2(now.getFullYear() % 100)}`;
}

/** `WS-CURTIME-HH-MM-SS`. */
export function formatTime(now: Date): string {
  return `${pad2(now.getHours())}:${pad2(now.getMinutes())}:${pad2(now.getSeconds())}`;
}

export function buildHeader(
  transactionId: string,
  programName: string,
  now: Date,
): ScreenHeaderFields {
  return {
    trnname: transactionId,
    title01: ScreenTitle.title01,
    title02: ScreenTitle.title02,
    pgmname: programName,
    curdate: formatDate(now),
    curtime: formatTime(now),
  };
}

/** What a CICS program receives: the map fields, the AID key and the commarea. */
export interface HandlerRequest<TFields> {
  readonly aid: AidKeyCode;
  /** Omitted when `EIBCALEN` is zero, i.e. the program is entered fresh. */
  readonly commarea?: CardDemoCommarea;
  readonly fields?: Partial<TFields>;
}

/** `EXEC CICS SEND MAP` followed by `RETURN TRANSID`. */
export interface SendMapResponse<TFields> {
  readonly kind: "map";
  readonly map: string;
  readonly mapset: string;
  readonly transactionId: string;
  readonly fields: TFields;
  /** Field the `MOVE -1 TO ...L` cursor request points at, when there is one. */
  readonly cursor?: string;
  readonly commarea: CardDemoCommarea;
}

/** `EXEC CICS SEND TEXT` followed by a bare `RETURN`; the session ends. */
export interface SendTextResponse {
  readonly kind: "text";
  readonly text: string;
}

/** `EXEC CICS XCTL` to the next program. */
export interface TransferResponse {
  readonly kind: "transfer";
  readonly program: string;
  /** Omitted when the `XCTL` carries no `COMMAREA`, i.e. `EIBCALEN` is zero. */
  readonly commarea?: CardDemoCommarea;
}

export type HandlerResponse<TFields> =
  | SendMapResponse<TFields>
  | SendTextResponse
  | TransferResponse;

/** Options every handler accepts, so screens are reproducible in tests. */
export interface HandlerOptions {
  /** Clock used for the header date and time; defaults to the current time. */
  readonly now?: Date;
}
