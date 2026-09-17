/**
 * The transport-independent shape of a 3270 conversation: the attention
 * identifier a program receives in `EIBAID`, the header fields every map
 * carries, and the request/response pair a handler works with.
 */

import type { CardDemoCommarea } from "./commarea.js";

/** `DFHAID` values the transaction programs branch on. */
export const AidKey = {
  enter: "ENTER",
  pf3: "PF3",
  pf4: "PF4",
  pf5: "PF5",
  pf7: "PF7",
  pf8: "PF8",
  clear: "CLEAR",
  pa1: "PA1",
  pa2: "PA2",
} as const;

export type AidKeyValue = (typeof AidKey)[keyof typeof AidKey];

/** `COTTL01Y` / `CSDAT01Y`: the title, transaction, program, date and time. */
export interface ScreenHeader {
  readonly title01: string;
  readonly title02: string;
  readonly tranName: string;
  readonly pgmName: string;
  /** `CURDATE` in `MM/DD/YY`. */
  readonly curDate: string;
  /** `CURTIME` in `HH:MM:SS`. */
  readonly curTime: string;
}

/** `CCDA-TITLE01`/`CCDA-TITLE02` from `COTTL01Y`. */
export const TITLE01 = "AWS Mainframe Modernization";
export const TITLE02 = "CardDemo";

/** Colour of the `ERRMSG` field; `DFHGREEN` marks the success messages. */
export type MessageColor = "red" | "green";

export interface OnlineRequest<TScreen> {
  /** The map fields as received by `RECEIVE MAP`. */
  readonly screen: TScreen;
  readonly aid: AidKeyValue;
  /** `DFHCOMMAREA`; `undefined` models `EIBCALEN = 0`. */
  readonly commarea?: CardDemoCommarea;
}

export interface OnlineResponse<TScreen> {
  /** The map fields as they would be sent by `SEND MAP`. */
  readonly screen: TScreen;
  /** Header fields `POPULATE-HEADER-INFO` fills in before every `SEND MAP`. */
  readonly header: ScreenHeader;
  readonly commarea: CardDemoCommarea;
  /**
   * Program that owns the next turn: the program named on `XCTL` when
   * {@link transfer} is set, otherwise the program itself re-displaying its
   * own map before `RETURN TRANSID`.
   */
  readonly nextProgram: string;
  /** `true` for `XCTL`, `false` for `RETURN TRANSID` with the map sent. */
  readonly transfer: boolean;
  /** Field that received `MOVE -1 TO <field>L`, i.e. where the cursor lands. */
  readonly cursor?: string;
  readonly messageColor: MessageColor;
}

/** `POPULATE-HEADER-INFO`: title, transaction, program and the clock. */
export const headerFor = (tranName: string, pgmName: string, now: Date): ScreenHeader => ({
  title01: TITLE01,
  title02: TITLE02,
  tranName,
  pgmName,
  curDate: formatHeaderDate(now),
  curTime: formatHeaderTime(now),
});

const pad2 = (value: number): string => String(value).padStart(2, "0");

/** `WS-CURDATE-MM-DD-YY` of `CSDAT01Y`. */
export function formatHeaderDate(now: Date): string {
  return `${pad2(now.getMonth() + 1)}/${pad2(now.getDate())}/${pad2(now.getFullYear() % 100)}`;
}

/** `WS-CURTIME-HH-MM-SS` of `CSDAT01Y`. */
export function formatHeaderTime(now: Date): string {
  return `${pad2(now.getHours())}:${pad2(now.getMinutes())}:${pad2(now.getSeconds())}`;
}
