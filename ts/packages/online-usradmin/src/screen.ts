/**
 * The typed request and response the COUSR handlers exchange with a transport,
 * standing in for `EXEC CICS RECEIVE MAP` / `SEND MAP` and `EXEC CICS RETURN`.
 */

import type { CardDemoCommarea } from "./commarea.js";

/** `DFHAID` — the attention identifiers the COUSR programs evaluate. */
export const AidKey = {
  enter: "ENTER",
  pf3: "PF3",
  pf4: "PF4",
  pf5: "PF5",
  pf7: "PF7",
  pf8: "PF8",
  pf12: "PF12",
  other: "OTHER",
} as const;

export type AidKeyCode = (typeof AidKey)[keyof typeof AidKey];

/** `DFHBMSCA` colours the programs move into `ERRMSGC`. */
export const MessageColor = {
  red: "RED",
  green: "GREEN",
  neutral: "NEUTRAL",
} as const;

export type MessageColorCode = (typeof MessageColor)[keyof typeof MessageColor];

/** The header fields every CardDemo map carries. */
export interface ScreenHeader {
  title01: string;
  title02: string;
  trnname: string;
  pgmname: string;
  curdate: string;
  curtime: string;
}

export interface ScreenBase {
  header: ScreenHeader;
  /** `ERRMSGO` — the message line at the foot of the map. */
  errmsg: string;
  /** `ERRMSGC` — the colour moved into the message line. */
  errmsgColor: MessageColorCode;
}

export interface HandlerRequest<TScreen> {
  readonly aid: AidKeyCode;
  readonly screen: TScreen;
  readonly commarea: CardDemoCommarea;
}

export interface HandlerResponse<TScreen> {
  readonly screen: TScreen;
  readonly commarea: CardDemoCommarea;
  /** `CDEMO-TO-PROGRAM` when control transfers, otherwise the program itself. */
  readonly nextProgram: string;
  /** True for `EXEC CICS XCTL`, false for `EXEC CICS RETURN` to the same map. */
  readonly transfer: boolean;
  /** `EXEC CICS SEND ... ERASE`; false only for the `COUSR00C` paging edges. */
  readonly erase: boolean;
}

export interface HandlerOptions {
  /** Injected for the `FUNCTION CURRENT-DATE` header stamp. */
  readonly now?: Date;
  /** Write the dataset back to disk after a successful update; defaults to true. */
  readonly persist?: boolean;
}
