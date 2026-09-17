/**
 * `COCOM01Y` — the CardDemo commarea every online program passes to the next,
 * plus the pieces of `CVCRD01Y` (AID key, next program/mapset/map) the card
 * programs use to describe where control goes next.
 *
 * A sibling migration owns these types in `@carddemo/online-signon`; they are
 * defined here so this package builds on its own and are meant to be
 * consolidated once both packages land.
 */

/** `DFHAID` keys, after the remapping `YYYY-STORE-PFKEY` performs. */
export type AidKey =
  | "ENTER"
  | "CLEAR"
  | "PA1"
  | "PA2"
  | "PFK01"
  | "PFK02"
  | "PFK03"
  | "PFK04"
  | "PFK05"
  | "PFK06"
  | "PFK07"
  | "PFK08"
  | "PFK09"
  | "PFK10"
  | "PFK11"
  | "PFK12";

/** `CDEMO-PGM-CONTEXT`: first pass through a program versus a re-entry. */
export type ProgramContext = "enter" | "reenter";

export type UserType = "A" | "U";

export interface CardDemoCommarea {
  readonly fromTranId: string;
  readonly fromProgram: string;
  readonly toTranId: string;
  readonly toProgram: string;
  readonly userId: string;
  readonly userType: UserType;
  readonly programContext: ProgramContext;
  readonly custId: string;
  readonly custFirstName: string;
  readonly custMiddleName: string;
  readonly custLastName: string;
  /** `CDEMO-ACCT-ID`, `PIC 9(11)`, kept as its 11 digit display form. */
  readonly acctId: string;
  readonly acctStatus: string;
  /** `CDEMO-CARD-NUM`, `PIC 9(16)`, kept as its 16 digit display form. */
  readonly cardNum: string;
  readonly lastMap: string;
  readonly lastMapSet: string;
}

export const ACCT_ID_LENGTH = 11;
export const CARD_NUM_LENGTH = 16;

export const ZERO_ACCT_ID = "0".repeat(ACCT_ID_LENGTH);
export const ZERO_CARD_NUM = "0".repeat(CARD_NUM_LENGTH);

export const emptyCommarea = (): CardDemoCommarea => ({
  fromTranId: "",
  fromProgram: "",
  toTranId: "",
  toProgram: "",
  userId: "",
  userType: "U",
  programContext: "enter",
  custId: "",
  custFirstName: "",
  custMiddleName: "",
  custLastName: "",
  acctId: ZERO_ACCT_ID,
  acctStatus: "",
  cardNum: ZERO_CARD_NUM,
  lastMap: "",
  lastMapSet: "",
});

/** The programs, transactions and maps the card screens transfer between. */
export const programs = {
  cardList: "COCRDLIC",
  cardDetail: "COCRDSLC",
  cardUpdate: "COCRDUPC",
  menu: "COMEN01C",
} as const;

export const tranIds = {
  cardList: "CCLI",
  cardDetail: "CCDL",
  cardUpdate: "CCUP",
  menu: "CM00",
} as const;

export const mapSets = {
  cardList: "COCRDLI",
  cardDetail: "COCRDSL",
  cardUpdate: "COCRDUP",
  menu: "COMEN01",
} as const;

export const maps = {
  cardList: "CCRDLIA",
  cardDetail: "CCRDSLA",
  cardUpdate: "CCRDUPA",
  menu: "COMEN1A",
} as const;

/** `CARDDAT`, the card file as the CICS programs name it. */
export const CARD_FILE_NAME = "CARDDAT";

/** `COTTL01Y` screen titles. */
export const titles = {
  title01: "      AWS Mainframe Modernization       ",
  title02: "              CardDemo                  ",
} as const;

/** The header fields every card map carries. */
export interface ScreenHeader {
  readonly title01: string;
  readonly title02: string;
  readonly tranName: string;
  readonly pgmName: string;
  /** `WS-CURDATE-MM-DD-YY`. */
  readonly curDate: string;
  /** `WS-CURTIME-HH-MM-SS`. */
  readonly curTime: string;
}

/** What a handler decided to do once it finished: send a map, or transfer. */
export interface HandlerOutcome<TScreen> {
  readonly commarea: CardDemoCommarea;
  readonly nextProgram: string;
  readonly nextMapSet: string;
  readonly nextMap: string;
  /** `true` when the COBOL program issued `EXEC CICS XCTL` instead of a map. */
  readonly transferControl: boolean;
  /** The map that was sent; absent when control was transferred. */
  readonly screen?: TScreen;
}

const pad2 = (value: number): string => String(value).padStart(2, "0");

export const screenHeader = (tranId: string, program: string, now: Date): ScreenHeader => ({
  title01: titles.title01,
  title02: titles.title02,
  tranName: tranId,
  pgmName: program,
  curDate: `${pad2(now.getMonth() + 1)}/${pad2(now.getDate())}/${pad2(now.getFullYear() % 100)}`,
  curTime: `${pad2(now.getHours())}:${pad2(now.getMinutes())}:${pad2(now.getSeconds())}`,
});
