/**
 * `COCOM01Y` — CARDDEMO-COMMAREA, the state every CICS program in the
 * application passes to the next one on `XCTL` / `RETURN TRANSID`.
 *
 * The COBOL layout is a fixed 137 byte area; here it is a typed record. The
 * later phase 2 packages (accounts, cards, transactions, user admin) take and
 * return the same shape, so the handlers can be chained the way the CICS
 * programs transfer to each other.
 */

/** `CDEMO-USER-TYPE` values (`CDEMO-USRTYP-ADMIN` / `CDEMO-USRTYP-USER`). */
export const UserType = {
  admin: "A",
  user: "U",
} as const;

export type UserTypeCode = (typeof UserType)[keyof typeof UserType];

/** `CDEMO-PGM-CONTEXT` values (`CDEMO-PGM-ENTER` / `CDEMO-PGM-REENTER`). */
export const PgmContext = {
  enter: 0,
  reenter: 1,
} as const;

export type PgmContextCode = (typeof PgmContext)[keyof typeof PgmContext];

export interface CardDemoCommarea {
  readonly fromTranid: string;
  readonly fromProgram: string;
  readonly toTranid: string;
  readonly toProgram: string;
  readonly userId: string;
  readonly userType: string;
  readonly pgmContext: PgmContextCode;
  readonly custId: number;
  readonly custFname: string;
  readonly custMname: string;
  readonly custLname: string;
  readonly acctId: number;
  readonly acctStatus: string;
  readonly cardNum: number;
  readonly lastMap: string;
  readonly lastMapset: string;
}

const emptyCommarea: CardDemoCommarea = {
  fromTranid: "",
  fromProgram: "",
  toTranid: "",
  toProgram: "",
  userId: "",
  userType: "",
  pgmContext: PgmContext.enter,
  custId: 0,
  custFname: "",
  custMname: "",
  custLname: "",
  acctId: 0,
  acctStatus: "",
  cardNum: 0,
  lastMap: "",
  lastMapset: "",
};

/** A commarea of spaces and zeros, optionally overridden field by field. */
export function createCommarea(overrides: Partial<CardDemoCommarea> = {}): CardDemoCommarea {
  return { ...emptyCommarea, ...overrides };
}

/** `CDEMO-USRTYP-ADMIN`. */
export function isAdminUser(commarea: CardDemoCommarea): boolean {
  return commarea.userType === UserType.admin;
}

/** `CDEMO-PGM-REENTER`. */
export function isReenter(commarea: CardDemoCommarea): boolean {
  return commarea.pgmContext === PgmContext.reenter;
}
