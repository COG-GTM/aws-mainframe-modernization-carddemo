/**
 * `COCOM01Y` — the CardDemo communication area, plus the user administration
 * context the COUSR programs redefine on the end of it.
 *
 * `COUSR00C`, `COUSR02C` and `COUSR03C` each append their own `CDEMO-CUxx-INFO`
 * group at the same offset, so the three groups are one overlay and are modelled
 * here as a single `userSelection` area shared by the four handlers.
 *
 * A sibling migration owns `@carddemo/online-signon`; once it lands, these
 * declarations move there and this module re-exports them.
 */

/** `CDEMO-USER-TYPE` — `CDEMO-USRTYP-ADMIN` / `CDEMO-USRTYP-USER`. */
export const UserType = {
  admin: "A",
  user: "U",
} as const;

export type UserTypeCode = (typeof UserType)[keyof typeof UserType];

/** `CDEMO-PGM-CONTEXT` — `CDEMO-PGM-ENTER` / `CDEMO-PGM-REENTER`. */
export const PgmContext = {
  enter: 0,
  reenter: 1,
} as const;

export type PgmContextCode = (typeof PgmContext)[keyof typeof PgmContext];

/** `CDEMO-CU00-INFO` / `CDEMO-CU02-INFO` / `CDEMO-CU03-INFO`. */
export interface UserSelectionArea {
  /** `CDEMO-CUxx-USRID-FIRST` — key of the first row on the page. */
  usridFirst: string;
  /** `CDEMO-CUxx-USRID-LAST` — key of the tenth row on the page. */
  usridLast: string;
  /** `CDEMO-CUxx-PAGE-NUM`. */
  pageNum: number;
  /** `CDEMO-CUxx-NEXT-PAGE-FLG` — `'Y'` when a further page exists. */
  nextPageFlg: string;
  /** `CDEMO-CUxx-USR-SEL-FLG` — the action typed against a listed user. */
  usrSelFlg: string;
  /** `CDEMO-CUxx-USR-SELECTED`. */
  usrSelected: string;
}

export interface CardDemoCommarea {
  fromTranid: string;
  fromProgram: string;
  toTranid: string;
  toProgram: string;
  userId: string;
  userType: string;
  pgmContext: PgmContextCode;
  custId: number;
  custFname: string;
  custMname: string;
  custLname: string;
  acctId: number;
  acctStatus: string;
  cardNum: number;
  lastMap: string;
  lastMapset: string;
  userSelection: UserSelectionArea;
}

export const programs = {
  signon: "COSGN00C",
  adminMenu: "COADM01C",
  userList: "COUSR00C",
  userAdd: "COUSR01C",
  userUpdate: "COUSR02C",
  userDelete: "COUSR03C",
} as const;

export const transactions = {
  userList: "CU00",
  userAdd: "CU01",
  userUpdate: "CU02",
  userDelete: "CU03",
} as const;

export function emptyUserSelection(): UserSelectionArea {
  return {
    usridFirst: "",
    usridLast: "",
    pageNum: 0,
    nextPageFlg: "N",
    usrSelFlg: "",
    usrSelected: "",
  };
}

export function emptyCommarea(): CardDemoCommarea {
  return {
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
    userSelection: emptyUserSelection(),
  };
}

/** `CDEMO-USRTYP-ADMIN`; only administrators reach the COUSR transactions. */
export function isAdmin(commarea: CardDemoCommarea): boolean {
  return commarea.userType.trim().toUpperCase() === UserType.admin;
}
