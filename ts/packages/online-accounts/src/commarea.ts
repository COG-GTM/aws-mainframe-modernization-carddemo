/**
 * `COCOM01Y` and `CVCRD01Y` — the CardDemo communication area and the screen
 * work area every online program passes to the next one.
 *
 * `@carddemo/online-signon` owns these types once that package lands; the
 * definitions here are the local equivalents this package uses in the meantime.
 */

/** `CCARD-AID-*`: the attention identifier the terminal sent. */
export type AidKey =
  | "ENTER"
  | "CLEAR"
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

/** `CDEMO-USER-TYPE`. */
export type UserType = "A" | "U" | "";

/** `CDEMO-PGM-CONTEXT`: 0 is `CDEMO-PGM-ENTER`, 1 is `CDEMO-PGM-REENTER`. */
export type ProgramContext = "ENTER" | "REENTER";

export interface CardDemoCommarea {
  readonly fromTranid: string;
  readonly fromProgram: string;
  readonly toTranid: string;
  readonly toProgram: string;
  readonly userId: string;
  readonly userType: UserType;
  readonly programContext: ProgramContext;
  readonly custId: number;
  readonly custFname: string;
  readonly custMname: string;
  readonly custLname: string;
  readonly acctId: number;
  readonly acctStatus: string;
  readonly cardNum: string;
  readonly lastMap: string;
  readonly lastMapset: string;
}

export const emptyCommarea: CardDemoCommarea = {
  fromTranid: "",
  fromProgram: "",
  toTranid: "",
  toProgram: "",
  userId: "",
  userType: "",
  programContext: "ENTER",
  custId: 0,
  custFname: "",
  custMname: "",
  custLname: "",
  acctId: 0,
  acctStatus: "",
  cardNum: "",
  lastMap: "",
  lastMapset: "",
};

/** The `CCARD-*` fields of the work area the programs hand back to CICS. */
export interface ScreenControl {
  /** `CCARD-NEXT-PROG`. */
  readonly nextProgram: string;
  /** `CCARD-NEXT-MAPSET`. */
  readonly nextMapset: string;
  /** `CCARD-NEXT-MAP`. */
  readonly nextMap: string;
  /** `CCARD-ERROR-MSG`. */
  readonly errorMessage: string;
  /** True when the program issued `EXEC CICS XCTL` instead of sending a map. */
  readonly transferControl: boolean;
}

export const MENU_PROGRAM = "COMEN01C";
export const MENU_TRANID = "CM00";

/** `CDEMO-TO-PROGRAM` / `CDEMO-TO-TRANID` for the PF03 exit path. */
export function exitTarget(commarea: CardDemoCommarea): {
  toProgram: string;
  toTranid: string;
} {
  return {
    toProgram: commarea.fromProgram.trim().length === 0 ? MENU_PROGRAM : commarea.fromProgram,
    toTranid: commarea.fromTranid.trim().length === 0 ? MENU_TRANID : commarea.fromTranid,
  };
}
