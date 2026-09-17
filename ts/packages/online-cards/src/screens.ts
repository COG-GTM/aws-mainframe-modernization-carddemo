/**
 * The screen field sets of the three card maps, `COCRDLI`, `COCRDSL` and
 * `COCRDUP`. Input types carry the fields the map sends back (`...I`), output
 * types the fields the program fills in (`...O`); attribute bytes are outside
 * the scope of the handlers.
 */

import type { ScreenHeader } from "./commarea.js";

export const MAX_SCREEN_LINES = 7;

/** One of the seven `COCRDLI` list lines. */
export interface CardListRow {
  /** `CRDSELn`, the action code the user typed. */
  readonly select: string;
  /** `ACCTNOn`. */
  readonly acctNo: string;
  /** `CRDNUMn`. */
  readonly cardNum: string;
  /** `CRDSTSn`. */
  readonly cardStatus: string;
}

export const emptyCardListRow = (): CardListRow => ({
  select: "",
  acctNo: "",
  cardNum: "",
  cardStatus: "",
});

export interface CardListInput {
  /** `ACCTSID`, the account number filter. */
  readonly acctsId: string;
  /** `CARDSID`, the card number filter. */
  readonly cardsId: string;
  /** `CRDSEL1` .. `CRDSEL7`. */
  readonly selects: readonly string[];
}

export interface CardListScreen extends ScreenHeader {
  readonly pageNo: string;
  readonly acctsId: string;
  readonly cardsId: string;
  readonly rows: readonly CardListRow[];
  readonly infoMsg: string;
  readonly errMsg: string;
}

export interface CardDetailInput {
  /** `ACCTSID`. */
  readonly acctsId: string;
  /** `CARDSID`. */
  readonly cardsId: string;
}

export interface CardDetailScreen extends ScreenHeader {
  readonly acctsId: string;
  readonly cardsId: string;
  /** `CRDNAME`. */
  readonly crdName: string;
  /** `CRDSTCD`. */
  readonly crdStcd: string;
  /** `EXPMON`. */
  readonly expMon: string;
  /** `EXPYEAR`. */
  readonly expYear: string;
  readonly infoMsg: string;
  readonly errMsg: string;
}

export interface CardUpdateInput {
  readonly acctsId: string;
  readonly cardsId: string;
  readonly crdName: string;
  readonly crdStcd: string;
  readonly expMon: string;
  readonly expYear: string;
  /** `EXPDAY` is protected on the map and echoed back unchanged. */
  readonly expDay: string;
}

export interface CardUpdateScreen extends ScreenHeader {
  readonly acctsId: string;
  readonly cardsId: string;
  readonly crdName: string;
  readonly crdStcd: string;
  readonly expMon: string;
  readonly expYear: string;
  readonly expDay: string;
  readonly infoMsg: string;
  readonly errMsg: string;
}
