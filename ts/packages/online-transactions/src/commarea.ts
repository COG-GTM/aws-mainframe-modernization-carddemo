/**
 * `COCOM01Y` — the CARDDEMO-COMMAREA every online program passes on `XCTL`
 * and `RETURN TRANSID`, plus the `CDEMO-CTxx-INFO` extension the transaction
 * programs append to it.
 *
 * The signon and menu programs share this structure; once
 * `@carddemo/online-signon` exists these definitions move there and this
 * module re-exports them.
 */

/** `CDEMO-CUSTOMER-INFO`. */
export interface CustomerInfo {
  /** `CDEMO-CUST-ID` PIC 9(09). */
  readonly custId: number;
  /** `CDEMO-CUST-FNAME` PIC X(25). */
  readonly firstName: string;
  /** `CDEMO-CUST-MNAME` PIC X(25). */
  readonly middleName: string;
  /** `CDEMO-CUST-LNAME` PIC X(25). */
  readonly lastName: string;
}

/** `CDEMO-ACCOUNT-INFO`. */
export interface AccountInfo {
  /** `CDEMO-ACCT-ID` PIC 9(11). */
  readonly acctId: number;
  /** `CDEMO-ACCT-STATUS` PIC X(01). */
  readonly acctStatus: string;
}

/**
 * `CDEMO-CTxx-INFO`: the browse position, page number and row selection the
 * transaction programs keep in the commarea between pseudo-conversational
 * turns. `COTRN00C`, `COTRN01C` and `COTRN02C` redefine the same bytes, so a
 * single structure serves all three.
 */
export interface TransactionListState {
  /** `CDEMO-CTxx-TRNID-FIRST` PIC X(16): id shown on the first row. */
  readonly trnIdFirst: string;
  /** `CDEMO-CTxx-TRNID-LAST` PIC X(16): id shown on the last row. */
  readonly trnIdLast: string;
  /** `CDEMO-CTxx-PAGE-NUM` PIC 9(08). */
  readonly pageNum: number;
  /** `CDEMO-CTxx-NEXT-PAGE-FLG` PIC X(01), `Y` when a further page exists. */
  readonly nextPageFlag: "Y" | "N";
  /** `CDEMO-CTxx-TRN-SEL-FLG` PIC X(01): the selection character typed. */
  readonly trnSelFlag: string;
  /** `CDEMO-CTxx-TRN-SELECTED` PIC X(16): the id the selection refers to. */
  readonly trnSelected: string;
}

/** `CARDDEMO-COMMAREA` as the transaction programs see it. */
export interface CardDemoCommarea {
  /** `CDEMO-FROM-TRANID` PIC X(04). */
  readonly fromTranId: string;
  /** `CDEMO-FROM-PROGRAM` PIC X(08). */
  readonly fromProgram: string;
  /** `CDEMO-TO-TRANID` PIC X(04). */
  readonly toTranId: string;
  /** `CDEMO-TO-PROGRAM` PIC X(08). */
  readonly toProgram: string;
  /** `CDEMO-USER-ID` PIC X(08). */
  readonly userId: string;
  /** `CDEMO-USER-TYPE` PIC X(01). */
  readonly userType: string;
  /**
   * `CDEMO-PGM-CONTEXT` PIC 9(01); `0` is `CDEMO-PGM-ENTER`, `1` is
   * `CDEMO-PGM-REENTER`, the flag that tells a program whether the screen it
   * is about to read was sent by itself.
   */
  readonly pgmContext: 0 | 1;
  readonly customer: CustomerInfo;
  readonly account: AccountInfo;
  /** `CDEMO-CARD-NUM` PIC 9(16). */
  readonly cardNum: number;
  /** `CDEMO-LAST-MAP` PIC X(7). */
  readonly lastMap: string;
  /** `CDEMO-LAST-MAPSET` PIC X(7). */
  readonly lastMapset: string;
  readonly listState: TransactionListState;
}

export const emptyTransactionListState = (): TransactionListState => ({
  trnIdFirst: "",
  trnIdLast: "",
  pageNum: 0,
  nextPageFlag: "N",
  trnSelFlag: "",
  trnSelected: "",
});

/** A commarea with every field at `SPACES`/`ZEROS`, as `INITIALIZE` leaves it. */
export const emptyCommarea = (): CardDemoCommarea => ({
  fromTranId: "",
  fromProgram: "",
  toTranId: "",
  toProgram: "",
  userId: "",
  userType: "",
  pgmContext: 0,
  customer: { custId: 0, firstName: "", middleName: "", lastName: "" },
  account: { acctId: 0, acctStatus: "" },
  cardNum: 0,
  lastMap: "",
  lastMapset: "",
  listState: emptyTransactionListState(),
});

/** `CDEMO-PGM-REENTER` — the screen was sent by this program. */
export const isReenter = (commarea: CardDemoCommarea): boolean => commarea.pgmContext === 1;
