package com.carddemo.programs.cocrdslc;

/** CICS file control for the CARD-RECORD VSAM dataset. */
public interface CardFileGateway {

    /** File CARDDAT (LIT-CARDFILENAME): READ by primary key CARD-NUM, RIDFLD PIC X(16). */
    CardReadResponse readCardDat(String cardNum16);

    /** File CARDAIX (LIT-CARDFILENAME-ACCT-PATH): READ via the CARD-ACCT-ID alternate index, RIDFLD PIC 9(11). */
    CardReadResponse readCardAix(String acctId11);
}
