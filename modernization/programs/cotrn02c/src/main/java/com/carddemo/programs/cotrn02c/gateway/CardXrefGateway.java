package com.carddemo.programs.cotrn02c.gateway;

import com.carddemo.programs.cotrn02c.copybook.CardXrefRecord;

/** CICS file control on the card cross-reference: CCXREF (KSDS by card) and CXACAIX (AIX path by account). */
public interface CardXrefGateway {

    /** Result of an EXEC CICS READ: the RESP/RESP2 pair and the record (null unless NORMAL/DUPKEY). */
    record ReadResult(CicsResp resp, int resp2, CardXrefRecord record) {
    }

    /** {@code READ DATASET('CXACAIX') RIDFLD(XREF-ACCT-ID)}. */
    ReadResult readByAcctId(long acctId);

    /** {@code READ DATASET('CCXREF') RIDFLD(XREF-CARD-NUM)}. */
    ReadResult readByCardNum(String cardNum);
}
