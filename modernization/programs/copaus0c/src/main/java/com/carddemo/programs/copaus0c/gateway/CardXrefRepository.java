package com.carddemo.programs.copaus0c.gateway;

import com.carddemo.programs.copaus0c.copybook.CardXrefRecord;

/** CICS READ DATASET('CXACAIX ') - card cross-reference via the account-id alternate index (NONUNIQUEKEY). */
public interface CardXrefRepository {
    String DATASET = "CXACAIX ";

    /** @param acctIdKey WS-CARD-RID-ACCT-ID-X PIC X(11) */
    FileReadResult<CardXrefRecord> readByAcctId(String acctIdKey);
}
