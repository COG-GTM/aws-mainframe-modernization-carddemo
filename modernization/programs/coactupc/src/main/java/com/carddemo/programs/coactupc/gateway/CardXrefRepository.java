package com.carddemo.programs.coactupc.gateway;

import com.carddemo.programs.coactupc.copybook.CardXrefRecord;

/** CXACAIX - card cross-reference alternate index path keyed by account id. */
public interface CardXrefRepository {

    /** EXEC CICS READ DATASET('CXACAIX') RIDFLD(acct-id X(11)). */
    FileResponse<CardXrefRecord> readByAcctId(String acctIdKey);
}
