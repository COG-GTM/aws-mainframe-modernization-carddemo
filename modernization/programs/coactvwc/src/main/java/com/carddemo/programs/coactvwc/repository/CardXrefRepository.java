package com.carddemo.programs.coactvwc.repository;

import com.carddemo.programs.coactvwc.copybook.CardXrefRecord;

/** Dataset CXACAIX: CARDXREF via the account-id alternate index path (KEYS(11,25) NONUNIQUEKEY). */
public interface CardXrefRepository {
    /** READ DATASET('CXACAIX') RIDFLD(acctIdX) KEYLENGTH(11). */
    FileReadResult<CardXrefRecord> readByAcctId(String acctIdX);
}
