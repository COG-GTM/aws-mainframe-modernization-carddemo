package com.carddemo.programs.cobil00c.gateway;

import com.carddemo.programs.cobil00c.copybook.TranRecord;

/** CICS file TRANSACT (VSAM KSDS keyed on TRAN-ID, 16 bytes). */
public interface TransactGateway {

    /** {@code EXEC CICS STARTBR DATASET('TRANSACT') RIDFLD(TRAN-ID)} (GTEQ; all X'FF' = position at end). */
    CicsResponse<Void> startBrowse(String tranIdKey);

    /** {@code EXEC CICS READPREV DATASET('TRANSACT') INTO(TRAN-RECORD) RIDFLD(TRAN-ID)}. */
    CicsResponse<TranRecord> readPrev();

    /** {@code EXEC CICS ENDBR DATASET('TRANSACT')}; issued without RESP so failures abend. */
    void endBrowse();

    /** {@code EXEC CICS WRITE DATASET('TRANSACT') FROM(TRAN-RECORD) RIDFLD(TRAN-ID)}. */
    CicsResponse<Void> write(TranRecord record);
}
