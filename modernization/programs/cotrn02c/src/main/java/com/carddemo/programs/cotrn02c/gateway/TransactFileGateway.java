package com.carddemo.programs.cotrn02c.gateway;

import com.carddemo.programs.cotrn02c.copybook.TranRecord;

/** CICS file control on the TRANSACT KSDS (key TRAN-ID X(16)). */
public interface TransactFileGateway {

    /** RESP/RESP2 of a file-control command, plus the record for READPREV. */
    record Result(CicsResp resp, int resp2, TranRecord record) {
        public static Result of(CicsResp resp) {
            return new Result(resp, 0, null);
        }
    }

    /** {@code STARTBR DATASET('TRANSACT') RIDFLD(key)}; an all-HIGH-VALUES key positions at end of file. */
    Result startBrowse(String key);

    /** {@code READPREV DATASET('TRANSACT') INTO(TRAN-RECORD)}. */
    Result readPrev();

    /** {@code ENDBR DATASET('TRANSACT')}. */
    void endBrowse();

    /** {@code WRITE DATASET('TRANSACT') FROM(TRAN-RECORD) RIDFLD(TRAN-ID)}. */
    Result write(TranRecord record);
}
