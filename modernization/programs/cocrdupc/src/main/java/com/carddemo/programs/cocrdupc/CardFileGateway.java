package com.carddemo.programs.cocrdupc;

import com.carddemo.programs.cocrdupc.copybook.CardRecord;

/** CICS file control on CARDDAT (VSAM KSDS, key CARD-NUM X(16), record CVACT02Y). */
public interface CardFileGateway {

    /** EXEC CICS READ FILE('CARDDAT ') RIDFLD(card-num) INTO(CARD-RECORD) RESP RESP2. */
    FileResult read(String cardNum);

    /** EXEC CICS READ FILE('CARDDAT ') UPDATE RIDFLD(card-num) ...: acquires the record lock. */
    FileResult readForUpdate(String cardNum);

    /** EXEC CICS REWRITE FILE('CARDDAT ') FROM(record): requires a prior READ UPDATE. */
    FileResult rewrite(CardRecord record);

    /** Task end (CICS RETURN / SYNCPOINT / ABEND): releases any record locks the task still holds. */
    default void syncpoint() {
    }

    /** RESP / RESP2 plus the record read (null unless NORMAL). */
    record FileResult(CicsResp resp, int resp2, CardRecord record) {
        public static FileResult normal(CardRecord r) {
            return new FileResult(CicsResp.NORMAL, 0, r);
        }

        public static FileResult of(CicsResp resp, int resp2) {
            return new FileResult(resp, resp2, null);
        }
    }
}
