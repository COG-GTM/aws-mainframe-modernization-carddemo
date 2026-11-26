package com.carddemo.programs.cocrdlic;

import com.carddemo.programs.cocrdlic.copybook.CardRecord;

/**
 * CICS file control browse over the CARDDAT KSDS (key CARD-NUM X(16)): STARTBR GTEQ, READNEXT, READPREV, ENDBR.
 * Implementations hold one browse cursor per file, like a CICS task.
 */
public interface CardFileGateway {

    /** Result of READNEXT / READPREV. {@code record} is null unless resp is NORMAL or DUPREC. */
    record ReadResult(int resp, int resp2, CardRecord record) {
        public boolean isOk() {
            return resp == CicsResp.NORMAL || resp == CicsResp.DUPREC;
        }
    }

    /** EXEC CICS STARTBR DATASET(file) RIDFLD(key) GTEQ; returns RESP. */
    int startBrowseGteq(String file, String key);

    /** EXEC CICS READNEXT. */
    ReadResult readNext(String file);

    /** EXEC CICS READPREV. */
    ReadResult readPrev(String file);

    /** EXEC CICS ENDBR; returns RESP. */
    int endBrowse(String file);
}
