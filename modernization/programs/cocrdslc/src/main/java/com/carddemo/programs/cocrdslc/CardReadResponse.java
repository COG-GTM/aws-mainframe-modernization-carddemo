package com.carddemo.programs.cocrdslc;

import com.carddemo.programs.cocrdslc.copybook.CardRecord;

/** Outcome of EXEC CICS READ ... INTO(CARD-RECORD) RESP(..) RESP2(..). {@code record} is set on NORMAL and on DUPKEY
 * (CICS still returns the first record of a non-unique alternate key). */
public record CardReadResponse(int resp, int resp2, CardRecord record) {

    public static CardReadResponse normal(CardRecord record) {
        return new CardReadResponse(CicsResp.NORMAL, 0, record);
    }

    public static CardReadResponse notFound() {
        return new CardReadResponse(CicsResp.NOTFND, 80, null);
    }

    public static CardReadResponse error(int resp, int resp2) {
        return new CardReadResponse(resp, resp2, null);
    }
}
