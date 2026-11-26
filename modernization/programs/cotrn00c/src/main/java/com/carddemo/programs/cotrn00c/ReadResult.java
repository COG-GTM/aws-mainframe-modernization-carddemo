package com.carddemo.programs.cotrn00c;

import com.carddemo.programs.cotrn00c.copybook.TranRecord;

/** Result of READNEXT / READPREV: the response and, when NORMAL, the record read INTO TRAN-RECORD. */
public record ReadResult(CicsResponse response, TranRecord record) {

    public static ReadResult of(TranRecord record) {
        return new ReadResult(CicsResponse.NORMAL, record);
    }

    public static ReadResult of(CicsResp resp) {
        return new ReadResult(CicsResponse.of(resp), null);
    }
}
