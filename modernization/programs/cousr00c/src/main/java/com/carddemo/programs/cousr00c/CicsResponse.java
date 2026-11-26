package com.carddemo.programs.cousr00c;

/** RESP / RESP2 pair returned by a CICS command. */
public record CicsResponse(CicsResp resp, int resp2) {

    public static final CicsResponse NORMAL = new CicsResponse(CicsResp.NORMAL, 0);

    public static CicsResponse of(CicsResp resp) {
        return new CicsResponse(resp, 0);
    }
}
