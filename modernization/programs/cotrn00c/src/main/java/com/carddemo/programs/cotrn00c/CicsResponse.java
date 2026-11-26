package com.carddemo.programs.cotrn00c;

/** RESP / RESP2 pair returned by a file-control command. */
public record CicsResponse(CicsResp resp, int resp2) {

    public static final CicsResponse NORMAL = new CicsResponse(CicsResp.NORMAL, 0);

    public static CicsResponse of(CicsResp resp) {
        return new CicsResponse(resp, 0);
    }
}
