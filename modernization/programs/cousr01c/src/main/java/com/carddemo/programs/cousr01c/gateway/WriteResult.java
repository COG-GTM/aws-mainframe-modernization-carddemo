package com.carddemo.programs.cousr01c.gateway;

/** RESP / RESP2 of an EXEC CICS WRITE (WS-RESP-CD / WS-REAS-CD). */
public record WriteResult(CicsResp resp, int resp2) {

    public static WriteResult normal() {
        return new WriteResult(CicsResp.NORMAL, 0);
    }
}
