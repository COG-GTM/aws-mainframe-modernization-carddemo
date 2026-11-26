package com.carddemo.programs.cousr02c.gateway;

import com.carddemo.programs.cousr02c.copybook.CicsResp;

/** RESP / RESP2 pair returned by a CICS file-control command. */
public record CicsResult(int resp, int resp2) {

    public static final CicsResult NORMAL = new CicsResult(CicsResp.NORMAL, 0);

    public boolean isNormal() {
        return resp == CicsResp.NORMAL;
    }
}
