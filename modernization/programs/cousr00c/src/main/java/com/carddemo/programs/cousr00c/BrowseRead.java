package com.carddemo.programs.cousr00c;

import com.carddemo.programs.cousr00c.copybook.SecUserData;

/** Result of READNEXT / READPREV: the response and, when NORMAL, the record read INTO SEC-USER-DATA. */
public record BrowseRead(CicsResponse response, SecUserData record) {

    public static BrowseRead of(CicsResp resp) {
        return new BrowseRead(CicsResponse.of(resp), null);
    }
}
