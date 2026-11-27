package com.carddemo.data.record;

import com.carddemo.common.codec.CobolField;

/** Fixed-width SEC-USER-DATA. Layout: copybook CSUSR01Y. */
public record UserSecurityRecord(
        @CobolField("SEC-USR-ID") String userId,
        @CobolField("SEC-USR-FNAME") String firstName,
        @CobolField("SEC-USR-LNAME") String lastName,
        @CobolField("SEC-USR-PWD") String password,
        @CobolField("SEC-USR-TYPE") String userType) {
}
