package com.carddemo.user;

import com.carddemo.common.data.CobolField;
import com.carddemo.common.data.CopybookRecordMapper;

/**
 * One USRSEC record (copybook CSUSR01Y) as an immutable value. {@link #MAPPER} converts it to and from
 * the fixed-width record; {@link UserSecurity} persists it.
 *
 * @param usrId SEC-USR-ID PIC X(08)
 * @param firstName SEC-USR-FNAME PIC X(20)
 * @param lastName SEC-USR-LNAME PIC X(20)
 * @param password SEC-USR-PWD PIC X(08)
 * @param usrType SEC-USR-TYPE PIC X(01)
 */
public record UserSecurityRecord(
        @CobolField("SEC-USR-ID") String usrId,
        @CobolField("SEC-USR-FNAME") String firstName,
        @CobolField("SEC-USR-LNAME") String lastName,
        @CobolField("SEC-USR-PWD") String password,
        @CobolField("SEC-USR-TYPE") UserType usrType) {

    public static final CopybookRecordMapper<UserSecurityRecord> MAPPER =
            CopybookRecordMapper.of(UserSecurityRecord.class, "CSUSR01Y");
}
