package com.carddemo.programs.cosgn00c;

/** Gateway for {@code EXEC CICS ASSIGN APPLID(...)} and {@code EXEC CICS ASSIGN SYSID(...)}. */
public interface CicsSystemInfo {

    /** APPLID of the CICS region (8 bytes). */
    String applid();

    /** SYSID of the CICS region (4 bytes, moved into an 8-byte map field). */
    String sysid();

    static CicsSystemInfo of(String applid, String sysid) {
        return new CicsSystemInfo() {
            @Override
            public String applid() {
                return applid;
            }

            @Override
            public String sysid() {
                return sysid;
            }
        };
    }
}
