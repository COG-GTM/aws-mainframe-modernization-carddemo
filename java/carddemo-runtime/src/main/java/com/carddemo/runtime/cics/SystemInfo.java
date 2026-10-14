package com.carddemo.runtime.cics;

/** Values returned by EXEC CICS ASSIGN APPLID / SYSID. */
public record SystemInfo(String applid, String sysid) {

    public static SystemInfo defaults() {
        return new SystemInfo("CARDDEMO", "CICS");
    }
}
