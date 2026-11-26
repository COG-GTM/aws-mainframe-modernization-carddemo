package com.carddemo.programs.coactupc.copybook;

/**
 * CSUSR01Y - SEC-USER-DATA. COPYed by COACTUPC but not referenced by its procedure division.
 *
 * @param usrId    SEC-USR-ID PIC X(08)
 * @param fname    SEC-USR-FNAME PIC X(20)
 * @param lname    SEC-USR-LNAME PIC X(20)
 * @param pwd      SEC-USR-PWD PIC X(08)
 * @param type     SEC-USR-TYPE PIC X(01)
 * @param filler   SEC-USR-FILLER PIC X(23)
 */
public record SecUserData(String usrId, String fname, String lname, String pwd, String type, String filler) {

    public static final int RECORD_LENGTH = 80;
}
