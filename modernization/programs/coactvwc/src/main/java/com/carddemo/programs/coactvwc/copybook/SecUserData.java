package com.carddemo.programs.coactvwc.copybook;

import com.carddemo.programs.coactvwc.support.Cobol;

/** CSUSR01Y: {@code SEC-USER-DATA} (80 bytes). COPYed by COACTVWC but not referenced in its logic. */
public record SecUserData(String secUsrId, String secUsrFname, String secUsrLname, String secUsrPwd,
                          String secUsrType, String secUsrFiller) {
    public static final int LENGTH = 80;

    public static SecUserData parse(String line) {
        String s = Cobol.alnum(line, LENGTH);
        return new SecUserData(s.substring(0, 8), s.substring(8, 28), s.substring(28, 48), s.substring(48, 56),
                s.substring(56, 57), s.substring(57, 80));
    }

    public String toFixedWidth() {
        return Cobol.alnum(secUsrId, 8) + Cobol.alnum(secUsrFname, 20) + Cobol.alnum(secUsrLname, 20)
                + Cobol.alnum(secUsrPwd, 8) + Cobol.alnum(secUsrType, 1) + Cobol.alnum(secUsrFiller, 23);
    }
}
