package com.carddemo.programs.cocrdupc.copybook;

import static com.carddemo.programs.cocrdupc.Cobol.alnum;

/** CSUSR01Y: 01 SEC-USER-DATA (80 bytes). COPYed by COCRDUPC but not referenced. */
public record SecUserData(
        String secUsrId,      // X(08)
        String secUsrFname,   // X(20)
        String secUsrLname,   // X(20)
        String secUsrPwd,     // X(08)
        String secUsrType,    // X(01)
        String secUsrFiller   // X(23)
) {
    public static final int LENGTH = 80;

    public SecUserData {
        secUsrId = alnum(secUsrId, 8);
        secUsrFname = alnum(secUsrFname, 20);
        secUsrLname = alnum(secUsrLname, 20);
        secUsrPwd = alnum(secUsrPwd, 8);
        secUsrType = alnum(secUsrType, 1);
        secUsrFiller = alnum(secUsrFiller, 23);
    }

    public static SecUserData parse(String raw) {
        String r = alnum(raw, LENGTH);
        return new SecUserData(r.substring(0, 8), r.substring(8, 28), r.substring(28, 48),
                r.substring(48, 56), r.substring(56, 57), r.substring(57, 80));
    }

    public String toFixedWidth() {
        return secUsrId + secUsrFname + secUsrLname + secUsrPwd + secUsrType + secUsrFiller;
    }
}
