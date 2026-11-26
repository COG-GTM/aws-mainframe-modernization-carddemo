package com.carddemo.programs.cocrdlic.copybook;

/** CSUSR01Y - SEC-USER-DATA (80 bytes). COPY'd by COCRDLIC but not referenced by its logic. */
public record SecUserData(String secUsrId, String secUsrFname, String secUsrLname, String secUsrPwd,
                          char secUsrType, String secUsrFiller) {
    public static final int LENGTH = 80;

    public SecUserData {
        secUsrId = Pic.x(secUsrId, 8);
        secUsrFname = Pic.x(secUsrFname, 20);
        secUsrLname = Pic.x(secUsrLname, 20);
        secUsrPwd = Pic.x(secUsrPwd, 8);
        secUsrFiller = Pic.x(secUsrFiller, 23);
    }

    public String toFixedWidth() {
        return secUsrId + secUsrFname + secUsrLname + secUsrPwd + secUsrType + secUsrFiller;
    }
}
