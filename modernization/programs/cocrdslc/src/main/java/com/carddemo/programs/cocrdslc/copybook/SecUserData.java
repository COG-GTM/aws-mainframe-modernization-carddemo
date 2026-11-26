package com.carddemo.programs.cocrdslc.copybook;

import com.carddemo.programs.cocrdslc.CobolText;
import com.carddemo.programs.cocrdslc.FixedWidthReader;

/** CSUSR01Y - SEC-USER-DATA (80 bytes). COPYed by COCRDSLC but not referenced by its logic. */
public record SecUserData(String secUsrId, String secUsrFname, String secUsrLname,
                          String secUsrPwd, String secUsrType, String secUsrFiller) {

    public static final int RECORD_LENGTH = 80;

    public SecUserData {
        secUsrId = CobolText.pic(secUsrId, 8);
        secUsrFname = CobolText.pic(secUsrFname, 20);
        secUsrLname = CobolText.pic(secUsrLname, 20);
        secUsrPwd = CobolText.pic(secUsrPwd, 8);
        secUsrType = CobolText.pic(secUsrType, 1);
        secUsrFiller = CobolText.pic(secUsrFiller, 23);
    }

    public static SecUserData parse(String line) {
        FixedWidthReader r = new FixedWidthReader(line, RECORD_LENGTH);
        return new SecUserData(r.x(8), r.x(20), r.x(20), r.x(8), r.x(1), r.x(23));
    }

    public String toFixedWidth() {
        return secUsrId + secUsrFname + secUsrLname + secUsrPwd + secUsrType + secUsrFiller;
    }
}
