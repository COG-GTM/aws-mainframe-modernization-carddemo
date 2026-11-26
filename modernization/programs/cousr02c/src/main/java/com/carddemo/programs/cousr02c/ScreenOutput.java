package com.carddemo.programs.cousr02c;

import com.carddemo.programs.cousr02c.copybook.Cousr2aMap;
import com.carddemo.programs.cousr02c.copybook.Pic;

/**
 * Snapshot of one {@code EXEC CICS SEND MAP('COUSR2A') MAPSET('COUSR02') FROM(COUSR2AO) ERASE CURSOR}: the
 * {@code xxxO} values (raw, fixed width), the ERRMSG colour byte (X'00' = BMS default, RED) and the cursor field.
 */
public record ScreenOutput(
        String trnname,
        String title01,
        String curdate,
        String pgmname,
        String title02,
        String curtime,
        String usrid,
        String fname,
        String lname,
        String passwd,
        String usrtype,
        String errmsg,
        char errmsgColor,
        String cursorField) {

    static ScreenOutput of(Cousr2aMap map) {
        return new ScreenOutput(map.trnname.getData(), map.title01.getData(), map.curdate.getData(),
                map.pgmname.getData(), map.title02.getData(), map.curtime.getData(), map.usridin.getData(),
                map.fname.getData(), map.lname.getData(), map.passwd.getData(), map.usrtype.getData(),
                map.errmsg.getData(), map.errmsg.getColor(), map.cursorField().name());
    }

    /** ERRMSG without trailing spaces. */
    public String message() {
        return Pic.trimRight(errmsg);
    }
}
