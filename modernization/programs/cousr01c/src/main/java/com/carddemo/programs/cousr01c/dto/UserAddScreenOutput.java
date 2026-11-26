package com.carddemo.programs.cousr01c.dto;

import com.carddemo.programs.cousr01c.copybook.Cousr1aMap.Field;
import com.carddemo.programs.cousr01c.copybook.Pic;

/**
 * Snapshot of COUSR1AO at EXEC CICS SEND MAP('COUSR1A') MAPSET('COUSR01') ERASE CURSOR.
 * Data values are the raw fixed-width xxxO fields; a field of all low-values is not transmitted
 * (the screen shows the BMS default). {@code errmsgColor} is ERRMSGC (low-value = BMS default RED).
 * {@code cursor} is the first field whose length was -1, or null when none was set.
 */
public record UserAddScreenOutput(
        String trnname,
        String title01,
        String curdate,
        String pgmname,
        String title02,
        String curtime,
        String fname,
        String lname,
        String userid,
        String passwd,
        String usrtype,
        String errmsg,
        char errmsgColor,
        Field cursor) {

    /** ERRMSG as displayed (low-values blanked, trailing blanks trimmed). */
    public String errmsgText() {
        return Pic.display(errmsg);
    }
}
