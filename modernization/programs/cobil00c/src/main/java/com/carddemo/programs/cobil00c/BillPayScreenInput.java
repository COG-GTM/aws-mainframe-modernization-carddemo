package com.carddemo.programs.cobil00c;

/**
 * Data returned by {@code EXEC CICS RECEIVE MAP('COBIL0A') MAPSET('COBIL00') INTO(COBIL0AI)}.
 * A {@code null} field was not received (CICS sets its input area to X'00'); a non-null field is
 * left-justified and blank padded to the BMS field length (JUSTIFY=(LEFT,BLANK) default).
 * Every COBIL0A field is FSET, so a real terminal returns all of them.
 */
public record BillPayScreenInput(
        String trnname,
        String title01,
        String curdate,
        String pgmname,
        String title02,
        String curtime,
        String actidin,
        String curbal,
        String confirm,
        String errmsg) {

    /** Only the operator-entered fields (account id and confirmation). */
    public static BillPayScreenInput of(String actidin, String confirm) {
        return new BillPayScreenInput(null, null, null, null, null, null, actidin, null, confirm, null);
    }

    /** Operator fields plus the FSET balance previously displayed. */
    public static BillPayScreenInput of(String actidin, String curbal, String confirm) {
        return new BillPayScreenInput(null, null, null, null, null, null, actidin, curbal, confirm, null);
    }

    public static BillPayScreenInput empty() {
        return new BillPayScreenInput(null, null, null, null, null, null, null, null, null, null);
    }
}
