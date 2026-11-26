package com.carddemo.programs.cousr02c;

/**
 * Data keyed on the COUSR2A screen and delivered by {@code EXEC CICS RECEIVE MAP('COUSR2A')}: the unprotected
 * fields USRIDIN, FNAME, LNAME, PASSWD and USRTYPE. A null value means the field was not transmitted (its
 * {@code xxxI} becomes LOW-VALUES with length 0); otherwise the value is truncated / space-padded to the field size.
 */
public record MapInput(String usridin, String fname, String lname, String passwd, String usrtype) {

    public static MapInput empty() {
        return new MapInput(null, null, null, null, null);
    }

    public static MapInput userId(String usridin) {
        return new MapInput(usridin, null, null, null, null);
    }
}
