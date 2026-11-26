package com.carddemo.programs.cousr03c.dto;

/**
 * Data received by EXEC CICS RECEIVE MAP('COUSR3A') for the transmitted fields (all are FSET).
 * A null component means the field was not transmitted (left as LOW-VALUES).
 */
public record ScreenInput(String usrIdIn, String fname, String lname, String usrType) {

    public static ScreenInput userId(String usrIdIn) {
        return new ScreenInput(usrIdIn, null, null, null);
    }
}
