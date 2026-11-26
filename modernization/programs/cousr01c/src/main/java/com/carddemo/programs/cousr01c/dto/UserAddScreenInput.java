package com.carddemo.programs.cousr01c.dto;

/**
 * Data keyed on the COUSR1A screen (unprotected fields FNAME, LNAME, USERID, PASSWD, USRTYPE).
 * A null or empty value means no data was received for the field (BMS delivers low-values);
 * otherwise the text is left-justified and blank-padded / truncated to the BMS field length.
 */
public record UserAddScreenInput(String fname, String lname, String userid, String passwd, String usrtype) {

    public static UserAddScreenInput empty() {
        return new UserAddScreenInput(null, null, null, null, null);
    }
}
