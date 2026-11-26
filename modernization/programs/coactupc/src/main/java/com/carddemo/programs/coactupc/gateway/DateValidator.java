package com.carddemo.programs.coactupc.gateway;

/** CALL 'CSUTLDTC' (wrapper over Language Environment CEEDAYS) used by EDIT-DATE-LE. */
public interface DateValidator {

    DateValidationResult validate(String date, String format);
}
