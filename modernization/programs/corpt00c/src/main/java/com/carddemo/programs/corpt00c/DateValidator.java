package com.carddemo.programs.corpt00c;

/** Gateway for {@code CALL 'CSUTLDTC' USING CSUTLDTC-DATE CSUTLDTC-DATE-FORMAT CSUTLDTC-RESULT}. */
public interface DateValidator {

    CsutldtcResult validate(String date, String dateFormat);
}
