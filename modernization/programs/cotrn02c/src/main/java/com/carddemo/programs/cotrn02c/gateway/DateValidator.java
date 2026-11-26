package com.carddemo.programs.cotrn02c.gateway;

import com.carddemo.programs.cotrn02c.copybook.CsutldtcParm;

/** {@code CALL 'CSUTLDTC' USING CSUTLDTC-DATE CSUTLDTC-DATE-FORMAT CSUTLDTC-RESULT}. */
public interface DateValidator {

    /** Returns the parm with CSUTLDTC-RESULT filled in. */
    CsutldtcParm validate(String date, String dateFormat);
}
