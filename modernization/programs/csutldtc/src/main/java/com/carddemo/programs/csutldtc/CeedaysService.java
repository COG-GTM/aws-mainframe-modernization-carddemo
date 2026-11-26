package com.carddemo.programs.csutldtc;

import com.carddemo.programs.csutldtc.copybook.VString;

/** Gateway for the Language Environment callable service {@code CALL "CEEDAYS"}. */
@FunctionalInterface
public interface CeedaysService {
    CeedaysResult ceedays(VString inputCharDate, VString pictureString);
}
