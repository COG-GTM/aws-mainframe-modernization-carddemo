package com.carddemo.programs.cotrtlic.gateway;

import com.carddemo.programs.cotrtlic.copybook.Sqlca;

/** CALL 'DSNTIAC' (CICS variant of DSNTIAR): formats the SQLCA into fixed-length message lines. */
public interface Dsntiac {

    /**
     * @param messageLength WS-DSNTIAC-MESG-LEN (size of the caller's message area)
     * @param lrecl         WS-DSNTIAC-LRECL (length of each line)
     */
    Result format(Sqlca sqlca, int messageLength, int lrecl);

    /** @param returnCode RETURN-CODE; {@code text} is null when DSNTIAC wrote nothing. */
    record Result(int returnCode, String text) {
    }
}
