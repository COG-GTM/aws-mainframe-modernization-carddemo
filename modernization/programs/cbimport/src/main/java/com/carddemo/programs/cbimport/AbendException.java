package com.carddemo.programs.cbimport;

/** Raised by 9999-ABEND-PROGRAM in place of CALL 'CEE3ABD'. */
public class AbendException extends RuntimeException {

    public AbendException(String reason) {
        super(reason);
    }
}
