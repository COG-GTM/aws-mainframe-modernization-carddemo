package com.carddemo.batch.jobs.intcalc;

/** The COBOL program's 9999-ABEND-PROGRAM path (CEE3ABD, abend code 999). */
public class IntcalcAbendException extends RuntimeException {

    public IntcalcAbendException(String message) {
        super(message);
    }
}
