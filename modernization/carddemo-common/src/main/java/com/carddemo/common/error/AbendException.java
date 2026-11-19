package com.carddemo.common.error;

import com.carddemo.common.batch.ReturnCode;

/**
 * An abnormal end: the Java equivalent of {@code CALL 'CEE3ABD'} (CBTRN02C {@code 9999-ABEND-PROGRAM},
 * ABCODE 999). It is never caught to skip a record; it fails the step and the launch exits with
 * {@link ReturnCode#ABEND} (16).
 */
public class AbendException extends RuntimeException {

    /** ABCODE moved before {@code CALL 'CEE3ABD'} in the CardDemo batch programs. */
    public static final int DEFAULT_ABEND_CODE = 999;

    private final int abendCode;

    public AbendException(String message) {
        this(message, DEFAULT_ABEND_CODE, null);
    }

    public AbendException(String message, Throwable cause) {
        this(message, DEFAULT_ABEND_CODE, cause);
    }

    public AbendException(String message, int abendCode, Throwable cause) {
        super(message, cause);
        this.abendCode = abendCode;
    }

    public int abendCode() {
        return abendCode;
    }

    /** Condition code of the launch that ends with this abend. */
    public int returnCode() {
        return ReturnCode.ABEND;
    }
}
