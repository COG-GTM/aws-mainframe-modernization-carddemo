package com.carddemo.programs.cotrtupc;

import com.carddemo.programs.cotrtupc.copybook.AbendData;

/**
 * EXEC CICS ABEND ABCODE(ABEND-CODE) raised by ABEND-ROUTINE, after ABEND-DATA was sent to the terminal.
 */
public final class CicsAbendException extends RuntimeException {

    private final transient AbendData abendData;

    public CicsAbendException(AbendData abendData, Throwable cause) {
        super("ABEND " + abendData.abendCode() + " in " + abendData.abendCulprit().trim() + ": "
                + abendData.abendMsg().trim(), cause);
        this.abendData = abendData;
    }

    public String abendCode() {
        return abendData.abendCode();
    }

    public AbendData abendData() {
        return abendData;
    }

    /** EXEC CICS SEND FROM(ABEND-DATA) LENGTH(LENGTH OF ABEND-DATA). */
    public String sentText() {
        return abendData.toFixed();
    }
}
