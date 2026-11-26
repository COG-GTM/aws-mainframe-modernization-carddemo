package com.carddemo.programs.cocrdupc;

import com.carddemo.programs.cocrdupc.copybook.AbendData;

/** ABEND-ROUTINE: SEND FROM(ABEND-DATA) then EXEC CICS ABEND ABCODE('9999'). */
public final class AbendException extends RuntimeException {

    private final String abcode;
    private final AbendData abendData;

    public AbendException(String abcode, AbendData abendData, Throwable cause) {
        super("ABEND " + abcode + ": " + abendData.encode().strip(), cause);
        this.abcode = abcode;
        this.abendData = abendData;
    }

    public String abcode() {
        return abcode;
    }

    /** The ABEND-DATA block that was sent to the terminal. */
    public AbendData abendData() {
        return abendData;
    }
}
