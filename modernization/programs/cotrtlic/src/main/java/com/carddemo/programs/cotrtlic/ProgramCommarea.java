package com.carddemo.programs.cotrtlic;

import com.carddemo.programs.cotrtlic.copybook.CardDemoCommarea;

/** WS-COMMAREA as passed on EXEC CICS RETURN TRANSID: CARDDEMO-COMMAREA followed by WS-THIS-PROGCOMMAREA. */
public record ProgramCommarea(CardDemoCommarea cardDemo, ThisProgCommarea thisProg) {

    /** LENGTH OF WS-COMMAREA. */
    public static final int LENGTH = 2000;

    public ProgramCommarea copy() {
        return new ProgramCommarea(cardDemo.copy(), thisProg.copy());
    }
}
