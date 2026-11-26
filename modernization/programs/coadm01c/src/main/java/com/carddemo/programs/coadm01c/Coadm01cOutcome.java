package com.carddemo.programs.coadm01c;

import com.carddemo.programs.coadm01c.copybook.CardDemoCommarea;
import com.carddemo.programs.coadm01c.copybook.Coadm1aOutput;

/** How a COADM01C task ended. */
public sealed interface Coadm01cOutcome {

    /** {@code EXEC CICS XCTL PROGRAM(program) [COMMAREA(commarea)]}; {@code commarea} is null when none passed. */
    record Transferred(String program, CardDemoCommarea commarea) implements Coadm01cOutcome {
    }

    /** {@code EXEC CICS SEND MAP('COADM1A') ERASE} then {@code EXEC CICS RETURN TRANSID(transId) COMMAREA(..)}. */
    record Returned(String transId, CardDemoCommarea commarea, Coadm1aOutput screen) implements Coadm01cOutcome {
    }
}
