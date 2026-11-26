package com.carddemo.programs.cosgn00c;

import com.carddemo.programs.cosgn00c.copybook.CardDemoCommarea;
import com.carddemo.programs.cosgn00c.copybook.Cosgn0aOutput;

/** How a COSGN00C task ended. */
public sealed interface SignonOutcome {

    /**
     * SEND MAP('COSGN0A') ERASE CURSOR, then EXEC CICS RETURN TRANSID(CC00) COMMAREA(CARDDEMO-COMMAREA).
     *
     * @param errorFlag value of WS-ERR-FLG when the task ended (88 ERR-FLG-ON)
     */
    record ScreenSent(Cosgn0aOutput map, String message, boolean errorFlag,
                      String returnTransId, CardDemoCommarea commarea) implements SignonOutcome {
    }

    /** SEND TEXT FROM(WS-MESSAGE) LENGTH(80) ERASE FREEKB, then EXEC CICS RETURN (conversation ends). */
    record TextSent(String text) implements SignonOutcome {
    }

    /** EXEC CICS XCTL PROGRAM(program) COMMAREA(CARDDEMO-COMMAREA). */
    record ControlTransferred(String program, CardDemoCommarea commarea) implements SignonOutcome {
    }
}
