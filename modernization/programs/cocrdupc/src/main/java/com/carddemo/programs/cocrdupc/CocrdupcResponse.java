package com.carddemo.programs.cocrdupc;

import com.carddemo.programs.cocrdupc.copybook.CardDemoCommarea;

/**
 * Outcome of a task. RETURN: EXEC CICS RETURN TRANSID('CCUP') COMMAREA(WS-COMMAREA) LENGTH(2000)
 * after a SEND MAP. XCTL: EXEC CICS SYNCPOINT then XCTL PROGRAM(CDEMO-TO-PROGRAM)
 * COMMAREA(CARDDEMO-COMMAREA).
 */
public record CocrdupcResponse(
        Kind kind,
        String transId,
        String program,
        String commarea,
        CardDemoCommarea carddemoCommarea,
        CardUpdateProgramArea programArea,
        CcrdupaScreen screen,
        boolean syncpoint) {

    public enum Kind { RETURN_TRANSID, XCTL }
}
