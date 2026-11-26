package com.carddemo.programs.cousr01c.dto;

import com.carddemo.programs.cousr01c.copybook.CardDemoCommarea;

/** How the task ended. */
public sealed interface CicsResponse {

    CardDemoCommarea commarea();

    /** SEND MAP followed by EXEC CICS RETURN TRANSID(...) COMMAREA(CARDDEMO-COMMAREA). */
    record SendAndReturn(UserAddScreenOutput screen, String transId, CardDemoCommarea commarea)
            implements CicsResponse {
    }

    /** EXEC CICS XCTL PROGRAM(CDEMO-TO-PROGRAM) COMMAREA(CARDDEMO-COMMAREA). */
    record Xctl(String program, CardDemoCommarea commarea) implements CicsResponse {
    }
}
