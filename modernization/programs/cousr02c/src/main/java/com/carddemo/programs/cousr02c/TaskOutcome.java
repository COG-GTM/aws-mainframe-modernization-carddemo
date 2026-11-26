package com.carddemo.programs.cousr02c;

import com.carddemo.programs.cousr02c.copybook.CardDemoCommarea;

/** How the task ended: {@code EXEC CICS RETURN TRANSID COMMAREA} or {@code EXEC CICS XCTL PROGRAM COMMAREA}. */
public sealed interface TaskOutcome {

    String commareaBytes();

    default CardDemoCommarea commarea() {
        return CardDemoCommarea.fromBytes(commareaBytes());
    }

    record Return(String transId, String commareaBytes) implements TaskOutcome {
    }

    record Xctl(String program, String commareaBytes) implements TaskOutcome {
    }
}
