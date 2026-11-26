package com.carddemo.programs.cotrn01c;

import java.util.List;

/** How the CICS task ended. */
public sealed interface CicsOutcome {

    Cotrn01cCommarea commarea();

    /** EXEC CICS XCTL PROGRAM(program) COMMAREA(CARDDEMO-COMMAREA). */
    record Xctl(String program, Cotrn01cCommarea commarea) implements CicsOutcome {
    }

    /**
     * EXEC CICS RETURN TRANSID(transId) COMMAREA(CARDDEMO-COMMAREA), after the SEND MAPs issued
     * during the task (COTRN01C may SEND twice in one task; the terminal shows the last one).
     */
    record Return(String transId, Cotrn01cCommarea commarea, List<TrnViewScreen> sentScreens)
            implements CicsOutcome {

        public Return {
            sentScreens = List.copyOf(sentScreens);
        }

        public TrnViewScreen screen() {
            return sentScreens.isEmpty() ? null : sentScreens.get(sentScreens.size() - 1);
        }
    }
}
