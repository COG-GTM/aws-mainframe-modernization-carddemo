package com.carddemo.programs.comen01c;

/** How the CICS task ended. */
public sealed interface ProgramOutcome {

    /** EXEC CICS XCTL PROGRAM(program) [COMMAREA(commarea)]; {@code commarea} is null when none is passed. */
    record Xctl(String program, String commarea) implements ProgramOutcome {
    }

    /** EXEC CICS RETURN TRANSID(transId) COMMAREA(commarea). */
    record Return(String transId, String commarea) implements ProgramOutcome {
    }
}
