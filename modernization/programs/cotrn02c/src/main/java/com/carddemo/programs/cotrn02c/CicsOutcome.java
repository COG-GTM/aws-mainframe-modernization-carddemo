package com.carddemo.programs.cotrn02c;

import com.carddemo.programs.cotrn02c.copybook.CardDemoCommarea;

/** How a COTRN02C task ended: every path ends in exactly one CICS RETURN or XCTL. */
public sealed interface CicsOutcome {

    CardDemoCommarea commarea();

    /** SEND MAP('COTRN2A') MAPSET('COTRN02') followed by RETURN TRANSID(transId) COMMAREA(...). */
    record SendMap(String mapset, String map, Cotrn2aResponse screen, String transId, CardDemoCommarea commarea)
            implements CicsOutcome {
    }

    /** XCTL PROGRAM(program) COMMAREA(...). */
    record Xctl(String program, CardDemoCommarea commarea) implements CicsOutcome {
    }

    /** RETURN TRANSID(transId) COMMAREA(...) without a screen (end of MAIN-PARA). */
    record Return(String transId, CardDemoCommarea commarea) implements CicsOutcome {
    }
}
