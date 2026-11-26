package com.carddemo.programs.copaus1c.gateway;

import com.carddemo.programs.copaus1c.FraudCommarea;

/** EXEC CICS LINK PROGRAM(program) COMMAREA(WS-FRAUD-DATA) NOHANDLE. */
public interface FraudLinkGateway {

    /**
     * Invoke the linked program, which may update {@code commarea} in place.
     *
     * @return true when EIBRESP = DFHRESP(NORMAL)
     */
    boolean link(String program, FraudCommarea commarea);
}
