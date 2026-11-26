package com.carddemo.programs.coadm01c;

import com.carddemo.programs.coadm01c.copybook.CardDemoCommarea;

/** CICS program control: {@code EXEC CICS XCTL PROGRAM(..) [COMMAREA(..)]}. */
public interface ProgramControlGateway {

    /**
     * Transfer control to {@code program}. On success control never returns to the caller's logic.
     *
     * @param commarea the COMMAREA passed, or {@code null} when XCTL is issued without one
     * @throws PgmIdErrException when the program is not installed (raises the PGMIDERR condition)
     */
    void xctl(String program, CardDemoCommarea commarea) throws PgmIdErrException;
}
