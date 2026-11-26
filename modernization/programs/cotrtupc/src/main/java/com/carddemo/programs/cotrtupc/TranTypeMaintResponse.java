package com.carddemo.programs.cotrtupc;

import com.carddemo.programs.cotrtupc.copybook.CtrtupaMap;

/**
 * Result of one task: either EXEC CICS RETURN TRANSID(CTTU) COMMAREA(WS-COMMAREA) after sending map
 * CTRTUPA, or EXEC CICS XCTL PROGRAM(...) COMMAREA(CARDDEMO-COMMAREA).
 */
public record TranTypeMaintResponse(Outcome outcome, String transId, String xctlProgram, String commarea,
                                    CtrtupaMap screen) {

    public enum Outcome { RETURN, XCTL }

    public static TranTypeMaintResponse returned(String transId, String commarea, CtrtupaMap screen) {
        return new TranTypeMaintResponse(Outcome.RETURN, transId, null, commarea, screen);
    }

    public static TranTypeMaintResponse xctl(String program, String commarea) {
        return new TranTypeMaintResponse(Outcome.XCTL, null, program, commarea, null);
    }
}
