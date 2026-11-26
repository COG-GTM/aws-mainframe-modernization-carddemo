package com.carddemo.programs.coactupc;

import com.carddemo.programs.coactupc.copybook.AbendData;
import com.carddemo.programs.coactupc.copybook.AidKey;
import com.carddemo.programs.coactupc.copybook.CactupaInput;
import com.carddemo.programs.coactupc.copybook.CactupaOutput;
import com.carddemo.programs.coactupc.copybook.CardDemoCommarea;

/**
 * How the task ended: EXEC CICS RETURN TRANSID(...) COMMAREA(...), EXEC CICS XCTL PROGRAM(...)
 * or EXEC CICS ABEND ABCODE(...) after sending ABEND-DATA.
 *
 * @param kind            RETURN, XCTL or ABEND
 * @param transId         TRANSID on RETURN
 * @param program         PROGRAM on XCTL
 * @param commarea        CARDDEMO-COMMAREA passed on RETURN / XCTL
 * @param programCommarea WS-THIS-PROGCOMMAREA appended to the COMMAREA on RETURN
 * @param screen          map sent with SEND MAP (RETURN only)
 * @param abendCode       ABCODE on ABEND
 * @param abendData       ABEND-DATA sent before the ABEND
 */
public record CicsResult(Kind kind, String transId, String program, CardDemoCommarea commarea,
        AcctUpdateCommarea programCommarea, CactupaOutput screen, String abendCode, AbendData abendData) {

    public enum Kind {
        RETURN, XCTL, ABEND
    }

    static CicsResult returned(String transId, CardDemoCommarea commarea, AcctUpdateCommarea programCommarea,
            CactupaOutput screen) {
        return new CicsResult(Kind.RETURN, transId, null, commarea, programCommarea, screen, null, null);
    }

    static CicsResult xctl(String program, CardDemoCommarea commarea) {
        return new CicsResult(Kind.XCTL, null, program, commarea, null, null, null, null);
    }

    static CicsResult abend(String abendCode, AbendData abendData) {
        return new CicsResult(Kind.ABEND, null, null, null, null, null, abendCode, abendData);
    }

    /** The next pseudo-conversational request carrying this RETURN's COMMAREA. */
    public CicsRequest next(AidKey aid, CactupaInput map) {
        if (kind != Kind.RETURN) {
            throw new IllegalStateException("No pseudo-conversational return: " + kind);
        }
        return new CicsRequest(aid, commarea.copy(), programCommarea.copy(), map);
    }
}
