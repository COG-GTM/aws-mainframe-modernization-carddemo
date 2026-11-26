package com.carddemo.programs.cotrn00c;

import java.util.List;

/**
 * How the task ended plus every SEND MAP issued (COTRN00C can SEND more than once per task; the
 * terminal shows the last one) and every DISPLAY line written to the job log.
 */
public record Cotrn00cResponse(
        Outcome outcome,
        String nextTransid,
        String xctlProgram,
        String abendCode,
        Cotrn00cCommarea commarea,
        List<ScreenImage> screens,
        List<String> displayLines) {

    public enum Outcome {
        /** EXEC CICS RETURN TRANSID(...) COMMAREA(...). */
        RETURN,
        /** EXEC CICS XCTL PROGRAM(...) COMMAREA(...). */
        XCTL,
        /** Task abended on an unhandled CICS condition. */
        ABEND
    }

    public ScreenImage lastScreen() {
        return screens.isEmpty() ? null : screens.get(screens.size() - 1);
    }
}
