package com.carddemo.programs.copaus1c;

import java.util.List;

/**
 * Everything one COPAUS1C task produced: the maps sent (in order), how control left the program
 * (RETURN TRANSID or XCTL), the COMMAREA passed on, and DISPLAY output.
 */
public record AuthViewResult(List<AuthViewScreen> screens, Control control, Copaus1cCommarea commarea,
                             List<String> displays) {

    /** How the task ended. */
    public sealed interface Control permits ReturnTransid, Xctl {
    }

    /** EXEC CICS RETURN TRANSID(transid) COMMAREA(CARDDEMO-COMMAREA). */
    public record ReturnTransid(String transid) implements Control {
    }

    /** EXEC CICS XCTL PROGRAM(program) COMMAREA(CARDDEMO-COMMAREA). */
    public record Xctl(String program) implements Control {
    }

    public AuthViewScreen lastScreen() {
        return screens.isEmpty() ? null : screens.get(screens.size() - 1);
    }
}
