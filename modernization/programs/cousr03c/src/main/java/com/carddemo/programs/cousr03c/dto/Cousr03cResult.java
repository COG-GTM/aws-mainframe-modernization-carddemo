package com.carddemo.programs.cousr03c.dto;

import com.carddemo.programs.cousr03c.Cousr03cCommarea;

import java.util.List;

/**
 * Outcome of one task: either EXEC CICS RETURN TRANSID(...) COMMAREA(...) or
 * EXEC CICS XCTL PROGRAM(...) COMMAREA(...). {@code screens} lists every SEND MAP in order
 * (the terminal shows the last one); {@code displayLines} captures COBOL DISPLAY output.
 */
public record Cousr03cResult(
        Action action,
        String returnTransid,
        String xctlProgram,
        Cousr03cCommarea commarea,
        List<UserDeleteScreen> screens,
        List<String> displayLines) {

    public enum Action { RETURN_TRANSID, XCTL }

    public UserDeleteScreen lastScreen() {
        return screens.isEmpty() ? null : screens.get(screens.size() - 1);
    }
}
