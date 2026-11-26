package com.carddemo.programs.cousr02c;

import java.util.List;

/**
 * Result of one task.
 *
 * @param screens      every SEND MAP issued, in order (the terminal shows the last one, each SEND uses ERASE)
 * @param outcome      RETURN or XCTL
 * @param displayLines DISPLAY output (CICS job log)
 */
public record Cousr02cResponse(List<ScreenOutput> screens, TaskOutcome outcome, List<String> displayLines) {

    /** The screen left on the terminal, or null if nothing was sent. */
    public ScreenOutput finalScreen() {
        return screens.isEmpty() ? null : screens.get(screens.size() - 1);
    }
}
