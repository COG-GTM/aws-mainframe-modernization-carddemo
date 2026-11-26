package com.carddemo.programs.cousr00c;

import java.util.List;

/**
 * Outcome of one task: either EXEC CICS RETURN TRANSID(...) COMMAREA(...) or EXEC CICS XCTL
 * PROGRAM(...) COMMAREA(...). {@code sends} lists every SEND MAP in order (the terminal shows
 * the last); {@code displays} holds DISPLAY (SYSOUT) lines.
 */
public record Cousr00cResponse(Kind kind, String target, Cousr00cCommarea commarea, List<ScreenSend> sends,
        List<String> displays) {

    public enum Kind { RETURN, XCTL }

    public Cousr00cResponse {
        sends = List.copyOf(sends);
        displays = List.copyOf(displays);
    }

    public ScreenSend lastSend() {
        return sends.isEmpty() ? null : sends.get(sends.size() - 1);
    }

    public Cousr0aScreen screen() {
        ScreenSend s = lastSend();
        return s == null ? null : s.screen();
    }
}
