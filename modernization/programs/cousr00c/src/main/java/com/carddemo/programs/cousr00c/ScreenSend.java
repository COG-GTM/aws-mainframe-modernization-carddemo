package com.carddemo.programs.cousr00c;

/** One EXEC CICS SEND MAP('COUSR0A') MAPSET('COUSR00') FROM(COUSR0AO) [ERASE] CURSOR. */
public record ScreenSend(Cousr0aScreen screen, boolean erase) {
}
