package com.carddemo.programs.coactupc.copybook;

import java.time.LocalDateTime;

/**
 * CSDAT01Y - WS-DATE-TIME: FUNCTION CURRENT-DATE broken into parts and the MM/DD/YY and
 * HH:MM:SS display forms used in the screen header.
 */
public final class DateTimeWork {

    private final LocalDateTime now;

    public DateTimeWork(LocalDateTime now) {
        this.now = now;
    }

    /** WS-CURDATE-MM-DD-YY. */
    public String curdateMmDdYy() {
        return String.format("%02d/%02d/%02d", now.getMonthValue(), now.getDayOfMonth(), now.getYear() % 100);
    }

    /** WS-CURTIME-HH-MM-SS. */
    public String curtimeHhMmSs() {
        return String.format("%02d:%02d:%02d", now.getHour(), now.getMinute(), now.getSecond());
    }

    /** WS-CURDATE-N (YYYYMMDD). */
    public String curdateYyyymmdd() {
        return String.format("%04d%02d%02d", now.getYear(), now.getMonthValue(), now.getDayOfMonth());
    }
}
