package com.carddemo.programs.cbimport;

import java.time.Clock;
import java.time.ZonedDateTime;
import java.util.Locale;

/** FUNCTION CURRENT-DATE: 21 characters YYYYMMDDHHMMSShh+hhmm. */
public final class CurrentDate {

    private CurrentDate() {
    }

    public static String of(Clock clock) {
        ZonedDateTime now = ZonedDateTime.now(clock);
        int offsetMinutes = now.getOffset().getTotalSeconds() / 60;
        char sign = offsetMinutes < 0 ? '-' : '+';
        int abs = Math.abs(offsetMinutes);
        return String.format(Locale.ROOT, "%04d%02d%02d%02d%02d%02d%02d%c%02d%02d",
                now.getYear(), now.getMonthValue(), now.getDayOfMonth(),
                now.getHour(), now.getMinute(), now.getSecond(), now.getNano() / 10_000_000,
                sign, abs / 60, abs % 60);
    }
}
