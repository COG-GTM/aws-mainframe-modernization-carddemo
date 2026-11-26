package com.carddemo.programs.codate01;

import java.util.Locale;

import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;

/** EXEC CICS ASKTIME / FORMATTIME arithmetic: ABSTIME is milliseconds since 1900-01-01 00:00 local time. */
public final class CicsTime {

    public static final LocalDateTime EPOCH = LocalDateTime.of(1900, 1, 1, 0, 0);

    private CicsTime() {
    }

    public static long absTime(LocalDateTime localTime) {
        return ChronoUnit.MILLIS.between(EPOCH, localTime);
    }

    public static LocalDateTime toLocalDateTime(long absTime) {
        return EPOCH.plus(absTime, ChronoUnit.MILLIS);
    }

    /** FORMATTIME MMDDYYYY(..) DATESEP(sep): 'mm-dd-yyyy' (10 bytes). */
    public static String mmddyyyy(long absTime, char dateSep) {
        LocalDateTime t = toLocalDateTime(absTime);
        return String.format(Locale.ROOT, "%02d%c%02d%c%04d", t.getMonthValue(), dateSep, t.getDayOfMonth(), dateSep, t.getYear());
    }

    /** FORMATTIME TIME(..) with the default TIMESEP ':' : 'hh:mm:ss' (8 bytes). */
    public static String time(long absTime, char timeSep) {
        LocalDateTime t = toLocalDateTime(absTime);
        return String.format(Locale.ROOT, "%02d%c%02d%c%02d", t.getHour(), timeSep, t.getMinute(), timeSep, t.getSecond());
    }
}
