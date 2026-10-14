package com.carddemo.runtime.cobol;

import java.time.Clock;
import java.time.ZonedDateTime;

/** COBOL intrinsic functions that need a Java implementation. */
public final class CobolIntrinsics {

    private CobolIntrinsics() {
    }

    /**
     * {@code FUNCTION CURRENT-DATE}: 21 characters {@code YYYYMMDDhhmmssHH+hhmm} (HH = hundredths of a
     * second, followed by the UTC offset sign and hours/minutes) taken from {@code clock}.
     */
    public static String currentDate(Clock clock) {
        ZonedDateTime now = ZonedDateTime.now(clock);
        int offsetSeconds = now.getOffset().getTotalSeconds();
        char sign = offsetSeconds < 0 ? '-' : '+';
        int abs = Math.abs(offsetSeconds);
        return String.format("%04d%02d%02d%02d%02d%02d%02d%c%02d%02d",
                now.getYear(), now.getMonthValue(), now.getDayOfMonth(),
                now.getHour(), now.getMinute(), now.getSecond(), now.getNano() / 10_000_000,
                sign, abs / 3600, (abs % 3600) / 60);
    }
}
