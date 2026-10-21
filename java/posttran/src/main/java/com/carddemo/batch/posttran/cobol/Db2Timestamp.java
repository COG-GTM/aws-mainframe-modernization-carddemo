package com.carddemo.batch.posttran.cobol;

import java.time.Clock;
import java.time.LocalDateTime;

/** Z-GET-DB2-FORMAT-TIMESTAMP: FUNCTION CURRENT-DATE as {@code YYYY-MM-DD-HH.MM.SS.hh0000}. */
public final class Db2Timestamp {

    private Db2Timestamp() {
    }

    public static String now(Clock clock) {
        LocalDateTime t = LocalDateTime.now(clock);
        return String.format("%04d-%02d-%02d-%02d.%02d.%02d.%02d0000",
                t.getYear(), t.getMonthValue(), t.getDayOfMonth(),
                t.getHour(), t.getMinute(), t.getSecond(), t.getNano() / 10_000_000);
    }
}
