package com.carddemo.transaction.io;

import java.time.Clock;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

/**
 * Port of {@code Z-GET-DB2-FORMAT-TIMESTAMP}: FUNCTION CURRENT-DATE reformatted as
 * {@code YYYY-MM-DD-HH.MM.SS.hh0000} (COB-MIL holds hundredths of a second; DB2-REST is '0000').
 */
public final class Db2Timestamp {

    private static final DateTimeFormatter FORMAT = DateTimeFormatter.ofPattern("yyyy-MM-dd-HH.mm.ss.SS");

    private Db2Timestamp() {
    }

    public static String now(Clock clock) {
        return format(LocalDateTime.now(clock));
    }

    public static String format(LocalDateTime time) {
        return FORMAT.format(time) + "0000";
    }
}
