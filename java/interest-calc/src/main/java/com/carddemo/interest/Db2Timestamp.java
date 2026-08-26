package com.carddemo.interest;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

/**
 * Z-GET-DB2-FORMAT-TIMESTAMP — formats FUNCTION CURRENT-DATE as {@code X(26)}
 * {@code YYYY-MM-DD-HH.MM.SS.hh0000}, where {@code hh} is hundredths of a second and the last four
 * bytes are literal zeroes.
 */
public final class Db2Timestamp {

    private static final DateTimeFormatter FORMAT = DateTimeFormatter.ofPattern("yyyy-MM-dd-HH.mm.ss.SS");

    private Db2Timestamp() {
    }

    public static String format(LocalDateTime timestamp) {
        return FORMAT.format(timestamp) + "0000";
    }
}
