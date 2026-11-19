package com.carddemo.common.cobol;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

/**
 * The 26-character DB2-style timestamp CardDemo batch programs build from {@code FUNCTION CURRENT-DATE}
 * ({@code CBTRN02C Z-GET-DB2-FORMAT-TIMESTAMP}, cbl:692-703): {@code YYYY-MM-DD-HH.MM.SS.hh0000}, where {@code hh} are the
 * hundredths of a second and the last four characters are the literal {@code '0000'}.
 */
public final class Db2Timestamp {

    private static final DateTimeFormatter SECONDS = DateTimeFormatter.ofPattern("yyyy-MM-dd-HH.mm.ss");

    private Db2Timestamp() {
    }

    public static String format(LocalDateTime now) {
        return now.format(SECONDS) + "." + String.format("%02d", now.getNano() / 10_000_000) + "0000";
    }
}
