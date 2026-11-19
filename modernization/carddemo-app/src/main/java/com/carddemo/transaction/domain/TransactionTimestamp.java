package com.carddemo.transaction.domain;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.time.format.ResolverStyle;
import java.util.List;
import java.util.Optional;

/**
 * TRAN-ORIG-TS / TRAN-PROC-TS codec (D-015). The 26-byte text is the record of truth and is written back unchanged;
 * the parsed value is a query aid. TRANSACT writers use two layouts for either field:
 * <ul>
 *   <li>{@link #DISPLAY} {@code yyyy-MM-dd HH:mm:ss.SSSSSS} (DALYTRAN samples, COBIL00C via CSDAT01Y)</li>
 *   <li>{@link #DB2} {@code yyyy-MM-dd-HH.mm.ss.SSSSSS} (CBTRN02C/CBACT04C Z-GET-DB2-FORMAT-TIMESTAMP)</li>
 * </ul>
 */
public final class TransactionTimestamp {

    public static final int LENGTH = 26;

    public static final DateTimeFormatter DISPLAY = strict("uuuu-MM-dd HH:mm:ss.SSSSSS");
    public static final DateTimeFormatter DB2 = strict("uuuu-MM-dd-HH.mm.ss.SSSSSS");

    private static final DateTimeFormatter DB2_HUNDREDTHS = strict("uuuu-MM-dd-HH.mm.ss.SS");
    private static final List<DateTimeFormatter> FORMATS = List.of(DISPLAY, DB2);

    private TransactionTimestamp() {
    }

    /** Parses either layout; empty when the text is not exactly one of them (e.g. spaces or a bad date). */
    public static Optional<LocalDateTime> parse(String text) {
        if (text == null || text.length() != LENGTH) {
            return Optional.empty();
        }
        for (DateTimeFormatter format : FORMATS) {
            try {
                return Optional.of(LocalDateTime.parse(text, format));
            } catch (DateTimeParseException ignored) {
                // try the next layout
            }
        }
        return Optional.empty();
    }

    /** CBTRN02C TRAN-PROC-TS: DB2 layout with hundredths followed by {@code 0000} ({@code CBTRN02C.cbl:692-705}). */
    public static String processingText(LocalDateTime time) {
        return DB2_HUNDREDTHS.format(time) + "0000";
    }

    private static DateTimeFormatter strict(String pattern) {
        return DateTimeFormatter.ofPattern(pattern).withResolverStyle(ResolverStyle.STRICT);
    }
}
