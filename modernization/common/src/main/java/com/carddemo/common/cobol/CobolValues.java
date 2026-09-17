package com.carddemo.common.cobol;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;

/**
 * Conversions between the fixed width representations used in the VSAM records and Java types.
 * Needed by the data migration and by the batch jobs that still read mainframe extracts.
 */
public final class CobolValues {

    /** DB2 timestamp layout produced by {@code Z-GET-DB2-FORMAT-TIMESTAMP}: yyyy-MM-dd-HH.mm.ss.SSSSSS. */
    private static final DateTimeFormatter DB2_TIMESTAMP =
            DateTimeFormatter.ofPattern("yyyy-MM-dd-HH.mm.ss.SSSSSS");

    private CobolValues() {
    }

    /** Trims the blank padding of a {@code PIC X(n)} field; an all blank field is null. */
    public static String trim(String value) {
        if (value == null) {
            return null;
        }
        String trimmed = value.trim();
        return trimmed.isEmpty() ? null : trimmed;
    }

    /** Parses a {@code PIC X(10)} date; unset legacy values (blanks, zeros) become null. */
    public static LocalDate date(String value) {
        String trimmed = trim(value);
        if (trimmed == null) {
            return null;
        }
        try {
            return LocalDate.parse(trimmed);
        } catch (DateTimeParseException ex) {
            return null;
        }
    }

    /** Parses a {@code PIC X(26)} DB2 format timestamp, tolerating ISO input from newer feeds. */
    public static LocalDateTime timestamp(String value) {
        String trimmed = trim(value);
        if (trimmed == null) {
            return null;
        }
        try {
            return LocalDateTime.parse(trimmed, DB2_TIMESTAMP);
        } catch (DateTimeParseException ex) {
            try {
                return LocalDateTime.parse(trimmed.replace(' ', 'T'));
            } catch (DateTimeParseException ignored) {
                return null;
            }
        }
    }

    public static String toDb2Timestamp(LocalDateTime value) {
        return value == null ? null : DB2_TIMESTAMP.format(value);
    }
}
