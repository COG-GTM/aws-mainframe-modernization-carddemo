package com.carddemo.batch.record;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;

/** Fixed-width field helpers for the legacy record layouts. */
public final class Fixed {

    /** DB2 timestamp {@code YYYY-MM-DD-HH.MM.SS.NNNNNN} as built by {@code Z-GET-DB2-FORMAT-TIMESTAMP}. */
    public static final DateTimeFormatter DB2_TS = DateTimeFormatter.ofPattern("yyyy-MM-dd-HH.mm.ss.SSSSSS");
    /** Timestamp form used in {@code app/data/ASCII/dailytran.txt}. */
    public static final DateTimeFormatter ISO_SPACE_TS = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSSSSS");

    private Fixed() {
    }

    /** Normalizes a raw input line: strips a trailing CR, right-pads to {@code length}; rejects longer lines. */
    public static String normalize(String line, int length) {
        String l = line.endsWith("\r") ? line.substring(0, line.length() - 1) : line;
        if (l.length() > length) {
            throw new IllegalArgumentException("Record longer than " + length + " bytes: " + l.length());
        }
        return pad(l, length);
    }

    /** 1-based COBOL-style field slice. */
    public static String field(String rec, int start, int length) {
        return rec.substring(start - 1, start - 1 + length);
    }

    public static String rtrim(String s) {
        int end = s.length();
        while (end > 0 && s.charAt(end - 1) == ' ') {
            end--;
        }
        return s.substring(0, end);
    }

    /** Trailing-space-trimmed text, {@code null} when blank (VARCHAR load rule). */
    public static String text(String s) {
        String t = rtrim(s);
        return t.isEmpty() ? null : t;
    }

    /** Left-justified, space-padded, truncated to {@code length} (alphanumeric MOVE). */
    public static String pad(String s, int length) {
        String v = s == null ? "" : s;
        if (v.length() >= length) {
            return v.substring(0, length);
        }
        return v + " ".repeat(length - v.length());
    }

    public static LocalDate date(String s) {
        String t = s.trim();
        if (t.isEmpty() || t.chars().allMatch(c -> c == '0')) {
            return null;
        }
        return LocalDate.parse(t);
    }

    public static String date(LocalDate d) {
        return d == null ? " ".repeat(10) : d.toString();
    }

    public static LocalDateTime timestamp(String s) {
        String t = s.trim();
        if (t.isEmpty() || t.chars().allMatch(c -> c == '0')) {
            return null;
        }
        try {
            return LocalDateTime.parse(t, DB2_TS);
        } catch (DateTimeParseException e) {
            return LocalDateTime.parse(t, ISO_SPACE_TS);
        }
    }

    public static String timestamp(LocalDateTime ts, DateTimeFormatter format) {
        return ts == null ? " ".repeat(26) : ts.format(format);
    }
}
