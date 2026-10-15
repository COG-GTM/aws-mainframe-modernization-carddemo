package com.carddemo.migration;

import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeParseException;
import java.util.LinkedHashMap;
import java.util.Map;

/** Decodes one fixed-length record into column/value pairs ready for JDBC binding. */
public final class RecordDecoder {

    private RecordDecoder() {
    }

    public static Map<String, Object> decode(byte[] record, RecordLayout layout, SourceEncoding encoding) {
        if (record.length != layout.recordLength()) {
            throw new IllegalArgumentException(layout.copybook() + " record must be " + layout.recordLength()
                    + " bytes but was " + record.length);
        }
        Map<String, Object> row = new LinkedHashMap<>();
        for (Field f : layout.fields()) {
            row.put(f.column(), decodeField(record, f, encoding));
        }
        return row;
    }

    static Object decodeField(byte[] record, Field f, SourceEncoding encoding) {
        return switch (f.type()) {
            case PACKED -> CobolCodec.packed(record, f.offset(), f.length(), f.scale());
            case UNSIGNED_ZONED, SIGNED_ZONED -> {
                BigDecimal value = encoding == SourceEncoding.EBCDIC
                        ? CobolCodec.zonedFromEbcdic(record, f.offset(), f.length(), f.scale())
                        : CobolCodec.zonedFromAscii(text(record, f, encoding), f.scale());
                if (f.type() == FieldType.UNSIGNED_ZONED && value.signum() < 0) {
                    throw new IllegalArgumentException(f.copybookName() + " is unsigned but decoded negative");
                }
                yield f.scale() == 0 ? (Object) value.longValueExact() : value;
            }
            case ALPHA -> {
                String s = stripTrailing(text(record, f, encoding));
                yield s.isEmpty() ? null : s;
            }
            case DATE -> parseDate(text(record, f, encoding).trim());
            case TIMESTAMP -> parseTimestamp(text(record, f, encoding).trim());
        };
    }

    private static String text(byte[] record, Field f, SourceEncoding encoding) {
        return encoding == SourceEncoding.EBCDIC
                ? CobolCodec.ebcdicToString(record, f.offset(), f.length())
                : new String(record, f.offset(), f.length(), StandardCharsets.ISO_8859_1);
    }

    private static String stripTrailing(String s) {
        int end = s.length();
        while (end > 0 && (s.charAt(end - 1) == ' ' || s.charAt(end - 1) == '\0')) {
            end--;
        }
        return s.substring(0, end);
    }

    static LocalDate parseDate(String s) {
        if (s.isEmpty() || s.chars().allMatch(c -> c == '0')) {
            return null;
        }
        try {
            return LocalDate.parse(s);
        } catch (DateTimeParseException e) {
            throw new IllegalArgumentException("Invalid date '" + s + "'", e);
        }
    }

    /** Accepts both DB2 (YYYY-MM-DD-HH.MM.SS.ffffff) and display (YYYY-MM-DD HH:MM:SS.ffffff) formats. */
    static LocalDateTime parseTimestamp(String s) {
        if (s.isEmpty()) {
            return null;
        }
        if (s.length() != 26) {
            throw new IllegalArgumentException("Invalid timestamp '" + s + "'");
        }
        String iso = s.substring(0, 10) + "T" + s.substring(11, 13) + ":" + s.substring(14, 16) + ":"
                + s.substring(17, 19) + "." + s.substring(20, 26);
        return LocalDateTime.parse(iso);
    }
}
