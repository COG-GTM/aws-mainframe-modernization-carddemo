package com.carddemo.seed;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;

/** Cursor over one fixed-width legacy record (ASCII layout of the VSAM copybooks). */
final class FixedRecord {

    private static final DateTimeFormatter DB2_TS = DateTimeFormatter.ofPattern("yyyy-MM-dd-HH.mm.ss.SSSSSS");
    private static final DateTimeFormatter SPACE_TS = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSSSSS");

    private final String line;
    private int pos;

    FixedRecord(String rawLine, int recordLength) {
        String stripped = rawLine.endsWith("\r") ? rawLine.substring(0, rawLine.length() - 1) : rawLine;
        if (stripped.length() > recordLength) {
            throw new IllegalArgumentException("Record longer than " + recordLength + ": " + stripped.length());
        }
        this.line = String.format("%-" + recordLength + "s", stripped);
    }

    String raw(int length) {
        String value = line.substring(pos, pos + length);
        pos += length;
        return value;
    }

    String text(int length) {
        String value = raw(length).stripTrailing();
        return value.isEmpty() ? null : value;
    }

    String textOrEmpty(int length) {
        return raw(length).stripTrailing();
    }

    long unsigned(int length) {
        String value = raw(length).strip();
        return value.isEmpty() ? 0 : Long.parseLong(value);
    }

    /** Zoned decimal with an ASCII overpunch sign in the last position ({, A-I positive; }, J-R negative). */
    BigDecimal signed(int length, int scale) {
        String value = raw(length);
        char last = value.charAt(length - 1);
        int digit;
        boolean negative = false;
        if (last == '{') {
            digit = 0;
        } else if (last >= 'A' && last <= 'I') {
            digit = last - 'A' + 1;
        } else if (last == '}') {
            digit = 0;
            negative = true;
        } else if (last >= 'J' && last <= 'R') {
            digit = last - 'J' + 1;
            negative = true;
        } else if (Character.isDigit(last)) {
            digit = last - '0';
        } else {
            throw new IllegalArgumentException("Invalid overpunch sign '" + last + "'");
        }
        String digits = value.substring(0, length - 1).replace(' ', '0') + digit;
        BigDecimal number = new BigDecimal(digits).movePointLeft(scale);
        return negative ? number.negate() : number;
    }

    LocalDate date(int length) {
        String value = raw(length).strip();
        if (value.isEmpty() || value.chars().allMatch(c -> c == '0' || c == '-')) {
            return null;
        }
        try {
            return LocalDate.parse(value);
        } catch (DateTimeParseException ex) {
            return null;
        }
    }

    LocalDateTime timestamp(int length) {
        String value = raw(length).strip();
        if (value.isEmpty()) {
            return null;
        }
        try {
            return LocalDateTime.parse(value, value.charAt(10) == ' ' ? SPACE_TS : DB2_TS);
        } catch (DateTimeParseException | StringIndexOutOfBoundsException ex) {
            return null;
        }
    }
}
