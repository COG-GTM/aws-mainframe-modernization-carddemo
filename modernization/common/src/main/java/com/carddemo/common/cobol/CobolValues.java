package com.carddemo.common.cobol;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.Arrays;

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

    /**
     * Stores {@code value} into a {@code PIC S9(n)V(scale)} target the way a COBOL MOVE, ADD or
     * COMPUTE without {@code ROUNDED} does: excess decimals are dropped toward zero.
     */
    public static BigDecimal truncate(BigDecimal value, int scale) {
        return value == null ? null : value.setScale(scale, RoundingMode.DOWN);
    }

    /** {@code COMPUTE target = dividend / divisor} into a target with {@code scale} decimals, no ROUNDED. */
    public static BigDecimal divide(BigDecimal dividend, BigDecimal divisor, int scale) {
        return dividend.divide(divisor, scale, RoundingMode.DOWN);
    }

    /** Right-pads (or cuts) a value to the width of a {@code PIC X(n)} field for fixed-width output. */
    public static String padRight(String value, int width) {
        String text = value == null ? "" : value;
        return text.length() >= width ? text.substring(0, width) : text + " ".repeat(width - text.length());
    }

    /** Renders a {@code PIC 9(n)} value with its leading zeros, as the COBOL record and screens show it. */
    public static String zeroPad(long value, int digits) {
        if (value < 0) {
            throw new IllegalArgumentException("PIC 9(" + digits + ") is unsigned: " + value);
        }
        String text = Long.toString(value);
        if (text.length() > digits) {
            throw new IllegalArgumentException(value + " does not fit PIC 9(" + digits + ")");
        }
        return "0".repeat(digits - text.length()) + text;
    }

    /**
     * Decodes a zoned decimal {@code PIC S9(n)V(scale)} field from the ASCII extracts in
     * {@code app/data/ASCII}: the sign is overpunched on the last digit ({@code '{'}, {@code 'A'}-{@code 'I'}
     * positive; {@code '}'}, {@code 'J'}-{@code 'R'} negative). Plain digits are unsigned
     * ({@code PIC 9(n)}) or positive.
     */
    public static BigDecimal decodeZoned(String field, int scale) {
        if (field == null || field.isBlank()) {
            throw new IllegalArgumentException("Empty zoned decimal field");
        }
        int last = field.length() - 1;
        char sign = field.charAt(last);
        boolean negative;
        char digit;
        if (sign >= '0' && sign <= '9') {
            negative = false;
            digit = sign;
        } else if (sign == '{') {
            negative = false;
            digit = '0';
        } else if (sign >= 'A' && sign <= 'I') {
            negative = false;
            digit = (char) ('1' + sign - 'A');
        } else if (sign == '}') {
            negative = true;
            digit = '0';
        } else if (sign >= 'J' && sign <= 'R') {
            negative = true;
            digit = (char) ('1' + sign - 'J');
        } else {
            throw new IllegalArgumentException("Invalid overpunch sign '" + sign + "' in " + field);
        }
        String digits = field.substring(0, last) + digit;
        if (!digits.chars().allMatch(Character::isDigit)) {
            throw new IllegalArgumentException("Non-numeric zoned decimal " + field);
        }
        BigDecimal value = new BigDecimal(new BigInteger(digits), scale);
        return negative ? value.negate() : value;
    }

    /** Encodes a value as an ASCII overpunched {@code PIC S9(integerDigits)V(scale)} field (truncating). */
    public static String encodeZoned(BigDecimal value, int integerDigits, int scale) {
        BigDecimal truncated = truncate(value, scale);
        String digits = truncated.unscaledValue().abs().toString();
        int width = integerDigits + scale;
        if (digits.length() > width) {
            throw new IllegalArgumentException(value + " does not fit PIC S9(" + integerDigits + ")V9(" + scale + ")");
        }
        digits = "0".repeat(width - digits.length()) + digits;
        int lastDigit = digits.charAt(width - 1) - '0';
        char sign = truncated.signum() < 0
                ? (lastDigit == 0 ? '}' : (char) ('J' + lastDigit - 1))
                : (lastDigit == 0 ? '{' : (char) ('A' + lastDigit - 1));
        return digits.substring(0, width - 1) + sign;
    }

    /**
     * Decodes a {@code COMP-3} (packed decimal) field: two digits per byte, the low nibble of the last
     * byte is the sign ({@code 0xD} / {@code 0xB} negative, {@code 0xC} / {@code 0xF} / {@code 0xA} / {@code 0xE} positive).
     */
    public static BigDecimal decodePacked(byte[] field, int scale) {
        StringBuilder digits = new StringBuilder(field.length * 2);
        for (int i = 0; i < field.length; i++) {
            int high = (field[i] >> 4) & 0x0F;
            int low = field[i] & 0x0F;
            digits.append(nibble(high, field));
            if (i < field.length - 1) {
                digits.append(nibble(low, field));
            } else if (low < 0x0A) {
                throw new IllegalArgumentException("Invalid packed decimal sign nibble " + low);
            }
        }
        int signNibble = field[field.length - 1] & 0x0F;
        BigDecimal value = new BigDecimal(new BigInteger(digits.toString()), scale);
        return signNibble == 0x0D || signNibble == 0x0B ? value.negate() : value;
    }

    private static char nibble(int value, byte[] field) {
        if (value > 9) {
            throw new IllegalArgumentException("Invalid packed decimal digit nibble " + value);
        }
        return (char) ('0' + value);
    }

    /**
     * Decodes a {@code COMP} / {@code BINARY} field: big-endian two's complement when signed
     * ({@code PIC S9}), unsigned otherwise.
     */
    public static BigDecimal decodeBinary(byte[] field, boolean signed, int scale) {
        BigInteger unscaled = signed ? new BigInteger(field) : new BigInteger(1, field);
        return new BigDecimal(unscaled, scale);
    }

    /**
     * Encodes a {@code COMP-3} field of {@code length} bytes: sign nibble {@code C}/{@code D} when signed,
     * {@code F} when unsigned; excess decimals are dropped toward zero.
     */
    public static byte[] encodePacked(BigDecimal value, int scale, int length, boolean signed) {
        BigDecimal truncated = truncate(value, scale);
        if (!signed && truncated.signum() < 0) {
            throw new IllegalArgumentException(value + " is negative, field is unsigned");
        }
        String digits = truncated.unscaledValue().abs().toString();
        int capacity = length * 2 - 1;
        if (digits.length() > capacity) {
            throw new IllegalArgumentException(value + " does not fit " + length + " packed bytes");
        }
        digits = "0".repeat(capacity - digits.length()) + digits;
        int sign = !signed ? 0x0F : truncated.signum() < 0 ? 0x0D : 0x0C;
        byte[] field = new byte[length];
        for (int i = 0; i < length; i++) {
            int high = digits.charAt(2 * i) - '0';
            int low = i == length - 1 ? sign : digits.charAt(2 * i + 1) - '0';
            field[i] = (byte) ((high << 4) | low);
        }
        return field;
    }

    /**
     * Encodes a {@code COMP} / {@code BINARY} field of {@code length} bytes, big-endian, two's complement
     * when signed; excess decimals are dropped toward zero.
     */
    public static byte[] encodeBinary(BigDecimal value, int scale, int length, boolean signed) {
        BigInteger unscaled = truncate(value, scale).unscaledValue();
        if (!signed && unscaled.signum() < 0) {
            throw new IllegalArgumentException(value + " is negative, field is unsigned");
        }
        int maxBits = signed ? length * 8 - 1 : length * 8;
        if (unscaled.bitLength() > maxBits) {
            throw new IllegalArgumentException(value + " does not fit " + length + " binary bytes");
        }
        byte[] minimal = unscaled.toByteArray();
        byte[] field = new byte[length];
        byte fill = (byte) (unscaled.signum() < 0 ? 0xFF : 0x00);
        Arrays.fill(field, fill);
        int copy = Math.min(minimal.length, length);
        System.arraycopy(minimal, minimal.length - copy, field, length - copy, copy);
        return field;
    }
}
