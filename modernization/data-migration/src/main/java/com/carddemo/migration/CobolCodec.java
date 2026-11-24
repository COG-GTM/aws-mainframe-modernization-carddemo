package com.carddemo.migration;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.charset.Charset;

/**
 * Converts COBOL storage formats to Java values.
 *
 * <ul>
 *   <li>EBCDIC character data is decoded with code page 037 (IBM US/Canada), the CardDemo CCSID.</li>
 *   <li>Zoned decimal: one digit per byte; the sign lives in the zone of the last byte. In EBCDIC the zone nibble is
 *       C/F (positive) or D (negative). After ASCII transfer the last byte becomes an overpunch character:
 *       '{', 'A'-'I' = +0..+9 and '}', 'J'-'R' = -0..-9.</li>
 *   <li>Packed decimal (COMP-3): two digits per byte, low nibble of the last byte is the sign (C/F +, D -).</li>
 * </ul>
 */
public final class CobolCodec {

    public static final Charset EBCDIC = Charset.forName("IBM037");

    private static final String POSITIVE_OVERPUNCH = "{ABCDEFGHI";
    private static final String NEGATIVE_OVERPUNCH = "}JKLMNOPQR";

    private CobolCodec() {
    }

    public static String ebcdicToString(byte[] bytes, int offset, int length) {
        return new String(bytes, offset, length, EBCDIC);
    }

    /** Decodes an EBCDIC zoned-decimal field. */
    public static BigDecimal zonedFromEbcdic(byte[] bytes, int offset, int length, int scale) {
        StringBuilder digits = new StringBuilder(length);
        boolean negative = false;
        for (int i = 0; i < length; i++) {
            int b = bytes[offset + i] & 0xFF;
            int zone = b >> 4;
            int digit = b & 0x0F;
            if (b == 0x40) {
                digit = 0;
            } else if (digit > 9) {
                throw new IllegalArgumentException(String.format("Invalid zoned digit 0x%02X", b));
            }
            if (i == length - 1 && zone == 0xD) {
                negative = true;
            }
            digits.append((char) ('0' + digit));
        }
        BigDecimal value = new BigDecimal(new BigInteger(digits.toString()), scale);
        return negative ? value.negate() : value;
    }

    /** Decodes an ASCII zoned-decimal field whose last character may carry an overpunched sign. */
    public static BigDecimal zonedFromAscii(String text, int scale) {
        if (text.isBlank()) {
            return BigDecimal.ZERO.setScale(scale);
        }
        String body = text.substring(0, text.length() - 1).replace(' ', '0');
        char last = text.charAt(text.length() - 1);
        boolean negative = false;
        char digit;
        int pos = POSITIVE_OVERPUNCH.indexOf(last);
        int neg = NEGATIVE_OVERPUNCH.indexOf(last);
        if (pos >= 0) {
            digit = (char) ('0' + pos);
        } else if (neg >= 0) {
            digit = (char) ('0' + neg);
            negative = true;
        } else if (Character.isDigit(last)) {
            digit = last;
        } else if (last == ' ') {
            digit = '0';
        } else {
            throw new IllegalArgumentException("Invalid zoned decimal '" + text + "'");
        }
        String all = body + digit;
        for (int i = 0; i < all.length(); i++) {
            if (!Character.isDigit(all.charAt(i))) {
                throw new IllegalArgumentException("Invalid zoned decimal '" + text + "'");
            }
        }
        BigDecimal value = new BigDecimal(new BigInteger(all), scale);
        return negative ? value.negate() : value;
    }

    /** Decodes a COMP-3 packed-decimal field. */
    public static BigDecimal packed(byte[] bytes, int offset, int length, int scale) {
        StringBuilder digits = new StringBuilder(length * 2);
        for (int i = 0; i < length; i++) {
            int b = bytes[offset + i] & 0xFF;
            int high = b >> 4;
            int low = b & 0x0F;
            checkDigit(high, b);
            digits.append((char) ('0' + high));
            if (i < length - 1) {
                checkDigit(low, b);
                digits.append((char) ('0' + low));
            } else if (low != 0xC && low != 0xD && low != 0xF) {
                throw new IllegalArgumentException(String.format("Invalid packed sign nibble 0x%X", low));
            }
        }
        boolean negative = (bytes[offset + length - 1] & 0x0F) == 0xD;
        BigDecimal value = new BigDecimal(new BigInteger(digits.toString()), scale);
        return negative ? value.negate() : value;
    }

    /** Encodes a value as COMP-3 (used by tests and the reverse-feed fallback during cutover). */
    public static byte[] toPacked(BigDecimal value, int digits, int scale) {
        BigInteger unscaled = value.setScale(scale).unscaledValue();
        String text = unscaled.abs().toString();
        int totalDigits = digits % 2 == 0 ? digits + 1 : digits;
        if (text.length() > totalDigits) {
            throw new IllegalArgumentException("Value " + value + " overflows " + digits + " digits");
        }
        text = "0".repeat(totalDigits - text.length()) + text;
        byte[] out = new byte[(totalDigits + 1) / 2];
        for (int i = 0; i < out.length; i++) {
            int high = text.charAt(i * 2) - '0';
            int low = i == out.length - 1 ? (unscaled.signum() < 0 ? 0xD : 0xC) : text.charAt(i * 2 + 1) - '0';
            out[i] = (byte) ((high << 4) | low);
        }
        return out;
    }

    private static void checkDigit(int nibble, int b) {
        if (nibble > 9) {
            throw new IllegalArgumentException(String.format("Invalid packed digit in byte 0x%02X", b));
        }
    }
}
