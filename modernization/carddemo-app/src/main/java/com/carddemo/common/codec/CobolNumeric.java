package com.carddemo.common.codec;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

/**
 * COBOL numeric storage formats to and from {@link BigDecimal} (ADR-0004).
 *
 * <ul>
 *   <li>Zoned decimal ({@code DISPLAY}) with the sign overpunched on the last digit: {@code {A-I} positive,
 *       {@code }J-R} negative (IBM zones C/D); GnuCOBOL's ASCII {@code p-y} negative form is accepted on read.</li>
 *   <li>Packed decimal ({@code COMP-3}): two digits per byte, sign in the last low nibble. C/A/E/F read as
 *       positive, D/B as negative; written as C/D when signed and F when unsigned.</li>
 *   <li>Binary ({@code COMP}/{@code BINARY}/{@code COMP-4}): big-endian two's complement of the unscaled value,
 *       2/4/8 bytes for 1-4/5-9/10-18 digits, truncated to the PIC digits on store ({@code TRUNC(STD)}).</li>
 * </ul>
 *
 * <p>The {@code encode*} methods implement the COBOL MOVE: excess fraction digits are truncated with
 * {@link RoundingMode#DOWN} (ADR-0005, no {@code ROUNDED} in the estate), excess high-order digits are dropped
 * and an unsigned receiver drops the sign. BigDecimal has no negative zero, so a negative-zero source decodes
 * to zero and zero is always stored with the positive sign.
 */
public final class CobolNumeric {

    static final String POSITIVE_OVERPUNCH = "{ABCDEFGHI";
    static final String NEGATIVE_OVERPUNCH = "}JKLMNOPQR";
    static final String GNUCOBOL_NEGATIVE = "pqrstuvwxy";

    private CobolNumeric() {
    }

    public static BigDecimal decodeZoned(String text, int scale, boolean signed) {
        if (text.isEmpty()) {
            throw new RecordFormatException("empty numeric field");
        }
        StringBuilder digits = new StringBuilder(text.length());
        boolean negative = false;
        int last = text.length() - 1;
        for (int i = 0; i <= last; i++) {
            char c = text.charAt(i);
            if (c >= '0' && c <= '9') {
                digits.append(c);
            } else if (i == last && signed && POSITIVE_OVERPUNCH.indexOf(c) >= 0) {
                digits.append((char) ('0' + POSITIVE_OVERPUNCH.indexOf(c)));
            } else if (i == last && signed && NEGATIVE_OVERPUNCH.indexOf(c) >= 0) {
                digits.append((char) ('0' + NEGATIVE_OVERPUNCH.indexOf(c)));
                negative = true;
            } else if (i == last && signed && GNUCOBOL_NEGATIVE.indexOf(c) >= 0) {
                digits.append((char) ('0' + GNUCOBOL_NEGATIVE.indexOf(c)));
                negative = true;
            } else {
                throw new RecordFormatException("invalid " + (signed ? "signed" : "unsigned")
                        + " zoned decimal '" + text + "' at position " + (i + 1));
            }
        }
        BigInteger unscaled = new BigInteger(digits.toString());
        return new BigDecimal(negative ? unscaled.negate() : unscaled, scale);
    }

    public static String encodeZoned(BigDecimal value, int digits, int scale, boolean signed) {
        BigDecimal stored = truncate(value, digits, scale, signed);
        String text = pad(stored.unscaledValue().abs().toString(), digits);
        if (!signed) {
            return text;
        }
        int lastDigit = text.charAt(digits - 1) - '0';
        char sign = stored.signum() < 0
                ? NEGATIVE_OVERPUNCH.charAt(lastDigit)
                : POSITIVE_OVERPUNCH.charAt(lastDigit);
        return text.substring(0, digits - 1) + sign;
    }

    public static int packedLength(int digits) {
        requireDigits(digits);
        return digits / 2 + 1;
    }

    public static BigDecimal decodePacked(byte[] image, int offset, int length, int scale) {
        StringBuilder digits = new StringBuilder(length * 2);
        boolean negative = false;
        for (int i = 0; i < length; i++) {
            int b = image[offset + i] & 0xFF;
            digits.append(digit(b >>> 4, image, offset, length));
            int low = b & 0x0F;
            if (i < length - 1) {
                digits.append(digit(low, image, offset, length));
            } else {
                negative = switch (low) {
                    case 0x0A, 0x0C, 0x0E, 0x0F -> false;
                    case 0x0B, 0x0D -> true;
                    default -> throw new RecordFormatException("invalid packed-decimal sign nibble "
                            + Integer.toHexString(low).toUpperCase() + " in " + hex(image, offset, length));
                };
            }
        }
        BigInteger unscaled = new BigInteger(digits.toString());
        return new BigDecimal(negative ? unscaled.negate() : unscaled, scale);
    }

    public static void encodePacked(byte[] image, int offset, int length, int digits, int scale,
                                    boolean signed, BigDecimal value) {
        if (packedLength(digits) != length) {
            throw new IllegalArgumentException(digits + " digits need " + packedLength(digits)
                    + " packed bytes, not " + length);
        }
        BigDecimal stored = truncate(value, digits, scale, signed);
        String text = pad(stored.unscaledValue().abs().toString(), length * 2 - 1);
        int sign = !signed ? 0x0F : stored.signum() < 0 ? 0x0D : 0x0C;
        for (int i = 0; i < length; i++) {
            int high = text.charAt(2 * i) - '0';
            int low = i < length - 1 ? text.charAt(2 * i + 1) - '0' : sign;
            image[offset + i] = (byte) ((high << 4) | low);
        }
    }

    public static int binaryLength(int digits) {
        requireDigits(digits);
        if (digits <= 4) {
            return 2;
        }
        return digits <= 9 ? 4 : 8;
    }

    public static BigDecimal decodeBinary(byte[] image, int offset, int length, int scale, boolean signed) {
        byte[] raw = new byte[length];
        System.arraycopy(image, offset, raw, 0, length);
        BigInteger unscaled = signed ? new BigInteger(raw) : new BigInteger(1, raw);
        return new BigDecimal(unscaled, scale);
    }

    public static void encodeBinary(byte[] image, int offset, int length, int digits, int scale,
                                    boolean signed, BigDecimal value) {
        if (binaryLength(digits) != length) {
            throw new IllegalArgumentException(digits + " digits need " + binaryLength(digits)
                    + " binary bytes, not " + length);
        }
        long unscaled = truncate(value, digits, scale, signed).unscaledValue().longValueExact();
        for (int i = length - 1; i >= 0; i--) {
            image[offset + i] = (byte) unscaled;
            unscaled >>= 8;
        }
    }

    /** The value a COBOL MOVE stores in a {@code digits}-digit field with {@code scale} fraction digits. */
    public static BigDecimal truncate(BigDecimal value, int digits, int scale, boolean signed) {
        requireDigits(digits);
        if (scale < 0 || scale > digits) {
            throw new IllegalArgumentException("scale " + scale + " outside 0.." + digits);
        }
        BigDecimal scaled = value.setScale(scale, RoundingMode.DOWN);
        BigInteger fitted = scaled.unscaledValue().abs().mod(BigInteger.TEN.pow(digits));
        if (signed && scaled.signum() < 0) {
            fitted = fitted.negate();
        }
        return new BigDecimal(fitted, scale);
    }

    /** True when the integer part of {@code value} fits the field, i.e. a MOVE loses no high-order digit. */
    public static boolean fits(BigDecimal value, int digits, int scale) {
        return value.setScale(scale, RoundingMode.DOWN).unscaledValue().abs()
                .compareTo(BigInteger.TEN.pow(digits)) < 0;
    }

    private static char digit(int nibble, byte[] image, int offset, int length) {
        if (nibble > 9) {
            throw new RecordFormatException("invalid packed-decimal digit nibble "
                    + Integer.toHexString(nibble).toUpperCase() + " in " + hex(image, offset, length));
        }
        return (char) ('0' + nibble);
    }

    private static String pad(String digits, int width) {
        return "0".repeat(width - digits.length()) + digits;
    }

    private static void requireDigits(int digits) {
        if (digits < 1 || digits > 18) {
            throw new IllegalArgumentException("PIC digits must be 1..18, got " + digits);
        }
    }

    static String hex(byte[] image, int offset, int length) {
        StringBuilder sb = new StringBuilder("X'");
        for (int i = 0; i < length; i++) {
            sb.append(String.format("%02X", image[offset + i] & 0xFF));
        }
        return sb.append('\'').toString();
    }
}
