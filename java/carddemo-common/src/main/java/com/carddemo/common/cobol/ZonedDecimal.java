package com.carddemo.common.cobol;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

/**
 * Zoned decimal (USAGE DISPLAY numeric) codec for EBCDIC and ASCII data.
 *
 * <p>EBCDIC digits are {@code Fx}; a signed picture carries the sign in the zone of the overpunched digit
 * ({@code Cx}/{@code Ax}/{@code Ex}/{@code Fx} positive, {@code Dx}/{@code Bx} negative). ASCII text keeps the
 * overpunch characters produced by an EBCDIC to ASCII text transfer: {@code '{'}, {@code 'A'}-{@code 'I'} for +0..+9
 * and {@code '}'}, {@code 'J'}-{@code 'R'} for -0..-9 ({@code 'p'}-{@code 'y'} is also accepted as negative, the
 * Micro Focus/GnuCOBOL ASCII convention).
 */
public final class ZonedDecimal {

    private static final String ASCII_POSITIVE = "{ABCDEFGHI";
    private static final String ASCII_NEGATIVE = "}JKLMNOPQR";

    private ZonedDecimal() {
    }

    /** Storage bytes: one per digit, plus one for a separate sign. */
    public static int byteLength(int digits, boolean signed, SignMode mode) {
        return digits + (signed && mode.separate() ? 1 : 0);
    }

    public static BigDecimal decode(byte[] buf, int offset, int length, int scale, boolean signed, SignMode mode,
                                    DataEncoding encoding) {
        int first = offset;
        int last = offset + length - 1;
        boolean negative = false;
        if (signed && mode.separate()) {
            int signPos = mode.leading() ? first : last;
            String s = encoding.decode(buf, signPos, 1);
            if (s.equals("-")) {
                negative = true;
            } else if (!s.equals("+")) {
                throw new CobolDataException(invalid(buf, offset, length, encoding, "separate sign '" + s + "'"));
            }
            if (mode.leading()) {
                first++;
            } else {
                last--;
            }
        }
        int overpunch = !signed || mode.separate() ? -1 : mode.leading() ? first : last;
        long acc = 0;
        BigInteger big = null;
        for (int i = first; i <= last; i++) {
            int b = buf[i] & 0xFF;
            int digit;
            if (encoding == DataEncoding.EBCDIC) {
                int zone = b >>> 4;
                digit = b & 0x0F;
                if (digit > 9) {
                    throw new CobolDataException(invalid(buf, offset, length, encoding, "byte " + Integer.toHexString(b)));
                }
                if (i == overpunch) {
                    if (zone == 0xD || zone == 0xB) {
                        negative = true;
                    } else if (zone != 0xC && zone != 0xF && zone != 0xA && zone != 0xE) {
                        throw new CobolDataException(invalid(buf, offset, length, encoding, "sign zone " + Integer.toHexString(zone)));
                    }
                } else if (zone != 0xF) {
                    throw new CobolDataException(invalid(buf, offset, length, encoding, "byte " + Integer.toHexString(b)));
                }
            } else {
                char c = (char) b;
                if (c >= '0' && c <= '9') {
                    digit = c - '0';
                } else if (i == overpunch && ASCII_POSITIVE.indexOf(c) >= 0) {
                    digit = ASCII_POSITIVE.indexOf(c);
                } else if (i == overpunch && ASCII_NEGATIVE.indexOf(c) >= 0) {
                    digit = ASCII_NEGATIVE.indexOf(c);
                    negative = true;
                } else if (i == overpunch && c >= 'p' && c <= 'y') {
                    digit = c - 'p';
                    negative = true;
                } else {
                    throw new CobolDataException(invalid(buf, offset, length, encoding, "character '" + c + "'"));
                }
            }
            if (big == null && i - first >= 18) {
                big = BigInteger.valueOf(acc);
            }
            if (big != null) {
                big = big.multiply(BigInteger.TEN).add(BigInteger.valueOf(digit));
            } else {
                acc = acc * 10 + digit;
            }
        }
        BigInteger unscaled = big != null ? big : BigInteger.valueOf(acc);
        return new BigDecimal(negative ? unscaled.negate() : unscaled, scale);
    }

    /** Decodes ASCII text such as {@code "00000001940{"} (S9(10)V99 = +194.00). */
    public static BigDecimal decodeText(String text, int scale, boolean signed) {
        byte[] bytes = DataEncoding.ASCII.encode(text);
        return decode(bytes, 0, bytes.length, scale, signed, SignMode.TRAILING_OVERPUNCH, DataEncoding.ASCII);
    }

    public static boolean isValid(byte[] buf, int offset, int length, boolean signed, SignMode mode, DataEncoding encoding) {
        try {
            decode(buf, offset, length, 0, signed, mode, encoding);
            return true;
        } catch (CobolDataException e) {
            return false;
        }
    }

    public static byte[] encode(BigDecimal value, int digits, int scale, boolean signed, SignMode mode,
                                DataEncoding encoding) {
        byte[] out = new byte[byteLength(digits, signed, mode)];
        encode(value, digits, scale, signed, mode, encoding, out, 0);
        return out;
    }

    /** Encodes with COBOL MOVE semantics (decimal truncation, magnitude only for unsigned); rejects overflow. */
    public static void encode(BigDecimal value, int digits, int scale, boolean signed, SignMode mode,
                              DataEncoding encoding, byte[] buf, int offset) {
        BigInteger unscaled = value.setScale(scale, RoundingMode.DOWN).unscaledValue();
        boolean negative = signed && unscaled.signum() < 0;
        String text = unscaled.abs().toString();
        if (text.length() > digits) {
            throw new CobolDataException("Value " + value + " overflows zoned picture of " + digits + " digits");
        }
        String padded = "0".repeat(digits - text.length()) + text;
        int pos = offset;
        if (signed && mode == SignMode.LEADING_SEPARATE) {
            buf[pos++] = encoding.encode(negative ? "-" : "+")[0];
        }
        byte[] digitBytes = encoding.encode(padded);
        int overpunch = !signed || mode.separate() ? -1 : mode.leading() ? 0 : digits - 1;
        for (int i = 0; i < digits; i++) {
            int d = padded.charAt(i) - '0';
            if (i == overpunch) {
                buf[pos++] = encoding == DataEncoding.EBCDIC
                        ? (byte) ((negative ? 0xD0 : 0xC0) | d)
                        : (byte) (negative ? ASCII_NEGATIVE : ASCII_POSITIVE).charAt(d);
            } else {
                buf[pos++] = digitBytes[i];
            }
        }
        if (signed && mode == SignMode.TRAILING_SEPARATE) {
            buf[pos] = encoding.encode(negative ? "-" : "+")[0];
        }
    }

    private static String invalid(byte[] buf, int offset, int length, DataEncoding encoding, String what) {
        return "Invalid zoned decimal (" + what + ") in " + encoding + " bytes " + Hex.of(buf, offset, length);
    }
}
