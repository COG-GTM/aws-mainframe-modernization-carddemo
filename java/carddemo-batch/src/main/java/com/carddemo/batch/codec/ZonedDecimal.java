package com.carddemo.batch.codec;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * USAGE DISPLAY numeric fields ("zoned decimal"): one ASCII digit per byte, with the sign
 * overpunched into the last byte using the EBCDIC convention the CardDemo sample data carries
 * ({@code '{'}=+0, {@code 'A'..'I'}=+1..+9, {@code '}'}=-0, {@code 'J'..'R'}=-1..-9).
 * GnuCOBOL's native ASCII negative overpunch ({@code 'p'..'y'}) and an unsigned last digit
 * (what COBOL {@code INITIALIZE} leaves behind) are accepted on decode.
 */
public final class ZonedDecimal {

    private static final String POSITIVE_OVERPUNCH = "{ABCDEFGHI";
    private static final String NEGATIVE_OVERPUNCH = "}JKLMNOPQR";

    private ZonedDecimal() {
    }

    /** Decodes {@code len} bytes at {@code off}; the result always carries exactly {@code scale} decimals. */
    public static BigDecimal decode(byte[] buf, int off, int len, int scale, boolean signed) {
        StringBuilder digits = new StringBuilder(len);
        boolean negative = false;
        for (int i = 0; i < len; i++) {
            char c = (char) (buf[off + i] & 0xFF);
            if (i == len - 1 && signed && !Character.isDigit(c)) {
                int p = POSITIVE_OVERPUNCH.indexOf(c);
                int n = NEGATIVE_OVERPUNCH.indexOf(c);
                if (p >= 0) {
                    digits.append((char) ('0' + p));
                } else if (n >= 0) {
                    digits.append((char) ('0' + n));
                    negative = true;
                } else if (c >= 'p' && c <= 'y') {
                    digits.append((char) ('0' + (c - 'p')));
                    negative = true;
                } else {
                    throw new CodecException("bad zoned sign byte '" + c + "' at offset " + (off + i));
                }
            } else if (c >= '0' && c <= '9') {
                digits.append(c);
            } else {
                throw new CodecException("non numeric zoned byte '" + c + "' at offset " + (off + i));
            }
        }
        BigDecimal v = new BigDecimal(new BigInteger(digits.toString()), scale);
        return negative ? v.negate() : v;
    }

    /**
     * Encodes {@code value} into {@code len} digit bytes with {@code scale} implied decimals, truncating
     * high-order digits like a COBOL MOVE does and overpunching the sign into the last byte when
     * {@code signed}. Zero is written as {@code +0} ({@code '{'}), which is what a MOVE produces.
     */
    public static void encode(byte[] buf, int off, int len, int scale, boolean signed, BigDecimal value) {
        BigDecimal scaled = value.setScale(scale, java.math.RoundingMode.DOWN);
        boolean negative = scaled.signum() < 0;
        String digits = scaled.abs().unscaledValue().toString();
        if (digits.length() > len) {
            digits = digits.substring(digits.length() - len);
        }
        StringBuilder sb = new StringBuilder(len);
        for (int i = digits.length(); i < len; i++) {
            sb.append('0');
        }
        sb.append(digits);
        if (signed) {
            int last = sb.charAt(len - 1) - '0';
            sb.setCharAt(len - 1, (negative ? NEGATIVE_OVERPUNCH : POSITIVE_OVERPUNCH).charAt(last));
        }
        for (int i = 0; i < len; i++) {
            buf[off + i] = (byte) sb.charAt(i);
        }
    }

    /** COBOL {@code INITIALIZE}: all digit bytes zero, no sign overpunch. */
    public static void initialize(byte[] buf, int off, int len) {
        for (int i = 0; i < len; i++) {
            buf[off + i] = '0';
        }
    }

    /** Whether the last byte carries a negative overpunch (used for DISPLAY). */
    public static boolean isNegative(byte[] buf, int off, int len) {
        char c = (char) (buf[off + len - 1] & 0xFF);
        return NEGATIVE_OVERPUNCH.indexOf(c) >= 0 || (c >= 'p' && c <= 'y');
    }

    /**
     * Renders the field the way GnuCOBOL {@code DISPLAY}s a signed zoned item: the plain digits followed
     * by a trailing {@code '+'} or {@code '-'} (for an unsigned PIC 9 item the digits alone).
     */
    public static String display(byte[] buf, int off, int len, boolean signed) {
        BigDecimal v = decode(buf, off, len, 0, signed);
        String digits = v.abs().unscaledValue().toString();
        StringBuilder sb = new StringBuilder(len + 1);
        for (int i = digits.length(); i < len; i++) {
            sb.append('0');
        }
        sb.append(digits);
        if (signed) {
            sb.append(isNegative(buf, off, len) ? '-' : '+');
        }
        return sb.toString();
    }
}
