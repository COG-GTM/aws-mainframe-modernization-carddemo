package com.carddemo.programs.copaus2c.copybook;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.charset.Charset;

/** EBCDIC (CP037) and packed-decimal (COMP-3) helpers for fixed-width mainframe records. */
public final class Ebcdic {

    public static final Charset CHARSET = Charset.forName("IBM037");

    private Ebcdic() {
    }

    public static String text(byte[] rec, int offset, int length) {
        return new String(rec, offset, length, CHARSET);
    }

    public static void putText(byte[] rec, int offset, int length, String value) {
        byte[] b = Pic.x(value, length).getBytes(CHARSET);
        System.arraycopy(b, 0, rec, offset, length);
    }

    /** Unpack a COMP-3 field of {@code length} bytes with {@code scale} implied decimals. */
    public static BigDecimal unpack(byte[] rec, int offset, int length, int scale) {
        StringBuilder digits = new StringBuilder();
        for (int i = 0; i < length; i++) {
            int b = rec[offset + i] & 0xFF;
            digits.append(b >> 4);
            if (i < length - 1) {
                digits.append(b & 0x0F);
            }
        }
        int sign = rec[offset + length - 1] & 0x0F;
        BigInteger unscaled = new BigInteger(digits.toString());
        if (sign == 0x0D || sign == 0x0B) {
            unscaled = unscaled.negate();
        }
        return new BigDecimal(unscaled, scale);
    }

    /** Pack {@code value} into a COMP-3 field of {@code length} bytes (preferred signs C/D). */
    public static void pack(byte[] rec, int offset, int length, int scale, BigDecimal value) {
        BigInteger unscaled = value.setScale(scale, java.math.RoundingMode.DOWN).unscaledValue();
        int nDigits = length * 2 - 1;
        String digits = unscaled.abs().toString();
        if (digits.length() > nDigits) {
            digits = digits.substring(digits.length() - nDigits);
        }
        digits = "0".repeat(nDigits - digits.length()) + digits + (unscaled.signum() < 0 ? "D" : "C");
        for (int i = 0; i < length; i++) {
            rec[offset + i] = (byte) Integer.parseInt(digits.substring(i * 2, i * 2 + 2), 16);
        }
    }
}
