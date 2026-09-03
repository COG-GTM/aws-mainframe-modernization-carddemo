package com.carddemo.mainframe.io;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * Decoder/encoder for COBOL {@code COMP-3} (packed decimal) fields.
 *
 * <p>Two decimal digits are packed per byte; the low-order nibble of the last byte carries the
 * sign ({@code 0xC}/{@code 0xF} positive, {@code 0xD} negative). A {@code PIC S9(n)V9(m) COMP-3}
 * field therefore occupies {@code (n + m) / 2 + 1} bytes. Packed fields are code-page independent:
 * they must be read from the raw bytes and must never be passed through EBCDIC-to-ASCII conversion.
 *
 * <p>The interest sliver's own copybooks (CVTRA01Y, CVTRA02Y, CVACT01Y, CVACT03Y, CVTRA05Y) declare
 * all numerics as {@code USAGE DISPLAY}, so this codec is unused by the CBACT04C parity harness.
 * It is part of the reusable reader because neighbouring CardDemo copybooks (for example the
 * Db2/IMS export layouts) do use {@code COMP-3}, and the next sliver will need it.
 */
public final class PackedDecimalCodec {

    private PackedDecimalCodec() {
    }

    /** Number of bytes occupied by a {@code COMP-3} field holding {@code digits} digits. */
    public static int byteLength(int digits) {
        return digits / 2 + 1;
    }

    /**
     * Decodes a packed-decimal field.
     *
     * @param offset byte offset of the field
     * @param digits number of digit positions declared in the PIC clause
     * @param scale  number of digits after the implied decimal point
     */
    public static BigDecimal decode(byte[] record, int offset, int digits, int scale) {
        int length = byteLength(digits);
        StringBuilder unsigned = new StringBuilder(length * 2);
        boolean negative = false;
        for (int i = 0; i < length; i++) {
            int b = record[offset + i] & 0xFF;
            int high = b >> 4;
            int low = b & 0x0F;
            if (high > 9) {
                throw new RecordDecodingException(
                        "Invalid packed-decimal nibble at offset " + (offset + i));
            }
            unsigned.append((char) ('0' + high));
            if (i < length - 1) {
                if (low > 9) {
                    throw new RecordDecodingException(
                            "Invalid packed-decimal nibble at offset " + (offset + i));
                }
                unsigned.append((char) ('0' + low));
            } else {
                negative = low == 0x0D || low == 0x0B;
            }
        }
        BigDecimal value = new BigDecimal(new BigInteger(unsigned.toString()), scale);
        return negative ? value.negate() : value;
    }

    /** Encodes a value into a packed-decimal field, truncating to the declared PIC capacity. */
    public static void encode(byte[] record, int offset, int digits, int scale, BigDecimal value) {
        BigDecimal fitted = CobolFixedPoint.fit(value, digits - scale, scale);
        String plain = fitted.abs().movePointRight(scale).toBigInteger().toString();
        int nibbles = byteLength(digits) * 2 - 1;
        if (plain.length() > nibbles) {
            plain = plain.substring(plain.length() - nibbles);
        }
        String padded = "0".repeat(nibbles - plain.length()) + plain;
        int length = byteLength(digits);
        for (int i = 0; i < length; i++) {
            int high = padded.charAt(i * 2) - '0';
            int low;
            if (i < length - 1) {
                low = padded.charAt(i * 2 + 1) - '0';
            } else {
                low = fitted.signum() < 0 ? 0x0D : 0x0C;
            }
            record[offset + i] = (byte) ((high << 4) | low);
        }
    }
}
