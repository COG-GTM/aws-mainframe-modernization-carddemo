package com.carddemo.interest.io;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * Decoder/encoder for COBOL {@code USAGE DISPLAY} numerics ("zoned decimal"),
 * i.e. {@code PIC 9(n)} and {@code PIC S9(n)V9(m)} fields.
 *
 * <p>Every digit occupies one byte. For signed fields the sign is "overpunched" onto the
 * zone nibble of the low-order digit: {@code 0xC} for positive, {@code 0xD} for negative,
 * {@code 0xF} for unsigned. In EBCDIC that renders as {@code {ABCDEFGHI} for +0..+9 and
 * }JKLMNOPQR} for -0..-9}. The implied decimal point ({@code V}) carries no byte, so the
 * scale must come from the copybook.
 *
 * <p>Spaces inside a numeric field are read as zero. That is deliberate leniency for dumps of
 * never-initialised records; a mainframe {@code COMPUTE} over a blank {@code DISPLAY} field would
 * normally raise a data exception (S0C7) instead. No shipped record exercises it — see open
 * question 6 of {@code modernization/CBACT04C-logic-map.md}.
 */
public final class ZonedDecimalCodec {

    private static final String POSITIVE_OVERPUNCH = "{ABCDEFGHI";
    private static final String NEGATIVE_OVERPUNCH = "}JKLMNOPQR";

    private ZonedDecimalCodec() {
    }

    /**
     * Decodes a zoned decimal field.
     *
     * @param record the raw EBCDIC record
     * @param offset byte offset of the field
     * @param digits number of digit positions (the {@code 9} count in the PIC clause)
     * @param scale  number of digits after the implied decimal point
     */
    public static BigDecimal decode(byte[] record, int offset, int digits, int scale) {
        String text = EbcdicText.toAscii(record, offset, digits);
        StringBuilder unsigned = new StringBuilder(digits);
        boolean negative = false;
        for (int i = 0; i < digits; i++) {
            char c = text.charAt(i);
            boolean last = i == digits - 1;
            if (c >= '0' && c <= '9') {
                unsigned.append(c);
            } else if (last && POSITIVE_OVERPUNCH.indexOf(c) >= 0) {
                unsigned.append((char) ('0' + POSITIVE_OVERPUNCH.indexOf(c)));
            } else if (last && NEGATIVE_OVERPUNCH.indexOf(c) >= 0) {
                unsigned.append((char) ('0' + NEGATIVE_OVERPUNCH.indexOf(c)));
                negative = true;
            } else if (c == ' ') {
                unsigned.append('0');
            } else {
                throw new RecordDecodingException(
                        "Invalid zoned decimal digit '" + c + "' at offset " + (offset + i)
                                + " in field [" + text + "]");
            }
        }
        BigDecimal value = new BigDecimal(new BigInteger(unsigned.toString()), scale);
        return negative ? value.negate() : value;
    }

    /**
     * Encodes a value into a zoned decimal field, truncating high-order digits and low-order
     * fraction digits exactly as a COBOL {@code MOVE} into the target PIC clause would.
     *
     * @param signed whether the PIC clause carries an {@code S} (sign overpunch on the last digit)
     */
    public static void encode(byte[] record, int offset, int digits, int scale, boolean signed, BigDecimal value) {
        BigDecimal fitted = CobolFixedPoint.fit(value, digits - scale, scale);
        boolean negative = fitted.signum() < 0;
        String plain = fitted.abs().movePointRight(scale).toBigInteger().toString();
        if (plain.length() > digits) {
            plain = plain.substring(plain.length() - digits);
        }
        String padded = "0".repeat(digits - plain.length()) + plain;
        String encoded;
        if (signed) {
            int lastDigit = padded.charAt(digits - 1) - '0';
            char overpunch = (negative ? NEGATIVE_OVERPUNCH : POSITIVE_OVERPUNCH).charAt(lastDigit);
            encoded = padded.substring(0, digits - 1) + overpunch;
        } else {
            encoded = padded;
        }
        System.arraycopy(EbcdicText.toEbcdic(encoded), 0, record, offset, digits);
    }
}
