package com.carddemo.programs.cbimport.copybook;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.charset.Charset;
import java.util.HexFormat;

/** Sequential decoder over a fixed-length byte record laid out per a copybook. */
public final class FieldCursor {

    private final byte[] data;
    private final Charset charset;
    private final boolean zonedEbcdic;
    private int pos;

    public FieldCursor(byte[] data, int offset, Charset charset) {
        this.data = data;
        this.charset = charset;
        byte[] zero = "0".getBytes(charset);
        this.zonedEbcdic = zero.length == 1 && (zero[0] & 0xFF) == 0xF0;
        this.pos = offset;
    }

    public int position() {
        return pos;
    }

    /** PIC X(n). */
    public String alnum(int len) {
        String s = new String(data, pos, len, charset);
        pos += len;
        return s;
    }

    /** PIC 9(n) USAGE DISPLAY. */
    public BigInteger zonedUnsigned(int digits) {
        if (!zonedEbcdic) {
            return Pic.parseUnsigned(alnum(digits));
        }
        BigInteger v = ebcdicDigits(digits);
        pos += digits;
        return v;
    }

    /** PIC S9(i)V9(s) USAGE DISPLAY. */
    public BigDecimal zonedSigned(int intDigits, int scale) {
        int len = intDigits + scale;
        if (!zonedEbcdic) {
            return Pic.parseSigned(alnum(len), scale);
        }
        BigInteger magnitude = ebcdicDigits(len);
        int zone = (data[pos + len - 1] >> 4) & 0x0F;
        pos += len;
        boolean negative = zone == 0x0B || zone == 0x0D;
        return new BigDecimal(negative ? magnitude.negate() : magnitude, scale);
    }

    /** EBCDIC zoned decimal: digit is the low nibble of each byte, zone of the last byte is the sign. */
    private BigInteger ebcdicDigits(int len) {
        BigInteger v = BigInteger.ZERO;
        for (int i = 0; i < len; i++) {
            v = v.multiply(BigInteger.TEN).add(BigInteger.valueOf((data[pos + i] & 0x0F) % 10));
        }
        return v;
    }

    /** PIC 9(n) COMP / COMP-4 / BINARY: big-endian unsigned. */
    public BigInteger binaryUnsigned(int digits) {
        int len = binaryLength(digits);
        BigInteger v = new BigInteger(1, slice(len));
        pos += len;
        return v;
    }

    /** PIC S9(i)V9(s) COMP: big-endian two's complement. */
    public BigDecimal binarySigned(int intDigits, int scale) {
        int len = binaryLength(intDigits + scale);
        BigInteger v = new BigInteger(slice(len));
        pos += len;
        return new BigDecimal(v, scale);
    }

    /** PIC [S]9(i)V9(s) COMP-3. Sign nibble X'D' or X'B' is negative; unsigned PICs ignore the sign. */
    public BigDecimal packed(int intDigits, int scale, boolean signed) {
        int len = packedLength(intDigits + scale);
        String hex = HexFormat.of().formatHex(slice(len));
        pos += len;
        BigInteger magnitude = BigInteger.ZERO;
        for (int i = 0; i < hex.length() - 1; i++) {
            int nibble = Character.digit(hex.charAt(i), 16);
            magnitude = magnitude.multiply(BigInteger.TEN).add(BigInteger.valueOf(nibble % 10));
        }
        char sign = hex.charAt(hex.length() - 1);
        boolean negative = signed && (sign == 'd' || sign == 'b');
        return new BigDecimal(negative ? magnitude.negate() : magnitude, scale);
    }

    public byte[] bytes(int len) {
        byte[] b = slice(len);
        pos += len;
        return b;
    }

    public void skip(int len) {
        pos += len;
    }

    private byte[] slice(int len) {
        byte[] b = new byte[len];
        System.arraycopy(data, pos, b, 0, len);
        return b;
    }

    /** Storage size of a binary item: 1-4 digits halfword, 5-9 fullword, 10-18 doubleword. */
    public static int binaryLength(int digits) {
        if (digits <= 4) {
            return 2;
        }
        return digits <= 9 ? 4 : 8;
    }

    public static int packedLength(int digits) {
        return digits / 2 + 1;
    }
}
