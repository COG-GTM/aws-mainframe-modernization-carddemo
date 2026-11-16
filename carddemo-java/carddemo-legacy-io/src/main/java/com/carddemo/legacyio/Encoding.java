package com.carddemo.legacyio;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;

/**
 * Character set of a legacy file. Zoned-decimal signs follow the EBCDIC convention in both: the last digit carries the
 * sign as an overpunch ({@code C}/{@code D} zone in EBCDIC; {@code {}, {@code A}-{@code I} for positive and
 * {@code }}, {@code J}-{@code R} for negative in ASCII, which is what {@code cobc -fsign=EBCDIC} writes and what the
 * {@code app/data/ASCII} files hold).
 *
 * <p>Text is converted through a one-to-one byte table built from the code page, not {@link String#getBytes}: Java's
 * IBM037 decodes both {@code 0x15} and {@code 0x25} to LF, so {@code 0x15} is mapped to NEL (U+0085) as in
 * CCSID 37 and every byte keeps its own character.
 */
public enum Encoding {
    /** Mainframe EBCDIC, code page 037 ({@code app/data/EBCDIC}). */
    EBCDIC(Charset.forName("IBM037"), (byte) 0x40),
    /** ASCII / ISO-8859-1 ({@code app/data/ASCII}); every byte maps to one character, so text round-trips. */
    ASCII(StandardCharsets.ISO_8859_1, (byte) 0x20);

    private static final String ASCII_POSITIVE = "{ABCDEFGHI";
    private static final String ASCII_NEGATIVE = "}JKLMNOPQR";

    private final Charset charset;
    private final byte space;
    private final char[] toChar = new char[256];
    private final Map<Character, Byte> toByte = new HashMap<>();

    Encoding(Charset charset, byte space) {
        this.charset = charset;
        this.space = space;
        for (int b = 0; b < 256; b++) {
            char c = new String(new byte[] {(byte) b}, charset).charAt(0);
            if (b == 0x15 && c == '\n' && charset.name().equals("IBM037")) {
                c = '\u0085';
            }
            toChar[b] = c;
            if (toByte.put(c, (byte) b) != null) {
                throw new IllegalStateException(charset + " maps two bytes to U+" + Integer.toHexString(c));
            }
        }
    }

    /** Decodes bytes one character per byte; every byte value has its own character, so text round-trips. */
    public String decode(byte[] bytes, int offset, int length) {
        char[] chars = new char[length];
        for (int i = 0; i < length; i++) {
            chars[i] = toChar[bytes[offset + i] & 0xFF];
        }
        return new String(chars);
    }

    /** Encodes text one byte per character; throws for a character the code page cannot hold. */
    public byte[] encode(CharSequence text) {
        byte[] bytes = new byte[text.length()];
        for (int i = 0; i < bytes.length; i++) {
            Byte b = toByte.get(text.charAt(i));
            if (b == null) {
                throw new IllegalArgumentException("'" + text.charAt(i) + "' (U+"
                        + Integer.toHexString(text.charAt(i)) + ") has no " + name() + " byte in: " + text);
            }
            bytes[i] = b;
        }
        return bytes;
    }

    public Charset charset() {
        return charset;
    }

    public byte space() {
        return space;
    }

    /** Digit value of a byte in a non-final zoned position, or -1 when the byte is not a plain digit. */
    int plainDigit(byte b) {
        int v = b & 0xFF;
        int zero = this == EBCDIC ? 0xF0 : '0';
        return v >= zero && v <= zero + 9 ? v - zero : -1;
    }

    byte plainDigitByte(int digit) {
        return (byte) ((this == EBCDIC ? 0xF0 : '0') + digit);
    }

    /**
     * Decodes the last byte of a zoned field: {@code digit} in the low bits and the sign in {@code 0x100} (negative)
     * or {@code 0x200} (unsigned zone). Returns -1 for a byte that is not a zoned digit.
     */
    int zonedLast(byte b) {
        int v = b & 0xFF;
        if (this == EBCDIC) {
            int zone = v >> 4;
            int digit = v & 0x0F;
            if (digit > 9) {
                return -1;
            }
            return switch (zone) {
                case 0xF -> digit | 0x200;
                case 0xC, 0xA, 0xE -> digit;
                case 0xD, 0xB -> digit | 0x100;
                default -> -1;
            };
        }
        if (v >= '0' && v <= '9') {
            return (v - '0') | 0x200;
        }
        int positive = ASCII_POSITIVE.indexOf(v);
        if (positive >= 0) {
            return positive;
        }
        int negative = ASCII_NEGATIVE.indexOf(v);
        return negative >= 0 ? negative | 0x100 : -1;
    }

    /** Last byte of a zoned field: unsigned zone when {@code signed} is false, else positive/negative overpunch. */
    byte zonedLastByte(int digit, boolean signed, boolean negative) {
        if (!signed) {
            return plainDigitByte(digit);
        }
        if (this == EBCDIC) {
            return (byte) ((negative ? 0xD0 : 0xC0) | digit);
        }
        return (byte) (negative ? ASCII_NEGATIVE : ASCII_POSITIVE).charAt(digit);
    }
}
