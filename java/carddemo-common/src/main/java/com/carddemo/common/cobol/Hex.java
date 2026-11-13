package com.carddemo.common.cobol;

/** Hex dump helper used in diagnostics. */
public final class Hex {

    private Hex() {
    }

    public static String of(byte[] buf, int offset, int length) {
        StringBuilder sb = new StringBuilder(length * 2);
        for (int i = offset; i < offset + length; i++) {
            sb.append(Character.forDigit((buf[i] >>> 4) & 0xF, 16)).append(Character.forDigit(buf[i] & 0xF, 16));
        }
        return sb.toString().toUpperCase();
    }

    public static String of(byte[] buf) {
        return of(buf, 0, buf.length);
    }

    public static byte[] parse(String hex) {
        String clean = hex.replace(" ", "");
        byte[] out = new byte[clean.length() / 2];
        for (int i = 0; i < out.length; i++) {
            out[i] = (byte) Integer.parseInt(clean.substring(i * 2, i * 2 + 2), 16);
        }
        return out;
    }
}
