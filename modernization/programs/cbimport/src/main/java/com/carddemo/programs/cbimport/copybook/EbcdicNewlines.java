package com.carddemo.programs.cbimport.copybook;

import java.nio.ByteBuffer;
import java.nio.charset.Charset;

/**
 * Java's IBM037 decodes both NL (X'15') and LF (X'25') as U+000A and encodes U+000A as X'15'. PIC X
 * moves copy bytes unchanged, so NL is decoded as U+0085 (which encodes back to X'15') and LF is
 * re-encoded as X'25'.
 */
public final class EbcdicNewlines {

    private static final byte NL = 0x15;
    private static final byte LF = 0x25;

    private EbcdicNewlines() {
    }

    public static boolean collapses(Charset cs) {
        return cs.newEncoder().maxBytesPerChar() == 1f
                && "\n".equals(new String(new byte[] {NL}, cs))
                && "\n".equals(new String(new byte[] {LF}, cs))
                && "\u0085".getBytes(cs).length == 1 && "\u0085".getBytes(cs)[0] == NL;
    }

    public static String decode(byte[] data, int offset, int len, Charset cs, boolean collapses) {
        String s = new String(data, offset, len, cs);
        if (!collapses || s.length() != len) {
            return s;
        }
        char[] chars = s.toCharArray();
        for (int i = 0; i < len; i++) {
            if (data[offset + i] == NL) {
                chars[i] = '\u0085';
            }
        }
        return new String(chars);
    }

    public static void restoreLineFeeds(String text, ByteBuffer encoded) {
        for (int i = 0; i < text.length(); i++) {
            if (text.charAt(i) == '\n') {
                encoded.put(encoded.position() + i, LF);
            }
        }
    }
}
