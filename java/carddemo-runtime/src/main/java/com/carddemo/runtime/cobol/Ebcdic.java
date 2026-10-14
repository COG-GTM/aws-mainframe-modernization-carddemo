package com.carddemo.runtime.cobol;

import java.nio.charset.Charset;

/** EBCDIC (code page 037) conversion for the datasets under app/data/EBCDIC. */
public final class Ebcdic {

    public static final Charset CP037 = Charset.forName("IBM037");

    private Ebcdic() {
    }

    /** Decodes EBCDIC text bytes to a String (character data only; packed/binary fields are not text). */
    public static String decode(byte[] ebcdic) {
        return new String(ebcdic, CP037);
    }

    public static byte[] encode(String text) {
        return text.getBytes(CP037);
    }
}
