package com.carddemo.programs.coacct01.support;

import java.nio.charset.Charset;

/**
 * Numeric value of an unsigned PIC 9(n) DISPLAY field as z/OS evaluates it in a numeric
 * comparison: each byte (EBCDIC, CCSID 037) contributes its low-order nibble as a digit; a nibble
 * above 9 raises a data exception (S0C7).
 */
public final class EbcdicNumeric {

    private static final Charset EBCDIC = Charset.forName("IBM037");

    private EbcdicNumeric() {
    }

    public static long value(String display) {
        byte[] bytes = display.getBytes(EBCDIC);
        long result = 0;
        for (byte b : bytes) {
            int digit = b & 0x0F;
            if (digit > 9) {
                throw new DataException("S0C7 invalid digit in numeric field '" + display + "'");
            }
            result = result * 10 + digit;
        }
        return result;
    }
}
