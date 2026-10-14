package com.carddemo.runtime.cobol;

/**
 * How IBM Enterprise COBOL evaluates an unsigned PIC 9(n) DISPLAY item that does not hold valid digits (typically a
 * PIC 9 REDEFINES of a PIC X field, e.g. {@code CC-ACCT-ID-N} or {@code WS-EDIT-DATE-MM-N}): the generated PACK
 * keeps the low-order nibble of every byte (EBCDIC) and the unsigned sign fix-up makes the last byte valid, so
 * spaces and LOW-VALUES count as 0 and {@code ' 1'} is 1 while {@code '1 '} is 10. A nibble above 9 would raise a
 * data exception (S0C7).
 */
public final class IbmDisplayNumeric {

    private IbmDisplayNumeric() {
    }

    /** Numeric value of an unsigned DISPLAY item; throws {@link NumberFormatException} for an S0C7 condition. */
    public static long unsignedValue(String raw) {
        byte[] ebcdic = Ebcdic.encode(raw);
        long v = 0;
        for (byte b : ebcdic) {
            int nibble = b & 0x0F;
            if (nibble > 9) {
                throw new NumberFormatException("Data exception (S0C7) evaluating '" + raw + "'");
            }
            v = v * 10 + nibble;
        }
        return v;
    }

    /**
     * MOVE of an alphanumeric item to an unsigned PIC 9 DISPLAY item of the same length: bytes are copied and the
     * zone of the last byte is forced to F (so a trailing space becomes '0').
     */
    public static String moveAlphanumericToUnsigned(String raw) {
        if (raw.isEmpty()) {
            return raw;
        }
        char last = raw.charAt(raw.length() - 1);
        if (last >= '0' && last <= '9') {
            return raw;
        }
        int nibble = Ebcdic.encode(String.valueOf(last))[0] & 0x0F;
        char fixed = nibble <= 9 ? (char) ('0' + nibble) : last;
        return raw.substring(0, raw.length() - 1) + fixed;
    }
}
