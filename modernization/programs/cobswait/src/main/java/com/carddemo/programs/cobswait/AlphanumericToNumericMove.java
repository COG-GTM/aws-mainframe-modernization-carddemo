package com.carddemo.programs.cobswait;

import com.carddemo.programs.cobswait.copybook.MvswaitTime;
import com.carddemo.programs.cobswait.copybook.ParmValue;
import java.nio.charset.Charset;

/**
 * {@code MOVE PARM-VALUE TO MVSWAIT-TIME}: alphanumeric PIC X(8) to unsigned binary PIC 9(8) COMP.
 *
 * <p>The COBOL standard leaves non-digit content undefined, so the two runtimes differ:
 *
 * <ul>
 *   <li>{@link #IBM_ENTERPRISE}: the sender is treated as an unsigned 8-digit zoned number (PACK,
 *       sign forced to F, CVB). Each byte contributes its EBCDIC digit nibble, so spaces count as
 *       0 and '3600&nbsp;&nbsp;&nbsp;&nbsp;' becomes 36000000. A digit nibble above 9 raises a data
 *       exception (S0C7).
 *   <li>{@link #GNUCOBOL}: behaviour observed with GnuCOBOL 3.1.2 {@code -std=ibm}. Spaces and
 *       commas are ignored, one leading sign is allowed and dropped (unsigned receiver), the
 *       fraction after one '.' is truncated, and any other content yields 0.
 * </ul>
 */
public enum AlphanumericToNumericMove {

    IBM_ENTERPRISE {
        @Override
        public MvswaitTime apply(ParmValue parm) {
            byte[] ebcdic = parm.value().getBytes(EBCDIC);
            long result = 0;
            for (int i = 0; i < ebcdic.length; i++) {
                int digit = ebcdic[i] & 0x0F;
                if (digit > 9) {
                    throw new DataExceptionAbend(
                            "MOVE PARM-VALUE TO MVSWAIT-TIME: invalid digit at offset " + i
                                    + " in '" + parm.value() + "'");
                }
                result = result * 10 + digit;
            }
            return new MvswaitTime(result);
        }
    },

    GNUCOBOL {
        @Override
        public MvswaitTime apply(ParmValue parm) {
            String s = parm.value().replace(" ", "").replace(",", "");
            int i = 0;
            if (!s.isEmpty() && (s.charAt(0) == '+' || s.charAt(0) == '-')) {
                i = 1;
            }
            int intStart = i;
            while (i < s.length() && isDigit(s.charAt(i))) {
                i++;
            }
            String integerPart = s.substring(intStart, i);
            if (i < s.length() && s.charAt(i) == '.') {
                int fracStart = ++i;
                while (i < s.length() && isDigit(s.charAt(i))) {
                    i++;
                }
                if (i == fracStart) {
                    return MvswaitTime.ZERO;
                }
            }
            if (i != s.length() || integerPart.isEmpty()) {
                return MvswaitTime.ZERO;
            }
            return new MvswaitTime(Long.parseLong(integerPart));
        }
    };

    private static final Charset EBCDIC = Charset.forName("IBM1047");

    private static boolean isDigit(char c) {
        return c >= '0' && c <= '9';
    }

    public abstract MvswaitTime apply(ParmValue parm);
}
