package com.carddemo.programs.cobswait;

import com.carddemo.programs.cobswait.copybook.MvswaitTime;
import com.carddemo.programs.cobswait.copybook.ParmValue;
import java.nio.charset.Charset;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
 *       tabs are ignored; one sign is allowed only as the first non-blank character and is
 *       dropped (unsigned receiver); commas after it are ignored; the fraction after one '.' is
 *       truncated; any other content yields 0.
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
            String s = parm.value().replace(" ", "").replace("\t", "");
            if (!s.isEmpty() && (s.charAt(0) == '+' || s.charAt(0) == '-')) {
                s = s.substring(1);
            }
            Matcher m = GNUCOBOL_NUMBER.matcher(s.replace(",", ""));
            if (!m.matches() || m.group(1).isEmpty()) {
                return MvswaitTime.ZERO;
            }
            return new MvswaitTime(Long.parseLong(m.group(1)));
        }
    };

    /** Unsigned integer digits with an optional '.' and fraction (fraction is truncated). */
    private static final Pattern GNUCOBOL_NUMBER = Pattern.compile("([0-9]*)(?:\\.[0-9]*)?");

    private static final Charset EBCDIC = Charset.forName("IBM1047");

    public abstract MvswaitTime apply(ParmValue parm);
}
