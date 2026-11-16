package com.carddemo.batch.jobs.creastmt;

import java.math.BigDecimal;
import java.math.BigInteger;

/** COBOL MOVE / STRING semantics used by the CBSTM03A port. */
final class Cobol {

    private Cobol() {
    }

    /** Alphanumeric MOVE: left-justify, space-pad or truncate to {@code len}. */
    static String pic(String value, int len) {
        if (value.length() >= len) {
            return value.substring(0, len);
        }
        return value + " ".repeat(len - value.length());
    }

    /** Unsigned {@code PIC 9(n)} DISPLAY rendering. */
    static String digits(long value, int len) {
        return String.format("%0" + len + "d", value);
    }

    /**
     * Numeric-edited MOVE of a 2-decimal value into {@code 9(n).99-} ({@code zeroSuppress=false})
     * or {@code Z(n).99-} ({@code zeroSuppress=true}). High-order digits that do not fit are
     * truncated, as in COBOL.
     */
    static String edited(BigDecimal value, int intDigits, boolean zeroSuppress) {
        BigInteger unscaled = value.setScale(2, java.math.RoundingMode.DOWN).unscaledValue();
        boolean negative = unscaled.signum() < 0;
        String all = unscaled.abs().toString();
        all = "0".repeat(Math.max(0, intDigits + 2 - all.length())) + all;
        all = all.substring(all.length() - (intDigits + 2));
        String intPart = all.substring(0, intDigits);
        if (zeroSuppress) {
            String stripped = intPart.replaceFirst("^0+", "");
            intPart = " ".repeat(intDigits - stripped.length()) + stripped;
        }
        return intPart + "." + all.substring(intDigits) + (negative ? "-" : " ");
    }

    /** One {@code STRING} source phrase {@code DELIMITED BY delimiter}. */
    static String delimited(String value, String delimiter) {
        int at = value.indexOf(delimiter);
        return at < 0 ? value : value.substring(0, at);
    }
}
