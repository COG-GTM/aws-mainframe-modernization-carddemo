package com.carddemo.programs.cbimport.copybook;

import java.math.BigDecimal;

/** Sequential reader over a fixed-width character record (output copybooks are all USAGE DISPLAY). */
final class Slicer {

    private final String line;
    private int pos;

    Slicer(String line, int length) {
        this.line = Pic.alnum(line, length);
    }

    String x(int len) {
        String s = line.substring(pos, pos + len);
        pos += len;
        return s;
    }

    long unsigned(int digits) {
        return Pic.parseUnsigned(x(digits)).longValueExact();
    }

    BigDecimal signed(int intDigits, int scale) {
        return Pic.parseSigned(x(intDigits + scale), scale);
    }
}
