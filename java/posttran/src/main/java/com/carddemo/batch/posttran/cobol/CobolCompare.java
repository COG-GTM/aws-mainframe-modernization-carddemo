package com.carddemo.batch.posttran.cobol;

import java.util.Arrays;

/** Alphanumeric comparison in the mainframe (EBCDIC) collating sequence. */
public final class CobolCompare {

    private CobolCompare() {
    }

    /** IF a >= b for two PIC X operands; the shorter operand is space-padded. */
    public static int compareAlphanumeric(String a, String b) {
        int len = Math.max(a.length(), b.length());
        byte[] x = Pic.x(a, len).getBytes(RecordFormat.EBCDIC.charset());
        byte[] y = Pic.x(b, len).getBytes(RecordFormat.EBCDIC.charset());
        return Arrays.compareUnsigned(x, y);
    }
}
