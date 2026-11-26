package com.carddemo.programs.cbstm03b.io;

import java.nio.charset.Charset;
import java.util.Comparator;

/**
 * VSAM KSDS key order and DFSORT CH order: unsigned byte comparison of the EBCDIC (code page 037)
 * encoding, so lowercase &lt; uppercase &lt; digits, unlike ASCII.
 */
public final class EbcdicCollation {

    public static final Charset CP037 = Charset.forName("IBM037");

    public static final Comparator<String> ORDER = EbcdicCollation::compare;

    private EbcdicCollation() {
    }

    public static int compare(String a, String b) {
        int c = java.util.Arrays.compareUnsigned(a.getBytes(CP037), b.getBytes(CP037));
        // Characters outside CP037 all encode to the same substitute byte; keep such keys distinct.
        return c != 0 ? c : a.compareTo(b);
    }
}
