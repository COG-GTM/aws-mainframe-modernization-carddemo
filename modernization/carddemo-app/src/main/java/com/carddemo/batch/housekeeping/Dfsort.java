package com.carddemo.batch.housekeeping;

import com.carddemo.common.codec.FixedWidthRecord;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

/**
 * The DFSORT control statements of the CardDemo JCL: {@code SORT FIELDS=(p,l,CH|ZD,A)} keys (positions 1-based as in
 * the JCL), applied as a stable sort. {@code CH} compares bytes in the record's code page; {@code ZD} compares the
 * value of an all-digit field and falls back to byte order otherwise (all-digit values first), as the baseline
 * emulator {@code scripts/baseline/baseline.py dfsort} does.
 */
public final class Dfsort {

    private Dfsort() {
    }

    public static Comparator<FixedWidthRecord> ch(int position, int length) {
        return (a, b) -> Arrays.compareUnsigned(a.bytes(), position - 1, position - 1 + length,
                b.bytes(), position - 1, position - 1 + length);
    }

    public static Comparator<FixedWidthRecord> zd(int position, int length) {
        Comparator<FixedWidthRecord> bytes = ch(position, length);
        return (a, b) -> {
            String x = a.text().substring(position - 1, position - 1 + length);
            String y = b.text().substring(position - 1, position - 1 + length);
            boolean dx = isDigits(x);
            boolean dy = isDigits(y);
            if (dx && dy) {
                return new BigInteger(x).compareTo(new BigInteger(y));
            }
            if (dx != dy) {
                return dx ? -1 : 1;
            }
            return bytes.compare(a, b);
        };
    }

    public static List<FixedWidthRecord> sort(List<FixedWidthRecord> records, Comparator<FixedWidthRecord> fields) {
        List<FixedWidthRecord> sorted = new ArrayList<>(records);
        sorted.sort(fields);
        return sorted;
    }

    /** The DFSORT end-of-step message (ICE054I) for {@code in} input and {@code out} output records. */
    public static String summary(long in, long out) {
        return "ICE054I 0 RECORDS - IN: " + in + ", OUT: " + out;
    }

    private static boolean isDigits(String s) {
        return !s.isEmpty() && s.chars().allMatch(c -> c >= '0' && c <= '9');
    }
}
