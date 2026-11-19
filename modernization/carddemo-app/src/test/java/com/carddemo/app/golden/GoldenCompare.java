package com.carddemo.app.golden;

import com.carddemo.common.cobol.CodePage;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HexFormat;
import java.util.List;
import java.util.regex.Pattern;

/** Parity checks of a Java run against a {@link GoldenCase} (plan section 7). */
public final class GoldenCompare {

    /** D-007: TRAN-PROC-TS hundredths, TRANSACT record bytes 325-326 (1-based), are not reproducible. */
    public static final int[] TRANSACT_MASK = {325, 326};

    private static final Pattern COUNT_LINE = Pattern.compile("^TRANSACTIONS (PROCESSED|REJECTED) *:.*$");

    private GoldenCompare() {
    }

    /**
     * Compares two unloads line by line. Records are padded to {@code recordLength} first (GnuCOBOL LINE SEQUENTIAL
     * drops trailing spaces), and the 1-based byte positions in {@code mask} are ignored. Returns the differences.
     */
    public static List<String> compareUnloads(List<String> expected, List<String> actual, int recordLength, int... mask) {
        List<String> diffs = new ArrayList<>();
        if (expected.size() != actual.size()) {
            diffs.add("record count: expected " + expected.size() + ", actual " + actual.size());
        }
        for (int i = 0; i < Math.min(expected.size(), actual.size()); i++) {
            String e = masked(expected.get(i), recordLength, mask);
            String a = masked(actual.get(i), recordLength, mask);
            if (!e.equals(a)) {
                diffs.add("record " + (i + 1) + ":\n  expected [" + expected.get(i) + "]\n  actual   [" + actual.get(i) + "]");
            }
        }
        return diffs;
    }

    static String masked(String line, int recordLength, int... mask) {
        if (line.length() > recordLength) {
            throw new IllegalArgumentException("record longer than " + recordLength + ": " + line.length());
        }
        char[] c = (line + " ".repeat(recordLength - line.length())).toCharArray();
        for (int position : mask) {
            c[position - 1] = '#';
        }
        return new String(c);
    }

    /** The SYSOUT count DISPLAYs (CBTRN02C.cbl:227-230), compared on normal end only. */
    public static List<String> countLines(List<String> sysout) {
        return sysout.stream().map(String::stripTrailing).filter(l -> COUNT_LINE.matcher(l).matches()).toList();
    }

    /**
     * Expected DALYREJS bytes in the input encoding. The oracle reads CP037 input after a CP037 to ISO-8859-1
     * conversion, so for GC-7 its DALYREJS is converted back to CP037 (R-14); ASCII cases compare as is.
     */
    public static byte[] expectedDalyrejs(Path oracleDalyrejs, CodePage inputEncoding) {
        byte[] oracle = readAllBytes(oracleDalyrejs);
        return CodePage.ASCII.convert(oracle, inputEncoding);
    }

    public static String sha256(byte[] bytes) {
        try {
            return HexFormat.of().formatHex(MessageDigest.getInstance("SHA-256").digest(bytes));
        } catch (NoSuchAlgorithmException ex) {
            throw new IllegalStateException(ex);
        }
    }

    public static List<String> lines(Path file) {
        try {
            return Files.readAllLines(file, CodePage.ASCII.charset());
        } catch (IOException ex) {
            throw new UncheckedIOException(ex);
        }
    }

    static byte[] readAllBytes(Path file) {
        try {
            return Files.readAllBytes(file);
        } catch (IOException ex) {
            throw new UncheckedIOException(ex);
        }
    }

    /** Splits a fixed-block file into records. */
    public static List<byte[]> records(byte[] bytes, int length) {
        if (bytes.length % length != 0) {
            throw new IllegalArgumentException(bytes.length + " bytes is not a multiple of " + length);
        }
        List<byte[]> records = new ArrayList<>();
        for (int i = 0; i < bytes.length; i += length) {
            records.add(Arrays.copyOfRange(bytes, i, i + length));
        }
        return records;
    }
}
