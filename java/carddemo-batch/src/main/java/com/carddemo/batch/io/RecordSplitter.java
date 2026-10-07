package com.carddemo.batch.io;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Splits the bytes of an input file into fixed-width records. Two formats are understood:
 * <ul>
 *   <li>{@link Format#FIXED}: records are exactly {@code recordLength} bytes, optionally followed by a
 *       {@code '\n'} (the way {@code app/data/ASCII} stores the 300/350-byte sample files). A trailing
 *       fragment shorter than a record is a file attribute mismatch (status {@code 39}).</li>
 *   <li>{@link Format#LINE_SEQUENTIAL}: one record per {@code '\n'}-terminated line; a short line is padded
 *       with spaces to {@code recordLength}, the way GnuCOBOL's {@code ORGANIZATION LINE SEQUENTIAL} READ
 *       into a spaces-initialised record area does (this is how the harness's KSDSLOAD loads the 36-byte
 *       lines of {@code cardxref.txt} into the 50-byte XREFFILE). A line longer than a record is status
 *       {@code 39}.</li>
 * </ul>
 */
final class RecordSplitter {

    enum Format { FIXED, LINE_SEQUENTIAL }

    private RecordSplitter() {
    }

    static List<byte[]> split(String ddname, byte[] bytes, int recordLength, Format format) {
        return format == Format.LINE_SEQUENTIAL
                ? splitLines(ddname, bytes, recordLength)
                : splitFixed(ddname, bytes, recordLength);
    }

    private static List<byte[]> splitFixed(String ddname, byte[] bytes, int recordLength) {
        List<byte[]> out = new ArrayList<>();
        int pos = 0;
        while (pos < bytes.length) {
            if (bytes.length - pos < recordLength) {
                throw new FileStatusException(ddname, "OPEN", FileStatusException.ATTRIBUTE_MISMATCH,
                        new IOException("trailing " + (bytes.length - pos) + " bytes are not a " + recordLength
                                + "-byte record"));
            }
            out.add(Arrays.copyOfRange(bytes, pos, pos + recordLength));
            pos += recordLength;
            if (pos < bytes.length && bytes[pos] == '\n') {
                pos++;
            }
        }
        return out;
    }

    private static List<byte[]> splitLines(String ddname, byte[] bytes, int recordLength) {
        List<byte[]> out = new ArrayList<>();
        int pos = 0;
        while (pos < bytes.length) {
            int end = pos;
            while (end < bytes.length && bytes[end] != '\n') {
                end++;
            }
            int lineLength = end - pos;
            if (lineLength > recordLength) {
                throw new FileStatusException(ddname, "OPEN", FileStatusException.ATTRIBUTE_MISMATCH,
                        new IOException("line " + (out.size() + 1) + " is " + lineLength + " bytes, longer than the "
                                + recordLength + "-byte record"));
            }
            byte[] rec = new byte[recordLength];
            Arrays.fill(rec, (byte) ' ');
            System.arraycopy(bytes, pos, rec, 0, lineLength);
            out.add(rec);
            pos = end + 1;
        }
        return out;
    }
}
