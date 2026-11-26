package com.carddemo.programs.cbact03c;

import com.carddemo.programs.cbact03c.copybook.CardXrefRecord;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;

/**
 * File-backed XREFFILE using the 50-byte CVACT03Y layout.
 *
 * <ul>
 *   <li>{@link Format#LINE_SEQUENTIAL}: one record per line (e.g. app/data/ASCII/cardxref.txt);
 *       trailing spaces trimmed by the export are restored by padding to 50 bytes, as a LINE SEQUENTIAL
 *       READ does for short lines.
 *       A line longer than 50 bytes reads with status "04" (record length conflict).</li>
 *   <li>{@link Format#FIXED}: contiguous 50-byte records with no delimiters, decoded with the given
 *       charset (e.g. IBM037 for app/data/EBCDIC/AWS.M2.CARDDEMO.CARDXREF.PS). A trailing partial
 *       record reads with status "04".</li>
 * </ul>
 *
 * Well-formed records are returned in ascending key order (first 16 bytes, compared as unsigned bytes
 * in the file's code page), as a KSDS read with ACCESS MODE SEQUENTIAL would; a duplicate key
 * (impossible in a KSDS) makes OPEN fail with status "22". Malformed-length records are returned after
 * all well-formed records, in file order, each as a read with status "04".
 */
public class FixedWidthXrefFile implements XrefFile {

    public enum Format { LINE_SEQUENTIAL, FIXED }

    private final Path path;
    private final Format format;
    private final Charset charset;
    private Iterator<ReadResult> cursor;

    public FixedWidthXrefFile(Path path, Format format, Charset charset) {
        this.path = path;
        this.format = format;
        this.charset = charset;
    }

    public static FixedWidthXrefFile ascii(Path path) {
        return new FixedWidthXrefFile(path, Format.LINE_SEQUENTIAL, StandardCharsets.US_ASCII);
    }

    public static FixedWidthXrefFile ebcdic(Path path) {
        return new FixedWidthXrefFile(path, Format.FIXED, Charset.forName("IBM037"));
    }

    @Override
    public String open() {
        List<String> images;
        try {
            images = format == Format.LINE_SEQUENTIAL
                    ? Files.readAllLines(path, charset)
                    : splitFixed(Files.readAllBytes(path));
        } catch (NoSuchFileException e) {
            return FileStatus.FILE_NOT_FOUND;
        } catch (IOException e) {
            return FileStatus.PERMANENT_IO_ERROR;
        }
        List<String> records = new ArrayList<>();
        List<ReadResult> malformed = new ArrayList<>();
        for (String image : images) {
            if (lengthOk(image)) {
                records.add(pad(image));
            } else {
                malformed.add(ReadResult.status(FileStatus.RECORD_LENGTH_MISMATCH));
            }
        }
        records.sort(Comparator.comparing(this::keyBytes, Arrays::compareUnsigned));
        List<ReadResult> results = new ArrayList<>();
        for (int i = 0; i < records.size(); i++) {
            if (i > 0 && key(records.get(i)).equals(key(records.get(i - 1)))) {
                return FileStatus.DUPLICATE_KEY;
            }
            results.add(ReadResult.of(records.get(i)));
        }
        results.addAll(malformed);
        cursor = results.iterator();
        return FileStatus.OK;
    }

    @Override
    public ReadResult readNext() {
        if (cursor == null) {
            return ReadResult.status(FileStatus.READ_NOT_OPEN);
        }
        return cursor.hasNext() ? cursor.next() : ReadResult.status(FileStatus.END_OF_FILE);
    }

    @Override
    public String close() {
        if (cursor == null) {
            return FileStatus.NOT_OPEN;
        }
        cursor = null;
        return FileStatus.OK;
    }

    private List<String> splitFixed(byte[] data) {
        List<String> out = new ArrayList<>();
        int len = CardXrefRecord.RECORD_LENGTH;
        for (int off = 0; off < data.length; off += len) {
            out.add(new String(data, off, Math.min(len, data.length - off), charset));
        }
        return out;
    }

    private boolean lengthOk(String image) {
        int len = CardXrefRecord.RECORD_LENGTH;
        return format == Format.LINE_SEQUENTIAL ? image.length() <= len : image.length() == len;
    }

    /** KSDS keys collate on raw bytes, so compare in the file's code page (IBM037: letters before digits). */
    private byte[] keyBytes(String record) {
        return key(record).getBytes(charset);
    }

    private static String pad(String s) {
        return s + " ".repeat(CardXrefRecord.RECORD_LENGTH - s.length());
    }

    private static String key(String record) {
        return record.substring(0, CardXrefRecord.CARD_NUM_LENGTH);
    }
}
