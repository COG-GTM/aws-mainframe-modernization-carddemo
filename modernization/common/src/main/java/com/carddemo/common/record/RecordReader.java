package com.carddemo.common.record;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.Closeable;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Sequential reader of fixed-width records. {@link #read()} returns null at end of file, like
 * {@code READ ... AT END}.
 */
public final class RecordReader implements Closeable {

    private final InputStream in;
    private final RecordLayout layout;
    private final CodePage codePage;
    private final RecordFormat format;
    private long recordNumber;
    private String lineEnding = "";

    public RecordReader(InputStream in, RecordLayout layout, CodePage codePage, RecordFormat format) {
        if (format.organization() == RecordFormat.Organization.LINE_SEQUENTIAL && codePage != CodePage.ASCII) {
            throw new IllegalArgumentException("Line-sequential files are ASCII");
        }
        this.in = in instanceof BufferedInputStream ? in : new BufferedInputStream(in);
        this.layout = layout;
        this.codePage = codePage;
        this.format = format;
    }

    public static List<CobolRecord> readAll(Path file, RecordLayout layout, CodePage codePage, RecordFormat format)
            throws IOException {
        try (RecordReader reader = new RecordReader(Files.newInputStream(file), layout, codePage, format)) {
            return reader.readAll();
        }
    }

    public List<CobolRecord> readAll() throws IOException {
        List<CobolRecord> records = new ArrayList<>();
        for (CobolRecord record = read(); record != null; record = read()) {
            records.add(record);
        }
        return records;
    }

    /** The next record, or null at end of file. */
    public CobolRecord read() throws IOException {
        byte[] bytes = format.organization() == RecordFormat.Organization.FIXED ? readFixed() : readLine();
        if (bytes == null) {
            return null;
        }
        recordNumber++;
        return CobolRecord.of(layout, codePage, bytes);
    }

    private byte[] readFixed() throws IOException {
        byte[] bytes = in.readNBytes(layout.length());
        if (bytes.length == 0) {
            return null;
        }
        if (bytes.length != layout.length()) {
            throw new IOException(layout.copybook() + " record " + (recordNumber + 1) + " is short: "
                    + bytes.length + " of " + layout.length() + " bytes");
        }
        return bytes;
    }

    private byte[] readLine() throws IOException {
        ByteArrayOutputStream line = new ByteArrayOutputStream(layout.length() + 2);
        int b = in.read();
        if (b < 0) {
            return null;
        }
        while (b >= 0 && b != '\n') {
            line.write(b);
            b = in.read();
        }
        byte[] text = line.toByteArray();
        boolean cr = text.length > 0 && text[text.length - 1] == '\r';
        int length = cr ? text.length - 1 : text.length;
        lineEnding = b < 0 ? (cr ? "\r" : "") : (cr ? "\r\n" : "\n");
        if (length > layout.length()) {
            throw new IOException(layout.copybook() + " line " + (recordNumber + 1) + " is " + length
                    + " bytes, record length is " + layout.length());
        }
        byte[] record = new byte[layout.length()];
        System.arraycopy(text, 0, record, 0, length);
        Arrays.fill(record, length, record.length, codePage.space());
        return record;
    }

    /**
     * Terminator of the last line-sequential record read: {@code "\n"}, {@code "\r\n"}, or {@code ""}
     * when the file ends without one. Lets a writer reproduce files with mixed line endings.
     */
    public String lineEnding() {
        return lineEnding;
    }

    /** Number of records read so far. */
    public long recordNumber() {
        return recordNumber;
    }

    @Override
    public void close() throws IOException {
        in.close();
    }
}
