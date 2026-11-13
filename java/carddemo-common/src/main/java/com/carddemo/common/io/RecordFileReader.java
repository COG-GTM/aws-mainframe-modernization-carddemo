package com.carddemo.common.io;

import com.carddemo.common.cobol.CobolDataException;
import com.carddemo.common.cobol.DataEncoding;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.Closeable;
import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;

/** Streams records out of a sequential dataset (the Java side of a COBOL {@code READ ... AT END}). */
public final class RecordFileReader implements Iterator<byte[]>, Closeable {

    private final InputStream in;
    private final RecordFormat format;
    private final int lrecl;
    private final byte pad;
    private byte[] next;
    private long recordNumber;

    public RecordFileReader(InputStream in, RecordFormat format, int lrecl, DataEncoding encoding) {
        this.in = in instanceof BufferedInputStream ? in : new BufferedInputStream(in);
        this.format = format;
        this.lrecl = lrecl;
        this.pad = encoding.space();
        this.next = read();
    }

    public static RecordFileReader open(Path path, RecordFormat format, int lrecl, DataEncoding encoding) {
        try {
            return new RecordFileReader(Files.newInputStream(path), format, lrecl, encoding);
        } catch (IOException e) {
            throw new UncheckedIOException("Cannot open " + path, e);
        }
    }

    public static List<byte[]> readAll(Path path, RecordFormat format, int lrecl, DataEncoding encoding) {
        try (RecordFileReader reader = open(path, format, lrecl, encoding)) {
            List<byte[]> records = new ArrayList<>();
            reader.forEachRemaining(records::add);
            return records;
        }
    }

    @Override
    public boolean hasNext() {
        return next != null;
    }

    @Override
    public byte[] next() {
        if (next == null) {
            throw new NoSuchElementException();
        }
        byte[] current = next;
        recordNumber++;
        next = read();
        return current;
    }

    /** One-based number of the record most recently returned by {@link #next()}. */
    public long recordNumber() {
        return recordNumber;
    }

    private byte[] read() {
        try {
            return switch (format) {
                case FIXED -> readFixed();
                case VARIABLE -> readVariable();
                case LINE -> readLine();
            };
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    private byte[] readFixed() throws IOException {
        byte[] record = in.readNBytes(lrecl);
        if (record.length == 0) {
            return null;
        }
        if (record.length < lrecl) {
            throw new CobolDataException("Short record " + (recordNumber + 1) + ": " + record.length + " of " + lrecl + " bytes");
        }
        return record;
    }

    private byte[] readVariable() throws IOException {
        byte[] rdw = in.readNBytes(4);
        if (rdw.length == 0) {
            return null;
        }
        if (rdw.length < 4) {
            throw new CobolDataException("Truncated RDW after record " + recordNumber);
        }
        int length = ((rdw[0] & 0xFF) << 8) | (rdw[1] & 0xFF);
        if (length < 4 || rdw[2] != 0 || rdw[3] != 0) {
            throw new CobolDataException("Invalid or spanned RDW after record " + recordNumber);
        }
        byte[] record = in.readNBytes(length - 4);
        if (record.length < length - 4) {
            throw new CobolDataException("Truncated variable record " + (recordNumber + 1));
        }
        return record;
    }

    private byte[] readLine() throws IOException {
        ByteArrayOutputStream line = new ByteArrayOutputStream(lrecl);
        int b;
        boolean any = false;
        while ((b = in.read()) != -1) {
            any = true;
            if (b == '\n') {
                break;
            }
            line.write(b);
        }
        if (!any) {
            return null;
        }
        byte[] bytes = line.toByteArray();
        int length = bytes.length > 0 && bytes[bytes.length - 1] == '\r' ? bytes.length - 1 : bytes.length;
        if (length > lrecl) {
            throw new CobolDataException("Line " + (recordNumber + 1) + " has " + length + " bytes, LRECL is " + lrecl);
        }
        byte[] record = new byte[lrecl];
        Arrays.fill(record, pad);
        System.arraycopy(bytes, 0, record, 0, length);
        return record;
    }

    @Override
    public void close() {
        try {
            in.close();
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }
}
