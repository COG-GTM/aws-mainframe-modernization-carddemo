package com.carddemo.legacyio;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Function;

/** Reads and writes whole CardDemo data files. */
public final class LegacyFile {

    private LegacyFile() {
    }

    /** Reads a CardDemo file in the sample-data format for its encoding (EBCDIC fixed, ASCII line sequential). */
    public static List<Record> read(Path path, CardDemoFile file, Encoding encoding) throws IOException {
        try (InputStream in = Files.newInputStream(path)) {
            return read(in, file, encoding, RecordFormat.defaultFor(encoding));
        }
    }

    public static List<Record> read(InputStream in, CardDemoFile file, Encoding encoding, RecordFormat format)
            throws IOException {
        return read(in, file.recordLength(), format, bytes -> file.codec(bytes, encoding));
    }

    /**
     * Reads every record of a stream.
     *
     * @param codecFor picks the codec for a record's bytes (records with {@code REDEFINES} variants differ by type)
     */
    public static List<Record> read(InputStream in, int recordLength, RecordFormat format,
            Function<byte[], RecordCodec> codecFor) throws IOException {
        byte[] data = in.readAllBytes();
        List<Record> records = new ArrayList<>();
        if (format == RecordFormat.FIXED) {
            if (data.length % recordLength != 0) {
                throw new IOException("File of " + data.length + " bytes is not a whole number of " + recordLength
                        + "-byte records");
            }
            for (int at = 0; at < data.length; at += recordLength) {
                byte[] bytes = Arrays.copyOfRange(data, at, at + recordLength);
                records.add(codecFor.apply(bytes).decode(bytes));
            }
            return records;
        }
        int start = 0;
        int lineNumber = 0;
        while (start < data.length) {
            lineNumber++;
            int newline = indexOf(data, (byte) '\n', start);
            int end = newline < 0 ? data.length : newline;
            LineFraming.Terminator terminator = LineFraming.Terminator.NONE;
            if (newline >= 0) {
                terminator = LineFraming.Terminator.LF;
                if (end > start && data[end - 1] == '\r') {
                    terminator = LineFraming.Terminator.CRLF;
                    end--;
                }
            }
            int length = end - start;
            if (length > recordLength) {
                throw new IOException("Line " + lineNumber + " has " + length + " bytes; records are " + recordLength);
            }
            byte[] bytes = new byte[recordLength];
            System.arraycopy(data, start, bytes, 0, length);
            RecordCodec probe = codecFor.apply(padded(bytes, length, Encoding.ASCII.space()));
            Arrays.fill(bytes, length, recordLength, probe.encoding().space());
            records.add(probe.decode(bytes).framing(new LineFraming(length, terminator)));
            start = newline < 0 ? data.length : newline + 1;
        }
        return records;
    }

    /** Writes records in the sample-data format for the encoding. */
    public static void write(Path path, List<Record> records, Encoding encoding) throws IOException {
        try (OutputStream out = Files.newOutputStream(path)) {
            write(out, records, encoding, RecordFormat.defaultFor(encoding));
        }
    }

    /**
     * Writes records. In line-sequential format a record keeps the line length and ending it was read with, as long
     * as the bytes past that length are still spaces; other records are written in full and end with LF. Only the
     * last record may be left without a line ending. A record whose bytes contain LF, or end in CR before a line
     * ending, cannot be read back line by line and is rejected; write such records (e.g. binary export fields) in
     * {@link RecordFormat#FIXED}.
     */
    public static void write(OutputStream out, List<Record> records, Encoding encoding, RecordFormat format)
            throws IOException {
        for (int index = 0; index < records.size(); index++) {
            Record record = records.get(index);
            byte[] bytes = new RecordCodec(record.layout(), encoding).encode(record);
            if (format == RecordFormat.FIXED) {
                out.write(bytes);
                continue;
            }
            LineFraming framing = record.framing();
            int length = bytes.length;
            byte[] terminator = LineFraming.Terminator.LF.bytes();
            if (framing != null) {
                boolean last = index == records.size() - 1;
                if (framing.terminator() != LineFraming.Terminator.NONE || last) {
                    terminator = framing.terminator().bytes();
                }
                if (framing.length() <= bytes.length && allSpaces(bytes, framing.length(), encoding.space())) {
                    length = framing.length();
                }
            }
            int lineFeed = indexOf(bytes, (byte) '\n', 0);
            if (lineFeed >= 0 && lineFeed < length) {
                throw new IllegalArgumentException("Record " + (index + 1) + " (" + record.layout().name()
                        + ") has a line-feed byte at offset " + lineFeed + "; write it in FIXED format");
            }
            if (terminator.length > 0 && length > 0 && bytes[length - 1] == '\r') {
                throw new IllegalArgumentException("Record " + (index + 1) + " (" + record.layout().name()
                        + ") ends in a carriage-return byte; write it in FIXED format");
            }
            out.write(bytes, 0, length);
            out.write(terminator);
        }
    }

    /** Encodes records to a byte array, see {@link #write(OutputStream, List, Encoding, RecordFormat)}. */
    public static byte[] toBytes(List<Record> records, Encoding encoding, RecordFormat format) {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        try {
            write(out, records, encoding, format);
        } catch (IOException e) {
            throw new IllegalStateException(e);
        }
        return out.toByteArray();
    }

    private static byte[] padded(byte[] bytes, int length, byte space) {
        byte[] copy = bytes.clone();
        Arrays.fill(copy, length, copy.length, space);
        return copy;
    }

    private static boolean allSpaces(byte[] bytes, int from, byte space) {
        for (int i = from; i < bytes.length; i++) {
            if (bytes[i] != space) {
                return false;
            }
        }
        return true;
    }

    private static int indexOf(byte[] data, byte value, int from) {
        for (int i = from; i < data.length; i++) {
            if (data[i] == value) {
                return i;
            }
        }
        return -1;
    }
}
