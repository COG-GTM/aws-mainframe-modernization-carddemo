package com.carddemo.legacyio;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Reads and writes whole legacy data files. Reading keeps each record's framing in a {@link FileRecord}, so writing
 * the result back reproduces the input byte for byte.
 */
public final class LegacyFileCodec {

    private final RecordCodec codec;
    private final Framing framing;

    public LegacyFileCodec(LegacyEncoding encoding, Framing framing) {
        this.codec = new RecordCodec(encoding);
        this.framing = framing;
    }

    /** ASCII line-sequential files, like {@code app/data/ASCII} and GnuCOBOL output. */
    public static LegacyFileCodec ascii() {
        return new LegacyFileCodec(LegacyEncoding.ASCII, Framing.LINE_SEQUENTIAL);
    }

    /** EBCDIC fixed-length files, like the z/OS extracts in {@code app/data/EBCDIC}. */
    public static LegacyFileCodec ebcdic() {
        return new LegacyFileCodec(LegacyEncoding.EBCDIC, Framing.FIXED);
    }

    public RecordCodec recordCodec() {
        return codec;
    }

    public List<FileRecord> read(Path file, RecordLayout layout) throws IOException {
        return read(Files.readAllBytes(file), layout);
    }

    public List<FileRecord> read(byte[] data, RecordLayout layout) {
        return read(data, layout.length(), LayoutSelector.of(layout));
    }

    public List<FileRecord> read(Path file, int recordLength, LayoutSelector selector) throws IOException {
        return read(Files.readAllBytes(file), recordLength, selector);
    }

    /** Reads records of {@code recordLength} bytes, decoding each with the layout {@code selector} picks. */
    public List<FileRecord> read(byte[] data, int recordLength, LayoutSelector selector) {
        List<FileRecord> records = new ArrayList<>();
        if (framing == Framing.FIXED) {
            if (data.length % recordLength != 0) {
                throw new IllegalArgumentException(
                        "file of %d bytes is not a whole number of %d-byte records".formatted(data.length, recordLength));
            }
            for (int offset = 0; offset < data.length; offset += recordLength) {
                byte[] bytes = Arrays.copyOfRange(data, offset, offset + recordLength);
                records.add(new FileRecord(decode(bytes, recordLength, selector, records.size()), recordLength,
                        LineEnding.NONE));
            }
            return records;
        }
        int start = 0;
        while (start < data.length) {
            int newline = indexOf(data, (byte) '\n', start);
            int end = newline < 0 ? data.length : newline;
            LineEnding ending = LineEnding.NONE;
            if (newline >= 0) {
                ending = end > start && data[end - 1] == '\r' ? LineEnding.CRLF : LineEnding.LF;
            }
            int contentEnd = ending == LineEnding.CRLF ? end - 1 : end;
            int stored = contentEnd - start;
            if (stored > recordLength) {
                throw new IllegalArgumentException("line %d has %d bytes, more than the %d-byte record"
                        .formatted(records.size() + 1, stored, recordLength));
            }
            byte[] bytes = new byte[recordLength];
            System.arraycopy(data, start, bytes, 0, stored);
            Arrays.fill(bytes, stored, recordLength, codec.encoding().space());
            records.add(new FileRecord(decode(bytes, recordLength, selector, records.size()), stored, ending));
            start = newline < 0 ? data.length : newline + 1;
        }
        return records;
    }

    /** Writes records with their original framing. */
    public byte[] write(List<FileRecord> records) {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        for (FileRecord fileRecord : records) {
            byte[] bytes = codec.encode(fileRecord.record());
            int stored = fileRecord.storedLength();
            for (int i = stored; i < bytes.length; i++) {
                if (bytes[i] != codec.encoding().space()) {
                    throw new IllegalArgumentException("record " + fileRecord.record().layout().name()
                            + " has data past its stored length " + stored);
                }
            }
            if (framing == Framing.FIXED && (stored != bytes.length || fileRecord.ending() != LineEnding.NONE)) {
                throw new IllegalArgumentException("fixed-length records are stored whole and unterminated");
            }
            if (framing == Framing.LINE_SEQUENTIAL) {
                checkLineContent(fileRecord, bytes, stored);
            }
            out.write(bytes, 0, stored);
            out.writeBytes(fileRecord.ending().bytes());
        }
        return out.toByteArray();
    }

    /** Writes records whole, LF-terminated for line-sequential files. */
    public byte[] writeRecords(List<Record> records) {
        LineEnding ending = framing == Framing.FIXED ? LineEnding.NONE : LineEnding.LF;
        return write(records.stream().map(r -> new FileRecord(r, r.layout().length(), ending)).toList());
    }

    public void write(Path file, List<FileRecord> records) throws IOException {
        Files.write(file, write(records));
    }

    private Record decode(byte[] bytes, int recordLength, LayoutSelector selector, int index) {
        RecordLayout layout = selector.select(bytes, codec.encoding());
        if (layout.length() != recordLength) {
            throw new IllegalArgumentException("record %d: layout %s does not match the %d-byte record length"
                    .formatted(index + 1, layout, recordLength));
        }
        return codec.decode(layout, bytes);
    }

    private static void checkLineContent(FileRecord fileRecord, byte[] bytes, int stored) {
        int newline = indexOf(bytes, (byte) '\n', 0);
        if (newline >= 0 && newline < stored) {
            throw new IllegalArgumentException("record " + fileRecord.record().layout().name()
                    + " has a newline byte at offset " + newline + " and cannot be written line sequential");
        }
        if (stored > 0 && bytes[stored - 1] == '\r' && fileRecord.ending() == LineEnding.LF) {
            throw new IllegalArgumentException("record " + fileRecord.record().layout().name()
                    + " ends in a carriage return that would read back as a CRLF ending");
        }
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
