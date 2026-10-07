package com.carddemo.common.file;

import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.codec.RecordFormatException;
import com.carddemo.common.codec.RecordLayout;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * Whole-file access to sequential datasets: RECFM=F images (back-to-back records, as in {@code app/data/EBCDIC}),
 * line-sequential text (as in {@code app/data/ASCII}; short lines space padded, CRLF accepted) and
 * variable-length files framed by a {@link RecordPrefix}.
 */
public final class RecordFiles {

    private RecordFiles() {
    }

    public static List<FixedWidthRecord> split(byte[] data, RecordLayout layout, RecordEncoding encoding) {
        int length = layout.length();
        if (data.length % length != 0) {
            throw new RecordFormatException(layout.name() + ": dataset length " + data.length
                    + " is not a multiple of record length " + length);
        }
        List<FixedWidthRecord> records = new ArrayList<>(data.length / length);
        for (int offset = 0; offset < data.length; offset += length) {
            byte[] image = new byte[length];
            System.arraycopy(data, offset, image, 0, length);
            records.add(new FixedWidthRecord(layout, image, encoding));
        }
        return records;
    }

    public static List<FixedWidthRecord> readFixed(String ddname, Path path, RecordLayout layout,
                                                   RecordEncoding encoding) {
        byte[] data = read(ddname, path);
        try {
            return split(data, layout, encoding);
        } catch (RecordFormatException e) {
            throw new FileStatusException(ddname, "READ", FileStatus.RECORD_LENGTH_MISMATCH, e);
        }
    }

    public static List<FixedWidthRecord> readLines(String ddname, Path path, RecordLayout layout,
                                                   RecordEncoding encoding) {
        String text = new String(read(ddname, path), encoding.charset());
        List<FixedWidthRecord> records = new ArrayList<>();
        String[] lines = text.split("\r?\n", -1);
        int count = lines[lines.length - 1].isEmpty() ? lines.length - 1 : lines.length;
        for (int i = 0; i < count; i++) {
            String line = lines[i];
            try {
                records.add(FixedWidthRecord.fromLine(layout, line, encoding));
            } catch (RecordFormatException e) {
                throw new FileStatusException(ddname, "READ", FileStatus.RECORD_LENGTH_ERROR, e);
            }
        }
        return records;
    }

    public static void writeFixed(String ddname, Path path, List<FixedWidthRecord> records) {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        for (FixedWidthRecord record : records) {
            out.writeBytes(record.bytes());
        }
        write(ddname, path, out.toByteArray());
    }

    /** Line-sequential WRITE; GnuCOBOL drops trailing spaces unless the file is fixed ({@code COB_LS_FIXED}). */
    public static void writeLines(String ddname, Path path, List<FixedWidthRecord> records,
                                  boolean stripTrailingSpaces) {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        for (FixedWidthRecord record : records) {
            String text = stripTrailingSpaces ? record.text().stripTrailing() : record.text();
            out.writeBytes(record.encoding().encode(text));
            out.write('\n');
        }
        write(ddname, path, out.toByteArray());
    }

    /** Reads every record payload of a {@link RecordPrefix#GNUCOBOL_VARSEQ} or {@link RecordPrefix#ZOS_RDW} file. */
    public static List<byte[]> readVariable(String ddname, Path path, RecordPrefix prefix) {
        if (prefix == RecordPrefix.NONE) {
            throw new IllegalArgumentException("unframed variable records cannot be split");
        }
        byte[] data = read(ddname, path);
        List<byte[]> records = new ArrayList<>();
        int pos = 0;
        while (pos < data.length) {
            if (pos + 4 > data.length) {
                throw new FileStatusException(ddname, "READ", FileStatus.RECORD_LENGTH_MISMATCH);
            }
            int length = prefix == RecordPrefix.GNUCOBOL_VARSEQ
                    ? ((data[pos] & 0xFF) << 24) | ((data[pos + 1] & 0xFF) << 16)
                      | ((data[pos + 2] & 0xFF) << 8) | (data[pos + 3] & 0xFF)
                    : (((data[pos] & 0xFF) << 8) | (data[pos + 1] & 0xFF)) - 4;
            pos += 4;
            if (length < 0 || pos + length > data.length) {
                throw new FileStatusException(ddname, "READ", FileStatus.RECORD_LENGTH_MISMATCH);
            }
            byte[] record = new byte[length];
            System.arraycopy(data, pos, record, 0, length);
            records.add(record);
            pos += length;
        }
        return records;
    }

    private static byte[] read(String ddname, Path path) {
        try {
            return Files.readAllBytes(path);
        } catch (NoSuchFileException e) {
            throw new FileStatusException(ddname, "OPEN", FileStatus.FILE_NOT_FOUND, e);
        } catch (IOException e) {
            throw new FileStatusException(ddname, "READ", FileStatus.PERMANENT_ERROR, e);
        }
    }

    private static void write(String ddname, Path path, byte[] data) {
        try {
            Path parent = path.toAbsolutePath().getParent();
            if (parent != null) {
                Files.createDirectories(parent);
            }
            Files.write(path, data);
        } catch (IOException e) {
            throw new FileStatusException(ddname, "WRITE", FileStatus.PERMANENT_ERROR, e);
        }
    }
}
