package com.carddemo.recordio.store;

import com.carddemo.recordio.codec.FixedWidthRecord;
import com.carddemo.recordio.codec.RecordEncoding;
import com.carddemo.recordio.codec.RecordFormatException;
import com.carddemo.recordio.layout.RecordLayout;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * A RECFM=F physical-sequential dataset: a byte stream of back-to-back records of one fixed
 * length, no record delimiters. Matches the shipped {@code app/data/EBCDIC} images.
 */
public final class FixedWidthFile {

    private FixedWidthFile() {
    }

    public static List<FixedWidthRecord> split(byte[] bytes, int recordLength, RecordEncoding encoding) {
        if (bytes.length % recordLength != 0) {
            throw new RecordFormatException("dataset length " + bytes.length
                    + " is not a multiple of record length " + recordLength);
        }
        List<FixedWidthRecord> records = new ArrayList<>(bytes.length / recordLength);
        for (int offset = 0; offset < bytes.length; offset += recordLength) {
            byte[] image = new byte[recordLength];
            System.arraycopy(bytes, offset, image, 0, recordLength);
            records.add(new FixedWidthRecord(image, encoding));
        }
        return records;
    }

    public static <T> List<T> readAll(Path path, RecordLayout<T> layout, RecordEncoding encoding) {
        try {
            return split(Files.readAllBytes(path), layout.length(), encoding).stream().map(layout::decode).toList();
        } catch (IOException e) {
            throw new UncheckedIOException("cannot read " + path, e);
        }
    }

    public static byte[] join(List<FixedWidthRecord> records) {
        int total = records.stream().mapToInt(FixedWidthRecord::length).sum();
        byte[] out = new byte[total];
        int offset = 0;
        for (FixedWidthRecord record : records) {
            byte[] image = record.bytes();
            System.arraycopy(image, 0, out, offset, image.length);
            offset += image.length;
        }
        return out;
    }

    public static void write(Path path, List<FixedWidthRecord> records) {
        try {
            Files.createDirectories(path.toAbsolutePath().getParent());
            Files.write(path, join(records));
        } catch (IOException e) {
            throw new UncheckedIOException("cannot write " + path, e);
        }
    }
}
