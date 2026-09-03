package com.carddemo.mainframe.io;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Reads an unblocked fixed-length ({@code RECFM=F}) mainframe dataset dump into raw records.
 *
 * <p>Sequential dumps of VSAM KSDS files carry no record delimiters: the record boundaries come
 * solely from the copybook {@code RECLN}. Reading the whole dataset is deliberate — the CardDemo
 * interest files are tens of kilobytes, and holding them in memory lets the parity harness run the
 * full input population rather than a sample.
 */
public final class FixedLengthRecordReader {

    private FixedLengthRecordReader() {
    }

    /** Splits a dataset file into fixed-length records of {@code recordLength} bytes. */
    public static List<byte[]> readRecords(Path dataset, int recordLength) {
        byte[] bytes;
        try {
            bytes = Files.readAllBytes(dataset);
        } catch (IOException e) {
            throw new UncheckedIOException("Cannot read dataset " + dataset, e);
        }
        return split(bytes, recordLength, dataset.getFileName().toString());
    }

    /** Splits an in-memory dataset image into fixed-length records. */
    public static List<byte[]> split(byte[] bytes, int recordLength, String datasetName) {
        if (bytes.length % recordLength != 0) {
            throw new RecordDecodingException("Dataset " + datasetName + " is " + bytes.length
                    + " bytes, which is not a multiple of the record length " + recordLength);
        }
        List<byte[]> records = new ArrayList<>(bytes.length / recordLength);
        for (int offset = 0; offset < bytes.length; offset += recordLength) {
            records.add(Arrays.copyOfRange(bytes, offset, offset + recordLength));
        }
        return records;
    }
}
