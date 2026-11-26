package com.carddemo.programs.cbact02c.io;

import com.carddemo.programs.cbact02c.copybook.FdCardfileRec;

import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

/**
 * In-memory KSDS: records are delivered in ascending FD-CARD-NUM order using EBCDIC collation
 * (space &lt; lowercase &lt; uppercase &lt; digits), as a VSAM KSDS read sequentially would.
 * Records whose length is not 150 are not indexable; they are served after all keyed records,
 * in input order, with status 04.
 */
public class InMemoryCardFile implements CardFile {

    private final List<String> records;
    private boolean open;
    private int next;

    public InMemoryCardFile(List<String> records) {
        List<String> sorted = new ArrayList<>();
        List<String> wrongLength = new ArrayList<>();
        for (String record : records) {
            (record.length() == FdCardfileRec.RECORD_LENGTH ? sorted : wrongLength).add(record);
        }
        sorted.sort(Comparator.comparing(InMemoryCardFile::key, InMemoryCardFile::compareEbcdic));
        sorted.addAll(wrongLength);
        this.records = List.copyOf(sorted);
    }

    static String key(String record) {
        return FdCardfileRec.fromFixedWidth(record).fdCardNum();
    }

    static int compareEbcdic(String a, String b) {
        Charset ebcdic = FixedWidthCardFile.ebcdic();
        return Arrays.compareUnsigned(a.getBytes(ebcdic), b.getBytes(ebcdic));
    }

    @Override
    public String open() {
        if (open) {
            return FileStatus.ALREADY_OPEN;
        }
        open = true;
        next = 0;
        return FileStatus.SUCCESS;
    }

    @Override
    public ReadResult read() {
        if (!open) {
            return new ReadResult(FileStatus.READ_NOT_OPEN, null);
        }
        if (next >= records.size()) {
            return new ReadResult(FileStatus.END_OF_FILE, null);
        }
        String record = records.get(next++);
        String status = record.length() == FdCardfileRec.RECORD_LENGTH
                ? FileStatus.SUCCESS
                : FileStatus.RECORD_LENGTH_MISMATCH;
        return new ReadResult(status, record);
    }

    @Override
    public String close() {
        if (!open) {
            return FileStatus.CLOSE_NOT_OPEN;
        }
        open = false;
        return FileStatus.SUCCESS;
    }
}
