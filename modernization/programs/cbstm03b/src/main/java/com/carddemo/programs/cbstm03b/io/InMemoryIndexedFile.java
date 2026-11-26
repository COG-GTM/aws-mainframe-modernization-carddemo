package com.carddemo.programs.cbstm03b.io;

import com.carddemo.programs.cbstm03b.copybook.FileStatus;

import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

/**
 * In-memory KSDS: fixed-length records keyed by a byte range, held in ascending key order.
 * Key order uses Java {@code String} comparison (ASCII collation, not EBCDIC).
 */
public final class InMemoryIndexedFile implements IndexedFile {

    private final String name;
    private final int recordLength;
    private final int keyOffset;
    private final int keyLength;
    private final boolean exists;
    private final TreeMap<String, String> records = new TreeMap<>();

    private boolean open;
    private boolean atEnd;
    private Iterator<Map.Entry<String, String>> cursor;

    private InMemoryIndexedFile(String name, int recordLength, int keyOffset, int keyLength,
                                List<String> data, boolean exists) {
        this.name = name;
        this.recordLength = recordLength;
        this.keyOffset = keyOffset;
        this.keyLength = keyLength;
        this.exists = exists;
        for (String rec : data) {
            if (rec.length() != recordLength) {
                throw new IllegalArgumentException(name + ": record length " + rec.length()
                        + " != " + recordLength);
            }
            String key = rec.substring(keyOffset, keyOffset + keyLength);
            if (records.putIfAbsent(key, rec) != null) {
                throw new IllegalArgumentException(name + ": duplicate key '" + key + "'");
            }
        }
    }

    public static InMemoryIndexedFile of(String name, int recordLength, int keyOffset, int keyLength,
                                         List<String> records) {
        return new InMemoryIndexedFile(name, recordLength, keyOffset, keyLength, records, true);
    }

    /** A DD whose dataset is not allocated: OPEN returns status 35. */
    public static InMemoryIndexedFile missing(String name, int recordLength, int keyOffset, int keyLength) {
        return new InMemoryIndexedFile(name, recordLength, keyOffset, keyLength, List.of(), false);
    }

    public String name() {
        return name;
    }

    public int size() {
        return records.size();
    }

    public boolean isOpen() {
        return open;
    }

    @Override
    public FileStatus open() {
        if (open) {
            return FileStatus.ALREADY_OPEN;
        }
        if (!exists) {
            return FileStatus.FILE_NOT_FOUND;
        }
        open = true;
        atEnd = false;
        cursor = records.entrySet().iterator();
        return FileStatus.SUCCESS;
    }

    @Override
    public FileStatus close() {
        if (!open) {
            return FileStatus.NOT_OPEN_ON_CLOSE;
        }
        open = false;
        cursor = null;
        return FileStatus.SUCCESS;
    }

    @Override
    public ReadResult readNext() {
        if (!open) {
            return ReadResult.of(FileStatus.NOT_OPEN_FOR_READ);
        }
        if (atEnd) {
            return ReadResult.of(FileStatus.READ_AFTER_END);
        }
        if (!cursor.hasNext()) {
            atEnd = true;
            return ReadResult.of(FileStatus.END_OF_FILE);
        }
        return new ReadResult(FileStatus.SUCCESS, cursor.next().getValue());
    }

    @Override
    public ReadResult readByKey(String key) {
        if (!open) {
            return ReadResult.of(FileStatus.NOT_OPEN_FOR_READ);
        }
        if (key == null || key.length() != keyLength) {
            throw new IllegalArgumentException(name + ": key must be " + keyLength + " bytes");
        }
        String rec = records.get(key);
        return rec == null ? ReadResult.of(FileStatus.RECORD_NOT_FOUND) : new ReadResult(FileStatus.SUCCESS, rec);
    }

}
