package com.carddemo.programs.cbtrn02c.io;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

/** KSDS emulation over a sorted map; the key is a fixed slice of the record area. */
public class InMemoryIndexedFile implements IndexedFile {

    private final int recordLength;
    private final int keyOffset;
    private final int keyLength;
    protected final TreeMap<String, String> records = new TreeMap<>();
    private OpenMode openMode;

    public InMemoryIndexedFile(int recordLength, int keyOffset, int keyLength) {
        this.recordLength = recordLength;
        this.keyOffset = keyOffset;
        this.keyLength = keyLength;
    }

    public InMemoryIndexedFile load(Iterable<String> rawRecords) {
        for (String r : rawRecords) {
            String rec = pad(r);
            records.put(keyOf(rec), rec);
        }
        return this;
    }

    public List<String> records() {
        return new ArrayList<>(records.values());
    }

    public Map<String, String> asMap() {
        return records;
    }

    public boolean isOpen() {
        return openMode != null;
    }

    @Override
    public String open(OpenMode mode) {
        if (openMode != null) {
            return FileStatus.OPEN_ALREADY;
        }
        String status = onOpen(mode);
        if (!FileStatus.OK.equals(status)) {
            return status;
        }
        if (mode == OpenMode.OUTPUT) {
            records.clear();
        }
        openMode = mode;
        return FileStatus.OK;
    }

    protected String onOpen(OpenMode mode) {
        return FileStatus.OK;
    }

    protected String onClose() {
        return FileStatus.OK;
    }

    @Override
    public ReadResult read(String key) {
        if (openMode != OpenMode.INPUT && openMode != OpenMode.I_O) {
            return ReadResult.of(FileStatus.READ_NOT_OPEN);
        }
        String rec = records.get(key);
        return rec == null ? ReadResult.of(FileStatus.RECORD_NOT_FOUND) : new ReadResult(FileStatus.OK, rec);
    }

    @Override
    public String write(String record) {
        if (openMode != OpenMode.OUTPUT && openMode != OpenMode.I_O && openMode != OpenMode.EXTEND) {
            return FileStatus.WRITE_NOT_OPEN;
        }
        String rec = pad(record);
        String key = keyOf(rec);
        if (records.containsKey(key)) {
            return FileStatus.DUPLICATE_KEY;
        }
        records.put(key, rec);
        return FileStatus.OK;
    }

    @Override
    public String rewrite(String record) {
        if (openMode != OpenMode.I_O) {
            return FileStatus.REWRITE_NOT_OPEN;
        }
        String rec = pad(record);
        String key = keyOf(rec);
        if (!records.containsKey(key)) {
            return FileStatus.RECORD_NOT_FOUND;
        }
        records.put(key, rec);
        return FileStatus.OK;
    }

    @Override
    public String close() {
        if (openMode == null) {
            return FileStatus.CLOSE_NOT_OPEN;
        }
        String status = onClose();
        openMode = null;
        return status;
    }

    private String keyOf(String rec) {
        return rec.substring(keyOffset, keyOffset + keyLength);
    }

    private String pad(String r) {
        if (r.length() == recordLength) {
            return r;
        }
        if (r.length() > recordLength) {
            return r.substring(0, recordLength);
        }
        return r + " ".repeat(recordLength - r.length());
    }
}
