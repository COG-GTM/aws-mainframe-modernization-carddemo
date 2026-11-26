package com.carddemo.programs.cbtrn02c.io;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

/** KSDS backed by a line-per-record file: loaded on OPEN INPUT / I-O, saved in key order on CLOSE. */
public class FileIndexedFile extends InMemoryIndexedFile {

    private final Path path;
    private final int recordLength;
    private boolean dirty;

    public FileIndexedFile(Path path, int recordLength, int keyOffset, int keyLength) {
        super(recordLength, keyOffset, keyLength);
        this.path = path;
        this.recordLength = recordLength;
    }

    @Override
    protected String onOpen(OpenMode mode) {
        records.clear();
        dirty = mode != OpenMode.INPUT;
        if (mode == OpenMode.OUTPUT) {
            return FileStatus.OK;
        }
        if (!Files.exists(path)) {
            return FileStatus.FILE_NOT_FOUND;
        }
        List<String> loaded;
        try {
            loaded = LineFiles.readRecords(path, recordLength);
        } catch (RuntimeException e) {
            return FileStatus.PERMANENT_ERROR;
        }
        load(loaded);
        // a KSDS cannot hold duplicate keys; refuse to open rather than silently drop records
        return records.size() == loaded.size() ? FileStatus.OK : FileStatus.PERMANENT_ERROR;
    }

    @Override
    protected String onClose() {
        if (dirty) {
            try {
                LineFiles.writeRecords(path, records());
            } catch (RuntimeException e) {
                return FileStatus.PERMANENT_ERROR;
            }
        }
        return FileStatus.OK;
    }
}
