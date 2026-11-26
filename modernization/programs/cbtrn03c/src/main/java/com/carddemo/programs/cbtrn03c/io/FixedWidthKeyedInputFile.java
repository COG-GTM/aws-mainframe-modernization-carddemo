package com.carddemo.programs.cbtrn03c.io;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.TreeMap;

/** VSAM KSDS emulation: an unloaded fixed-width data set indexed in memory by its prime key. */
public final class FixedWidthKeyedInputFile implements KeyedInputFile {
    private final Path path;
    private final int lrecl;
    private final int keyOffset;
    private final int keyLength;
    private final RecordFormat format;
    private Map<String, String> index;

    public FixedWidthKeyedInputFile(Path path, int lrecl, int keyOffset, int keyLength, RecordFormat format) {
        this.path = path;
        this.lrecl = lrecl;
        this.keyOffset = keyOffset;
        this.keyLength = keyLength;
        this.format = format;
    }

    @Override
    public String open() {
        if (!Files.isRegularFile(path)) {
            return FileStatus.FILE_NOT_FOUND;
        }
        try {
            Map<String, String> loaded = new TreeMap<>();
            for (String record : FixedWidthRecords.load(path, lrecl, format)) {
                loaded.putIfAbsent(record.substring(keyOffset, keyOffset + keyLength), record);
            }
            index = loaded;
            return FileStatus.OK;
        } catch (IOException e) {
            return FileStatus.PERMANENT_ERROR;
        }
    }

    @Override
    public ReadResult read(String key) {
        if (index == null) {
            return ReadResult.status(FileStatus.NOT_OPEN_FOR_READ);
        }
        String record = index.get(key);
        return record == null ? ReadResult.status(FileStatus.RECORD_NOT_FOUND) : ReadResult.ok(record);
    }

    @Override
    public String close() {
        if (index == null) {
            return FileStatus.NOT_OPEN_FOR_CLOSE;
        }
        index = null;
        return FileStatus.OK;
    }
}
