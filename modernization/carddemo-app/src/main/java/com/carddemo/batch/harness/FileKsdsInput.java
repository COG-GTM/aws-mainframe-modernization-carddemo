package com.carddemo.batch.harness;

import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.codec.RecordLayout;
import com.carddemo.common.file.FileStatus;
import com.carddemo.common.file.FileStatusException;
import com.carddemo.common.file.RecordFiles;
import java.nio.file.Path;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;

final class FileKsdsInput implements KsdsInput {

    private final String ddname;
    private final Path path;
    private final RecordLayout layout;
    private final RecordEncoding encoding;
    private Iterator<FixedWidthRecord> records;

    FileKsdsInput(String ddname, Path path, RecordLayout layout, RecordEncoding encoding) {
        this.ddname = ddname;
        this.path = path;
        this.layout = layout;
        this.encoding = encoding;
    }

    @Override
    public String ddname() {
        return ddname;
    }

    @Override
    public void open() {
        List<FixedWidthRecord> all = encoding == RecordEncoding.EBCDIC
                ? RecordFiles.readFixed(ddname, path, layout, encoding)
                : RecordFiles.readLines(ddname, path, layout, encoding);
        records = all.iterator();
    }

    @Override
    public Optional<FixedWidthRecord> readNext() {
        if (records == null) {
            throw new FileStatusException(ddname, "READ", FileStatus.NOT_OPEN_INPUT);
        }
        return records.hasNext() ? Optional.of(records.next()) : Optional.empty();
    }

    @Override
    public void close() {
        if (records == null) {
            throw new FileStatusException(ddname, "CLOSE", FileStatus.NOT_OPEN);
        }
        records = null;
    }
}
