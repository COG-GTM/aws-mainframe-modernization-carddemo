package com.carddemo.batch.harness;

import java.nio.file.Files;
import java.io.IOException;
import com.carddemo.common.file.FileStatusException;
import com.carddemo.common.file.FileStatus;
import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.file.RecordPrefix;
import com.carddemo.common.file.VariableRecordWriter;
import java.nio.file.Path;

/**
 * RECFM=V through {@link VariableRecordWriter}: {@code MOVE rec TO VBR-REC(1:len)} + {@code WRITE} is
 * {@link #write(byte[], int)}.
 */
public final class VariableFileSink implements RecordSink {

    private final String ddname;
    private final Path path;
    private final VariableRecordWriter writer;
    private long count;

    public VariableFileSink(String ddname, Path path, RecordPrefix prefix, int minLength, int maxLength) {
        this.ddname = ddname;
        this.path = path;
        this.writer = new VariableRecordWriter(ddname, path, prefix, minLength, maxLength);
    }

    @Override
    public String ddname() {
        return ddname;
    }

    @Override
    public void open() {
        Path parent = path.toAbsolutePath().getParent();
        try {
            if (parent != null) {
                Files.createDirectories(parent);
            }
        } catch (IOException e) {
            throw new FileStatusException(ddname, "OPEN", FileStatus.PERMANENT_ERROR, e);
        }
        writer.open();
        count = 0;
    }

    @Override
    public void write(FixedWidthRecord record) {
        write(record.bytes(), record.bytes().length);
    }

    public void write(byte[] recordArea, int length) {
        writer.write(recordArea, length);
        count++;
    }

    @Override
    public long count() {
        return count;
    }

    @Override
    public boolean isOpen() {
        return writer.isOpen();
    }

    @Override
    public void close() {
        if (writer.isOpen()) {
            writer.close();
        }
    }
}
