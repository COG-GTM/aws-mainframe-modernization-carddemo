package com.carddemo.programs.cbtrn03c.io;

import com.carddemo.programs.cbtrn03c.support.Fixed;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

/** Sequential output data set of fixed-length records (records are space-padded/truncated to LRECL). */
public final class FixedWidthSequentialOutputFile implements SequentialOutputFile {
    private final Path path;
    private final int lrecl;
    private final RecordFormat format;
    private BufferedWriter writer;

    public FixedWidthSequentialOutputFile(Path path, int lrecl, RecordFormat format) {
        this.path = path;
        this.lrecl = lrecl;
        this.format = format;
    }

    @Override
    public String open() {
        try {
            writer = Files.newBufferedWriter(path, FixedWidthRecords.CHARSET);
            return FileStatus.OK;
        } catch (IOException e) {
            return FileStatus.PERMANENT_ERROR;
        }
    }

    @Override
    public String write(String record) {
        if (writer == null) {
            return FileStatus.NOT_OPEN_FOR_WRITE;
        }
        try {
            writer.write(Fixed.pic(record, lrecl));
            if (format == RecordFormat.LINE) {
                writer.write('\n');
            }
            return FileStatus.OK;
        } catch (IOException e) {
            return FileStatus.PERMANENT_ERROR;
        }
    }

    @Override
    public String close() {
        if (writer == null) {
            return FileStatus.NOT_OPEN_FOR_CLOSE;
        }
        try {
            writer.close();
            return FileStatus.OK;
        } catch (IOException e) {
            return FileStatus.PERMANENT_ERROR;
        } finally {
            writer = null;
        }
    }
}
