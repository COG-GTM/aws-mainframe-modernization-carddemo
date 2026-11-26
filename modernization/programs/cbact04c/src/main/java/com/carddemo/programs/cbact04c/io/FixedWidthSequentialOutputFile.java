package com.carddemo.programs.cbact04c.io;

import com.carddemo.programs.cbact04c.copybook.RecordCodec;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;

/**
 * Sequential output writing one fixed-width record image per line (RECFM=F, LRECL = codec length).
 * OPEN creates a new file and fails if the path already exists (DISP=NEW), so existing files and symlinks are never truncated.
 */
public class FixedWidthSequentialOutputFile<T> implements SequentialOutputFile<T> {

    private final Path path;
    private final RecordCodec<T> codec;
    private BufferedWriter writer;

    public FixedWidthSequentialOutputFile(Path path, RecordCodec<T> codec) {
        this.path = path;
        this.codec = codec;
    }

    @Override
    public String open() {
        try {
            writer = Files.newBufferedWriter(path, FixedWidth.CHARSET, StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE);
            return FileStatus.OK;
        } catch (IOException e) {
            return FileStatus.PERMANENT_ERROR;
        }
    }

    @Override
    public String write(T record) {
        if (writer == null) {
            return FileStatus.WRITE_NOT_OPEN;
        }
        try {
            writer.write(codec.encode(record));
            writer.write('\n');
            return FileStatus.OK;
        } catch (IOException e) {
            return FileStatus.PERMANENT_ERROR;
        }
    }

    @Override
    public String close() {
        if (writer == null) {
            return FileStatus.CLOSE_NOT_OPEN;
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
