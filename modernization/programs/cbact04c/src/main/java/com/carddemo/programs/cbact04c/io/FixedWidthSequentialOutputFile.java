package com.carddemo.programs.cbact04c.io;

import com.carddemo.programs.cbact04c.copybook.RecordCodec;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.UncheckedIOException;
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
    private boolean created;

    public FixedWidthSequentialOutputFile(Path path, RecordCodec<T> codec) {
        this.path = path;
        this.codec = codec;
    }

    @Override
    public String open() {
        try {
            writer = Files.newBufferedWriter(path, FixedWidth.CHARSET, StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE);
            created = true;
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

    /** Deletes the output only if this instance created it (DISP=(NEW,CATLG,DELETE) on abend). */
    public void deleteIfCreated() {
        if (!created) {
            return;
        }
        if (writer != null) {
            try {
                writer.close();
            } catch (IOException ignored) {
                // the output is discarded anyway; a failed flush must not keep it
            }
            writer = null;
        }
        try {
            Files.deleteIfExists(path);
            created = false;
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }
}
