package com.carddemo.programs.cbtrn01c.io;

import com.carddemo.programs.cbtrn01c.copybook.Pic;
import com.carddemo.programs.cbtrn01c.copybook.RecordCodec;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Line-per-record fixed-width file (ASCII export of a RECFM=F dataset). Short lines are
 * space-padded to the record length (trailing spaces are trimmed by the export); long lines
 * report status 04 (record length mismatch).
 */
public final class FixedWidthSequentialFile<R> implements SequentialInputFile<R> {

    private final Path path;
    private final RecordCodec<R> codec;
    private BufferedReader reader;

    public FixedWidthSequentialFile(Path path, RecordCodec<R> codec) {
        this.path = path;
        this.codec = codec;
    }

    @Override
    public String open() {
        if (!Files.isRegularFile(path)) {
            return FileStatus.FILE_NOT_FOUND;
        }
        try {
            reader = Files.newBufferedReader(path, StandardCharsets.ISO_8859_1);
            return FileStatus.OK;
        } catch (IOException e) {
            return FileStatus.PERMANENT_ERROR;
        }
    }

    @Override
    public ReadResult<R> read() {
        if (reader == null) {
            return ReadResult.of(FileStatus.READ_NOT_OPEN);
        }
        String line;
        try {
            line = reader.readLine();
        } catch (IOException e) {
            return ReadResult.of(FileStatus.PERMANENT_ERROR);
        }
        if (line == null) {
            return ReadResult.of(FileStatus.END_OF_FILE);
        }
        if (line.length() > codec.length()) {
            return ReadResult.of(FileStatus.WRONG_LENGTH);
        }
        String image = Pic.alpha(line, codec.length());
        return new ReadResult<>(FileStatus.OK, codec.parse(image), image);
    }

    @Override
    public String close() {
        if (reader == null) {
            return FileStatus.CLOSE_NOT_OPEN;
        }
        try {
            reader.close();
            return FileStatus.OK;
        } catch (IOException e) {
            return FileStatus.PERMANENT_ERROR;
        } finally {
            reader = null;
        }
    }
}
