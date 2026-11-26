package com.carddemo.programs.cbact02c.io;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * CARDFILE backed by a line-per-record fixed-width text file (the ASCII unload of
 * AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS, 150 bytes per record laid out per CVACT02Y).
 * Records are served in key order. Missing file gives status 35, an I/O failure 30.
 */
public class FixedWidthCardFile implements CardFile {

    private final Path path;
    private final Charset charset;
    private InMemoryCardFile delegate;

    public FixedWidthCardFile(Path path) {
        this(path, StandardCharsets.ISO_8859_1);
    }

    public FixedWidthCardFile(Path path, Charset charset) {
        this.path = path;
        this.charset = charset;
    }

    @Override
    public String open() {
        if (delegate != null) {
            return FileStatus.ALREADY_OPEN;
        }
        if (!Files.isRegularFile(path)) {
            return FileStatus.FILE_NOT_FOUND;
        }
        try {
            delegate = new InMemoryCardFile(Files.readAllLines(path, charset));
        } catch (IOException e) {
            return FileStatus.PERMANENT_ERROR;
        }
        return delegate.open();
    }

    @Override
    public ReadResult read() {
        if (delegate == null) {
            return new ReadResult(FileStatus.READ_NOT_OPEN, null);
        }
        return delegate.read();
    }

    @Override
    public String close() {
        if (delegate == null) {
            return FileStatus.CLOSE_NOT_OPEN;
        }
        String status = delegate.close();
        delegate = null;
        return status;
    }
}
