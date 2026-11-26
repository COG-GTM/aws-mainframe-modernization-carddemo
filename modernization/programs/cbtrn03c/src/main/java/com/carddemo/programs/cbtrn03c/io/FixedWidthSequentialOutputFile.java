package com.carddemo.programs.cbtrn03c.io;

import com.carddemo.programs.cbtrn03c.support.Fixed;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.channels.Channels;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.OpenOption;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.FileAttribute;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.Set;

/**
 * Sequential output data set of fixed-length records (records are space-padded/truncated to LRECL).
 * The file is never opened through a symbolic link and, on POSIX file systems, is created owner-only.
 */
public final class FixedWidthSequentialOutputFile implements SequentialOutputFile {
    private final Path path;
    private final int lrecl;
    private final RecordFormat format;
    private static final Set<OpenOption> OPEN_OPTIONS = Set.of(StandardOpenOption.CREATE, StandardOpenOption.WRITE,
            StandardOpenOption.TRUNCATE_EXISTING, LinkOption.NOFOLLOW_LINKS);
    private BufferedWriter writer;
    private boolean opened;

    public FixedWidthSequentialOutputFile(Path path, int lrecl, RecordFormat format) {
        this.path = path;
        this.lrecl = lrecl;
        this.format = format;
    }

    @Override
    public String open() {
        try {
            FileAttribute<?>[] attrs = FileSystems.getDefault().supportedFileAttributeViews().contains("posix")
                    ? new FileAttribute<?>[] {PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rw-------"))}
                    : new FileAttribute<?>[0];
            writer = new BufferedWriter(Channels.newWriter(Files.newByteChannel(path, OPEN_OPTIONS, attrs),
                    FixedWidthRecords.CHARSET));
            opened = true;
            return FileStatus.OK;
        } catch (IOException e) {
            return FileStatus.PERMANENT_ERROR;
        }
    }

    /** True once this run has successfully opened (created or replaced) the data set. */
    public boolean wasOpened() {
        return opened;
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
