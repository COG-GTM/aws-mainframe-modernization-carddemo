package com.carddemo.programs.cbtrn03c.io;

import com.carddemo.programs.cbtrn03c.support.Fixed;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.channels.Channels;
import java.nio.channels.SeekableByteChannel;
import java.nio.charset.CharsetEncoder;
import java.nio.charset.CodingErrorAction;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.OpenOption;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.FileAttribute;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.Set;

/**
 * Sequential output data set of fixed-length records (records are space-padded/truncated to LRECL).
 * OPEN replaces any existing entry (file or symbolic link, never its target) with a newly created file that
 * is owner-only (rw-------) on POSIX file systems, so handles to the old file never see the new report.
 * Characters that are not representable in ISO-8859-1 fail the WRITE instead of being substituted.
 */
public final class FixedWidthSequentialOutputFile implements SequentialOutputFile {
    private final Path path;
    private final int lrecl;
    private final RecordFormat format;
    private static final Set<OpenOption> OPEN_OPTIONS = Set.of(StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE,
            LinkOption.NOFOLLOW_LINKS);
    private static final Set<PosixFilePermission> OWNER_ONLY = PosixFilePermissions.fromString("rw-------");
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
            boolean posix = FileSystems.getDefault().supportedFileAttributeViews().contains("posix");
            FileAttribute<?>[] attrs = posix
                    ? new FileAttribute<?>[] {PosixFilePermissions.asFileAttribute(OWNER_ONLY)}
                    : new FileAttribute<?>[0];
            Files.deleteIfExists(path);
            SeekableByteChannel channel = Files.newByteChannel(path, OPEN_OPTIONS, attrs);
            CharsetEncoder encoder = FixedWidthRecords.CHARSET.newEncoder()
                    .onMalformedInput(CodingErrorAction.REPORT)
                    .onUnmappableCharacter(CodingErrorAction.REPORT);
            writer = new BufferedWriter(new OutputStreamWriter(Channels.newOutputStream(channel), encoder));
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
