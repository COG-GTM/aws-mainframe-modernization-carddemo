package com.carddemo.programs.cbimport.io;

import com.carddemo.programs.cbimport.FileStatus;
import com.carddemo.programs.cbimport.copybook.EbcdicNewlines;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.channels.SeekableByteChannel;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.Charset;
import java.nio.charset.CodingErrorAction;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.FileAttribute;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.EnumSet;

/**
 * RECFM=FB output file. Records are encoded with {@code charset} (IBM037 for mainframe-identical
 * bytes, US-ASCII for the app/data/ASCII form) and followed by {@code recordSeparator} (empty for
 * true FB, "\n" for line-sequential).
 *
 * <p>Like DISP=NEW, the file must not already exist (an existing file or symlink fails the OPEN), and
 * on POSIX systems it is created readable by the owner only. Each record is written straight to the
 * channel so an out-of-space condition fails the WRITE, as an Sx37 abend would on z/OS; a partially
 * written record is truncated away so later records keep their boundaries. In line-sequential mode a
 * record containing CR or LF is rejected rather than split across lines.
 */
public final class FixedWidthRecordFileWriter implements RecordWriter {

    private static final FileAttribute<?>[] OWNER_ONLY = {
            PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rw-------"))};

    private final Path path;
    private final int recordLength;
    private final Charset charset;
    private final byte[] separator;
    private final boolean collapsesNewlines;
    private SeekableByteChannel out;
    private boolean created;

    public FixedWidthRecordFileWriter(Path path, int recordLength, Charset charset, String recordSeparator) {
        this.path = path;
        this.recordLength = recordLength;
        this.charset = charset;
        this.separator = recordSeparator.getBytes(charset);
        this.collapsesNewlines = EbcdicNewlines.collapses(charset);
    }

    public Path path() {
        return path;
    }

    /** True once {@link #open()} has created the file, i.e. it is ours to delete on ABEND. */
    public boolean created() {
        return created;
    }

    @Override
    public String open() {
        var options = EnumSet.of(StandardOpenOption.WRITE, StandardOpenOption.CREATE_NEW);
        boolean posix = path.getFileSystem().supportedFileAttributeViews().contains("posix");
        try {
            out = Files.newByteChannel(path, options, posix ? OWNER_ONLY : new FileAttribute<?>[0]);
            created = true;
            return FileStatus.OK;
        } catch (NoSuchFileException e) {
            return FileStatus.FILE_NOT_FOUND;
        } catch (IOException e) {
            return FileStatus.PERMANENT_ERROR;
        }
    }

    @Override
    public String write(String record) {
        if (record.length() != recordLength) {
            return FileStatus.BOUNDARY_VIOLATION;
        }
        if (separator.length > 0 && (record.indexOf('\n') >= 0 || record.indexOf('\r') >= 0)) {
            return FileStatus.PERMANENT_ERROR;
        }
        ByteBuffer encoded;
        try {
            encoded = charset.newEncoder()
                    .onMalformedInput(CodingErrorAction.REPORT)
                    .onUnmappableCharacter(CodingErrorAction.REPORT)
                    .encode(CharBuffer.wrap(record));
        } catch (CharacterCodingException e) {
            return FileStatus.PERMANENT_ERROR;
        }
        if (encoded.remaining() != recordLength) {
            return FileStatus.BOUNDARY_VIOLATION;
        }
        if (collapsesNewlines) {
            EbcdicNewlines.restoreLineFeeds(record, encoded);
        }
        ByteBuffer buf = ByteBuffer.allocate(recordLength + separator.length).put(encoded).put(separator).flip();
        long start = -1;
        try {
            start = out.position();
            while (buf.hasRemaining()) {
                out.write(buf);
            }
            return FileStatus.OK;
        } catch (IOException e) {
            discardPartialRecord(start);
            return FileStatus.PERMANENT_ERROR;
        }
    }

    private void discardPartialRecord(long start) {
        if (start < 0) {
            return;
        }
        try {
            out.truncate(start);
            out.position(start);
        } catch (IOException ignored) {
            // the WRITE already reports the failure
        }
    }

    @Override
    public String close() {
        try {
            if (out != null) {
                out.close();
            }
            return FileStatus.OK;
        } catch (IOException e) {
            return FileStatus.PERMANENT_ERROR;
        }
    }
}
