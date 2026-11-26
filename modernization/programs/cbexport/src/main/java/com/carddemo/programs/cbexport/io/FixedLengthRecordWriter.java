package com.carddemo.programs.cbexport.io;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.channels.Channels;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.FileAttribute;
import java.nio.file.attribute.PosixFileAttributeView;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.Arrays;
import java.util.Set;
import java.util.UUID;

/**
 * Writes RECFM=F records with no delimiters. When a key position is given it behaves like a
 * KSDS loaded with ACCESS MODE SEQUENTIAL: keys must be strictly ascending (unsigned byte
 * comparison) or the WRITE returns status 21.
 *
 * <p>The export carries SSNs and CVVs, so records go to a new owner-only ({@code rw-------} on
 * POSIX) temporary file in the target directory, which replaces the target on CLOSE. Like the
 * JCL's DELETE/DEFINE of the cluster, an existing target is never truncated in place: it keeps
 * its contents until CLOSE, and a symlink at the target is replaced rather than followed.
 */
public final class FixedLengthRecordWriter implements RecordWriter {

    private boolean publishFailed;
    private final Path path;
    private final int recordLength;
    private final int keyOffset;
    private final int keyLength;
    private byte[] lastKey;
    private Path temp;
    private OutputStream out;

    public FixedLengthRecordWriter(Path path, int recordLength) {
        this(path, recordLength, 0, 0);
    }

    public FixedLengthRecordWriter(Path path, int recordLength, int keyOffset, int keyLength) {
        this.path = path;
        this.recordLength = recordLength;
        this.keyOffset = keyOffset;
        this.keyLength = keyLength;
    }

    @Override
    public String open() {
        if (Files.isDirectory(path, LinkOption.NOFOLLOW_LINKS)) {
            return FileStatus.PERMANENT_ERROR;
        }
        try {
            Path dir = path.toAbsolutePath().getParent();
            FileAttribute<?>[] attrs = Files.getFileStore(dir).supportsFileAttributeView(PosixFileAttributeView.class)
                    ? new FileAttribute<?>[] {PosixFilePermissions.asFileAttribute(
                            PosixFilePermissions.fromString("rw-------"))}
                    : new FileAttribute<?>[0];
            // CREATE_NEW (O_EXCL) never follows a pre-planted link and the stream is the creating handle.
            temp = dir.resolve("." + path.getFileName() + "." + UUID.randomUUID() + ".tmp");
            out = Channels.newOutputStream(Files.newByteChannel(temp,
                    Set.of(StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE), attrs));
            lastKey = null;
            return FileStatus.OK;
        } catch (IOException | RuntimeException e) {
            discardTemp();
            return FileStatus.PERMANENT_ERROR;
        }
    }

    @Override
    public String write(byte[] record) {
        if (record.length != recordLength) {
            return FileStatus.RECORD_LENGTH_ERROR;
        }
        if (keyLength > 0) {
            byte[] key = Arrays.copyOfRange(record, keyOffset, keyOffset + keyLength);
            if (lastKey != null && Arrays.compareUnsigned(key, lastKey) <= 0) {
                return FileStatus.SEQUENCE_ERROR;
            }
            lastKey = key;
        }
        try {
            out.write(record);
            return FileStatus.OK;
        } catch (IOException e) {
            return FileStatus.PERMANENT_ERROR;
        }
    }

    /** True when the last CLOSE could not publish the export to its destination. */
    public boolean publishFailed() {
        return publishFailed;
    }

    @Override
    public void close() {
        if (out == null) {
            return;
        }
        try {
            out.close();
            Files.move(temp, path, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE);
            publishFailed = false;
        } catch (IOException e) {
            // CBEXPORT does not check CLOSE status; callers can query publishFailed().
            publishFailed = true;
        } finally {
            out = null;
            discardTemp();
        }
    }

    private void discardTemp() {
        if (temp != null) {
            try {
                Files.deleteIfExists(temp);
            } catch (IOException ignored) {
                // best effort
            }
            temp = null;
        }
    }
}
