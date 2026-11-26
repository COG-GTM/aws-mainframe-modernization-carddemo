package com.carddemo.programs.cbexport.io;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.channels.Channels;
import java.nio.file.Files;
import java.nio.file.OpenOption;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.FileAttribute;
import java.nio.file.attribute.PosixFileAttributeView;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.PosixFilePermissions;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Set;

/**
 * Writes RECFM=F records with no delimiters. When a key position is given it behaves like a
 * KSDS loaded with ACCESS MODE SEQUENTIAL: keys must be strictly ascending (unsigned byte
 * comparison) or the WRITE returns status 21.
 */
public final class FixedLengthRecordWriter implements RecordWriter {

    private final Path path;
    private final int recordLength;
    private final int keyOffset;
    private final int keyLength;
    // The export carries SSNs and CVVs: create it readable by the owner only.
    private static final Set<PosixFilePermission> OWNER_READ_WRITE = PosixFilePermissions.fromString("rw-------");
    private static final Set<OpenOption> OPTIONS = Set.of(
            StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE);

    private byte[] lastKey;
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
        try {
            out = Channels.newOutputStream(Files.newByteChannel(path, OPTIONS, ownerOnly()));
            if (Files.getFileStore(path).supportsFileAttributeView(PosixFileAttributeView.class)) {
                Files.setPosixFilePermissions(path, OWNER_READ_WRITE);
            }
            lastKey = null;
            return FileStatus.OK;
        } catch (IOException e) {
            return FileStatus.PERMANENT_ERROR;
        }
    }

    private FileAttribute<?>[] ownerOnly() throws IOException {
        Path dir = path.toAbsolutePath().getParent();
        if (dir != null && Files.isDirectory(dir)
                && Files.getFileStore(dir).supportsFileAttributeView(PosixFileAttributeView.class)) {
            return new FileAttribute<?>[] {PosixFilePermissions.asFileAttribute(OWNER_READ_WRITE)};
        }
        return new FileAttribute<?>[0];
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

    @Override
    public void close() {
        if (out != null) {
            try {
                out.close();
            } catch (IOException ignored) {
                // CLOSE status is not checked by CBEXPORT.
            }
            out = null;
        }
    }
}
