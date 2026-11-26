package com.carddemo.programs.dbunldgs;

import com.carddemo.programs.dbunldgs.copybook.ImsFunctionCodes;
import com.carddemo.programs.dbunldgs.copybook.PcbMask;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.channels.OverlappingFileLockException;
import java.nio.file.Files;
import java.nio.file.OpenOption;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.Set;

/**
 * GSAM output dataset backed by a RECFM=F file: each ISRT writes exactly {@code recordLength}
 * bytes (PASFLDBD RECORD=(100), PADFLDBD RECORD=(200)). I/O failures set status 'AO'.
 * <p>
 * Opening (allocation) does not discard existing content; call {@link #truncate()} once every
 * dataset of the step is allocated, mirroring DISP=OLD load-mode GSAM. New files are created
 * owner-only (0600) on POSIX file systems because they carry card and account data. Regular files
 * are locked exclusively while open, like the DISP=OLD enqueue in UNLDGSAM.
 */
public final class FixedWidthGsamFile implements DliCallHandler, AutoCloseable {

    private static final Set<OpenOption> OPEN_OPTIONS = Set.of(StandardOpenOption.CREATE, StandardOpenOption.WRITE);

    private final int recordLength;
    private final FileChannel channel;
    private final boolean regularFile;

    public FixedWidthGsamFile(Path path, int recordLength) {
        this.recordLength = recordLength;
        try {
            this.channel = open(path);
            this.regularFile = Files.isRegularFile(path);
            if (regularFile) {
                lockExclusive(channel, path);
            }
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    /** DISP=OLD: exclusive enqueue on the dataset for the life of the step. */
    private static void lockExclusive(FileChannel channel, Path path) throws IOException {
        FileLock lock;
        try {
            lock = channel.tryLock();
        } catch (OverlappingFileLockException e) {
            lock = null;
        }
        if (lock == null) {
            channel.close();
            throw new IOException("Dataset in use: " + path);
        }
    }

    private static FileChannel open(Path path) throws IOException {
        if (path.getFileSystem().supportedFileAttributeViews().contains("posix")) {
            return FileChannel.open(path, OPEN_OPTIONS,
                    PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rw-------")));
        }
        return FileChannel.open(path, OPEN_OPTIONS);
    }

    /** Empties a regular file; non-seekable targets (pipes, devices) are left as-is. */
    public void truncate() throws IOException {
        if (!regularFile) {
            return;
        }
        channel.truncate(0);
        channel.position(0);
    }

    @Override
    public void cbltdli(String function, PcbMask pcb, byte[] ioArea, String ssa) {
        if (!ImsFunctionCodes.FUNC_ISRT.equals(function)) {
            pcb.setPcbStatus("AD");
            return;
        }
        byte[] rec = new byte[recordLength];
        System.arraycopy(ioArea, 0, rec, 0, Math.min(recordLength, ioArea.length));
        long start = -1;
        try {
            if (regularFile) {
                start = channel.position();
            }
            ByteBuffer buf = ByteBuffer.wrap(rec);
            while (buf.hasRemaining()) {
                channel.write(buf);
            }
            pcb.setPcbStatus("  ");
        } catch (IOException e) {
            discardPartialRecord(start);
            pcb.setPcbStatus("AO");
        }
    }

    private void discardPartialRecord(long start) {
        if (start < 0) {
            return;
        }
        try {
            channel.truncate(start);
            channel.position(start);
        } catch (IOException ignored) {
            // status AO already reports the failed ISRT
        }
    }

    @Override
    public void close() throws IOException {
        channel.close();
    }
}
