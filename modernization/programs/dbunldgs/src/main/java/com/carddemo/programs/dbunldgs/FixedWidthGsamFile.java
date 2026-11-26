package com.carddemo.programs.dbunldgs;

import com.carddemo.programs.dbunldgs.copybook.ImsFunctionCodes;
import com.carddemo.programs.dbunldgs.copybook.PcbMask;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
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
 * owner-only (0600) on POSIX file systems because they carry card and account data.
 */
public final class FixedWidthGsamFile implements DliCallHandler, AutoCloseable {

    private static final Set<OpenOption> OPEN_OPTIONS = Set.of(StandardOpenOption.CREATE, StandardOpenOption.WRITE);

    private final int recordLength;
    private final FileChannel channel;

    public FixedWidthGsamFile(Path path, int recordLength) {
        this.recordLength = recordLength;
        try {
            this.channel = open(path);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    private static FileChannel open(Path path) throws IOException {
        if (path.getFileSystem().supportedFileAttributeViews().contains("posix")) {
            return FileChannel.open(path, OPEN_OPTIONS,
                    PosixFilePermissions.asFileAttribute(PosixFilePermissions.fromString("rw-------")));
        }
        return FileChannel.open(path, OPEN_OPTIONS);
    }

    public void truncate() throws IOException {
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
        try {
            ByteBuffer buf = ByteBuffer.wrap(rec);
            while (buf.hasRemaining()) {
                channel.write(buf);
            }
            pcb.setPcbStatus("  ");
        } catch (IOException e) {
            pcb.setPcbStatus("AO");
        }
    }

    @Override
    public void close() throws IOException {
        channel.close();
    }
}
