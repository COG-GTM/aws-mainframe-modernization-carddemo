package com.carddemo.programs.cbact01c.io;

import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;

/**
 * RECFM=V sequential output ({@code RECORD IS VARYING IN SIZE FROM min TO max}). Each record is preceded by a
 * 4-byte header: big-endian 2-byte length then two zero bytes.
 */
public final class VariableRecordFileWriter implements RecordWriter {

    /** How the 2-byte length in the record header is counted. */
    public enum HeaderFormat {
        /** z/OS RDW: length includes the 4-byte header. */
        IBM_RDW(4),
        /** GnuCOBOL COB_VARSEQ_FORMAT=0: length of the data only. */
        GNUCOBOL_VARSEQ_0(0);

        private final int headerAdjust;

        HeaderFormat(int headerAdjust) {
            this.headerAdjust = headerAdjust;
        }
    }

    private final Path path;
    private final int minLength;
    private final int maxLength;
    private final HeaderFormat format;
    private OutputStream out;
    private boolean created;

    public VariableRecordFileWriter(Path path, int minLength, int maxLength, HeaderFormat format) {
        this.path = path;
        this.minLength = minLength;
        this.maxLength = maxLength;
        this.format = format;
    }

    @Override
    public String open() {
        try {
            // DISP=NEW: never replace or follow an existing file or link.
            out = new BufferedOutputStream(
                    Files.newOutputStream(path, StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE));
            created = true;
            return FileStatus.OK;
        } catch (IOException e) {
            return FileStatus.PERMANENT_ERROR;
        }
    }

    /** True once {@link #open()} has created the file, so only this job's output is deleted on abend. */
    public boolean created() {
        return created;
    }

    public Path path() {
        return path;
    }

    @Override
    public String write(byte[] record) {
        if (out == null) {
            return FileStatus.NOT_OPEN_FOR_WRITE;
        }
        if (record.length < minLength || record.length > maxLength) {
            return FileStatus.RECORD_SIZE_INVALID;
        }
        int len = record.length + format.headerAdjust;
        try {
            out.write(new byte[] {(byte) (len >> 8), (byte) len, 0, 0});
            out.write(record);
            return FileStatus.OK;
        } catch (IOException e) {
            return FileStatus.PERMANENT_ERROR;
        }
    }

    @Override
    public String close() {
        if (out == null) {
            return FileStatus.NOT_OPEN_FOR_CLOSE;
        }
        try {
            out.close();
            return FileStatus.OK;
        } catch (IOException e) {
            return FileStatus.PERMANENT_ERROR;
        } finally {
            out = null;
        }
    }
}
