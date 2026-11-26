package com.carddemo.programs.cbimport.io;

import com.carddemo.programs.cbimport.FileStatus;
import com.carddemo.programs.cbimport.copybook.ExportRecord;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Reads an unloaded EXPORT KSDS (fixed 500-byte records, no delimiters, already in key order such as
 * app/data/EBCDIC/AWS.M2.CARDDEMO.EXPORT.DATA.PS).
 */
public final class FixedWidthExportFileReader implements ExportRecordReader {

    private final Path path;
    private InputStream in;

    public FixedWidthExportFileReader(Path path) {
        this.path = path;
    }

    @Override
    public String open() {
        if (path == null || !Files.isRegularFile(path)) {
            return FileStatus.FILE_NOT_FOUND;
        }
        try {
            in = new BufferedInputStream(Files.newInputStream(path));
            return FileStatus.OK;
        } catch (IOException e) {
            return FileStatus.PERMANENT_ERROR;
        }
    }

    @Override
    public ReadResult read() {
        try {
            byte[] buf = in.readNBytes(ExportRecord.LENGTH);
            if (buf.length == 0) {
                return new ReadResult(FileStatus.EOF, null);
            }
            if (buf.length != ExportRecord.LENGTH) {
                return new ReadResult(FileStatus.RECORD_LENGTH_CONFLICT, null);
            }
            return new ReadResult(FileStatus.OK, buf);
        } catch (IOException e) {
            return new ReadResult(FileStatus.PERMANENT_ERROR, null);
        }
    }

    @Override
    public String close() {
        try {
            if (in != null) {
                in.close();
            }
            return FileStatus.OK;
        } catch (IOException e) {
            return FileStatus.PERMANENT_ERROR;
        }
    }
}
