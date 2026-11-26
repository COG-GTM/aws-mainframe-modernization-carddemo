package com.carddemo.programs.cbact02c.io;

import com.carddemo.programs.cbact02c.copybook.FdCardfileRec;

import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

/**
 * CARDFILE backed by a fixed-width unload of AWS.M2.CARDDEMO.CARDDATA.VSAM.KSDS, 150 bytes per
 * record laid out per CVACT02Y. Two record formats are supported:
 * {@link RecordFormat#LINE} (one record per text line, e.g. app/data/ASCII/carddata.txt) and
 * {@link RecordFormat#FIXED_BLOCK} (RECFM=FB LRECL=150 with no delimiters, e.g.
 * app/data/EBCDIC/AWS.M2.CARDDEMO.CARDDATA.PS, decoded with IBM037).
 * Records are served in key order. A missing file gives status 35; any other I/O failure 30.
 */
public class FixedWidthCardFile implements CardFile {

    public static final Charset EBCDIC = Charset.forName("IBM037");

    public enum RecordFormat { LINE, FIXED_BLOCK }

    private final Path path;
    private final Charset charset;
    private final RecordFormat format;
    private InMemoryCardFile delegate;

    public FixedWidthCardFile(Path path) {
        this(path, StandardCharsets.ISO_8859_1, RecordFormat.LINE);
    }

    public FixedWidthCardFile(Path path, Charset charset, RecordFormat format) {
        this.path = path;
        this.charset = charset;
        this.format = format;
    }

    /** RECFM=FB EBCDIC dataset, as shipped in app/data/EBCDIC. */
    public static FixedWidthCardFile fixedBlockEbcdic(Path path) {
        return new FixedWidthCardFile(path, EBCDIC, RecordFormat.FIXED_BLOCK);
    }

    @Override
    public String open() {
        if (delegate != null) {
            return FileStatus.ALREADY_OPEN;
        }
        if (Files.notExists(path)) {
            return FileStatus.FILE_NOT_FOUND;
        }
        try {
            delegate = new InMemoryCardFile(format == RecordFormat.LINE
                    ? Files.readAllLines(path, charset)
                    : splitFixedBlock(Files.readAllBytes(path)));
        } catch (IOException e) {
            return FileStatus.PERMANENT_ERROR;
        }
        return delegate.open();
    }

    private List<String> splitFixedBlock(byte[] data) {
        int lrecl = FdCardfileRec.RECORD_LENGTH;
        List<String> records = new ArrayList<>();
        for (int offset = 0; offset < data.length; offset += lrecl) {
            int length = Math.min(lrecl, data.length - offset);
            records.add(new String(data, offset, length, charset));
        }
        return records;
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
