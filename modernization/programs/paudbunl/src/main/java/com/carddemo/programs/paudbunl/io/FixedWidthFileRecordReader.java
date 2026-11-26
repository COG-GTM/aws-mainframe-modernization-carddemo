package com.carddemo.programs.paudbunl.io;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;

/** Reads a RECFM=FB file back as fixed-length records (e.g. OUTFIL1/OUTFIL2 for PAUDBLOD). */
public final class FixedWidthFileRecordReader implements RecordReader {

    private final InputStream in;
    private final int lrecl;

    public FixedWidthFileRecordReader(Path path, int lrecl) throws IOException {
        this.in = new BufferedInputStream(Files.newInputStream(path));
        this.lrecl = lrecl;
    }

    @Override
    public int recordLength() {
        return lrecl;
    }

    @Override
    public Optional<byte[]> read() {
        try {
            byte[] rec = in.readNBytes(lrecl);
            if (rec.length == 0) {
                return Optional.empty();
            }
            if (rec.length != lrecl) {
                throw new IllegalStateException("Short record: " + rec.length + " of " + lrecl + " bytes");
            }
            return Optional.of(rec);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    @Override
    public void close() {
        try {
            in.close();
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }
}
