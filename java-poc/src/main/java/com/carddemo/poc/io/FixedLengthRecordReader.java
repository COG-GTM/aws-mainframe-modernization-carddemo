package com.carddemo.poc.io;

import java.io.BufferedInputStream;
import java.io.Closeable;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.util.Optional;

/**
 * Java stand-in for a COBOL {@code SELECT ... ORGANIZATION IS INDEXED ACCESS MODE IS SEQUENTIAL}
 * file opened for INPUT.
 *
 * <p>A VSAM KSDS read sequentially returns records in key order. The CardDemo sample data sets
 * ({@code app/data/EBCDIC/*.PS}, {@code app/data/ASCII/*.txt}) are the REPRO source for those
 * KSDS clusters and are already sorted by key, so reading the flat file sequentially is
 * behaviourally equivalent to {@code READ NEXT} against the KSDS.
 *
 * <p>Two physical encodings are supported:
 * <ul>
 *   <li>EBCDIC (CP037) fixed-block: no record separators, e.g. {@code AWS.M2.CARDDEMO.CARDXREF.PS}</li>
 *   <li>ASCII text: one record per line, e.g. {@code cardxref.txt}. Trailing spaces may have been
 *       stripped, so short lines are space-padded back to the record length (as REPRO would).</li>
 * </ul>
 * Records are handed back as ISO-8859-1 bytes/strings so that zoned-decimal overpunch
 * characters ({@code {}}, {@code }}, A-R) are identical regardless of the source encoding.
 *
 * <p>Mirrors the COBOL contract: every operation sets a {@link FileStatus} instead of throwing,
 * so the calling program can reproduce its {@code IF XREFFILE-STATUS = '00' ... ELSE ...} logic.
 */
public final class FixedLengthRecordReader implements Closeable {

    public static final Charset EBCDIC = Charset.forName("IBM037");

    private final Path path;
    private final int recordLength;
    private final Charset charset;
    private final boolean lineSeparated;

    private InputStream in;
    private FileStatus status = FileStatus.NOT_OPEN;

    public FixedLengthRecordReader(Path path, int recordLength, Charset charset, boolean lineSeparated) {
        this.path = path;
        this.recordLength = recordLength;
        this.charset = charset;
        this.lineSeparated = lineSeparated;
    }

    /**
     * Picks the encoding from the file name: {@code *.txt} is treated as ASCII line-separated
     * text, anything else (e.g. {@code *.PS}) as EBCDIC fixed-block.
     */
    public static FixedLengthRecordReader forDataFile(Path path, int recordLength) {
        boolean ascii = path.getFileName().toString().toLowerCase().endsWith(".txt");
        return ascii
                ? new FixedLengthRecordReader(path, recordLength, StandardCharsets.ISO_8859_1, true)
                : new FixedLengthRecordReader(path, recordLength, EBCDIC, false);
    }

    /** {@code OPEN INPUT file}. */
    public FileStatus open() {
        if (in != null) {
            return status = FileStatus.ALREADY_OPEN;
        }
        try {
            in = new BufferedInputStream(Files.newInputStream(path));
            return status = FileStatus.SUCCESS;
        } catch (NoSuchFileException e) {
            return status = FileStatus.FILE_NOT_FOUND;
        } catch (IOException e) {
            return status = FileStatus.IO_ERROR;
        }
    }

    /**
     * {@code READ file INTO record}. Returns the next record translated to ISO-8859-1, or
     * {@link Optional#empty()} with status {@code 10} at end of file.
     */
    public Optional<byte[]> readNext() {
        if (in == null) {
            status = FileStatus.READ_NOT_OPEN;
            return Optional.empty();
        }
        try {
            byte[] raw = lineSeparated ? readLine() : readFixedBlock();
            if (raw == null) {
                return Optional.empty();
            }
            status = FileStatus.SUCCESS;
            return Optional.of(translate(raw));
        } catch (IOException e) {
            status = FileStatus.IO_ERROR;
            return Optional.empty();
        }
    }

    private byte[] readFixedBlock() throws IOException {
        byte[] raw = in.readNBytes(recordLength);
        if (raw.length == 0) {
            status = FileStatus.END_OF_FILE;
            return null;
        }
        if (raw.length < recordLength) {
            // A short trailing record is a data error, not EOF.
            status = FileStatus.IO_ERROR;
            return null;
        }
        return raw;
    }

    private byte[] readLine() throws IOException {
        byte[] raw = new byte[recordLength];
        java.util.Arrays.fill(raw, (byte) ' ');
        int n = 0;
        int c = in.read();
        if (c == -1) {
            status = FileStatus.END_OF_FILE;
            return null;
        }
        while (c != -1 && c != '\n') {
            if (c != '\r') {
                if (n >= recordLength) {
                    status = FileStatus.IO_ERROR;
                    return null;
                }
                raw[n++] = (byte) c;
            }
            c = in.read();
        }
        return raw;
    }

    private byte[] translate(byte[] raw) {
        if (charset.equals(StandardCharsets.ISO_8859_1)) {
            return raw;
        }
        return new String(raw, charset).getBytes(StandardCharsets.ISO_8859_1);
    }

    /** {@code CLOSE file}. */
    @Override
    public void close() {
        if (in == null) {
            status = FileStatus.NOT_OPEN;
            return;
        }
        try {
            in.close();
            status = FileStatus.SUCCESS;
        } catch (IOException e) {
            status = FileStatus.IO_ERROR;
        } finally {
            in = null;
        }
    }

    /** Last FILE STATUS set by open/read/close. */
    public FileStatus status() {
        return status;
    }
}
