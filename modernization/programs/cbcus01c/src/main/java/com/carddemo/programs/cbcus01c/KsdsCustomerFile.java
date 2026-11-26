package com.carddemo.programs.cbcus01c;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;

/**
 * File-backed CUSTFILE presented in ascending FD-CUST-ID order like a KSDS read with ACCESS MODE
 * SEQUENTIAL. Two layouts are supported:
 * <ul>
 *   <li>line sequential (default): one record per line, as in app/data/ASCII/custdata.txt; short
 *       lines are space padded and long lines truncated to 500 bytes;</li>
 *   <li>fixed block ({@link #fixedBlock}): consecutive 500-byte records with no delimiters, as in
 *       the RECFM=FB feed app/data/EBCDIC/AWS.M2.CARDDEMO.CUSTDATA.PS (charset IBM037). A file whose
 *       length is not a multiple of 500 fails OPEN with status '39'.</li>
 * </ul>
 * Records are byte-oriented, so only single-byte charsets are accepted. Bytes the charset cannot
 * decode fail OPEN with status '30' instead of being replaced.
 */
public final class KsdsCustomerFile implements CustomerFile {

    private final Path path;
    private final Charset charset;
    private final boolean fixedBlock;
    private Iterator<FdCustfileRec> cursor;

    public KsdsCustomerFile(Path path) {
        this(path, StandardCharsets.US_ASCII);
    }

    public KsdsCustomerFile(Path path, Charset charset) {
        this(path, charset, false);
    }

    private KsdsCustomerFile(Path path, Charset charset, boolean fixedBlock) {
        if (charset.newEncoder().maxBytesPerChar() > 1.0f) {
            throw new IllegalArgumentException("CUSTFILE charset must be single-byte (1 char = 1 byte): " + charset);
        }
        this.path = path;
        this.charset = charset;
        this.fixedBlock = fixedBlock;
    }

    /** Fixed-block (RECFM=FB, LRECL=500) file with no record delimiters. */
    public static KsdsCustomerFile fixedBlock(Path path, Charset charset) {
        return new KsdsCustomerFile(path, charset, true);
    }

    @Override
    public String open() {
        if (cursor != null) {
            return "41";
        }
        try {
            List<FdCustfileRec> records = new ArrayList<>();
            if (fixedBlock) {
                byte[] bytes = Files.readAllBytes(path);
                if (bytes.length % FdCustfileRec.RECORD_LENGTH != 0) {
                    return "39";
                }
                CharsetDecoder decoder = charset.newDecoder()
                        .onMalformedInput(CodingErrorAction.REPORT)
                        .onUnmappableCharacter(CodingErrorAction.REPORT);
                for (int off = 0; off < bytes.length; off += FdCustfileRec.RECORD_LENGTH) {
                    records.add(FdCustfileRec.fromFixedWidth(decoder.decode(
                            ByteBuffer.wrap(bytes, off, FdCustfileRec.RECORD_LENGTH)).toString()));
                }
            } else {
                for (String line : Files.readAllLines(path, charset)) {
                    if (!line.isEmpty()) {
                        records.add(FdCustfileRec.fromFixedWidth(line));
                    }
                }
            }
            // KSDS key sequence is the unsigned byte order of the key in the file's encoding.
            records.sort(Comparator.comparing((FdCustfileRec r) -> r.fdCustId().getBytes(charset), Arrays::compareUnsigned));
            cursor = records.iterator();
            return FileStatus.SUCCESS;
        } catch (NoSuchFileException e) {
            return FileStatus.FILE_NOT_FOUND;
        } catch (IOException e) {
            return "30";
        }
    }

    @Override
    public ReadResult readNext() {
        if (cursor == null) {
            return ReadResult.status(FileStatus.LOGIC_ERROR_READ_NOT_OPEN);
        }
        return cursor.hasNext() ? ReadResult.ok(cursor.next()) : ReadResult.status(FileStatus.END_OF_FILE);
    }

    @Override
    public String close() {
        if (cursor == null) {
            return FileStatus.NOT_OPEN;
        }
        cursor = null;
        return FileStatus.SUCCESS;
    }
}
