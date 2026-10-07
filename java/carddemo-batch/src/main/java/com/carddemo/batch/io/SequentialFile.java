package com.carddemo.batch.io;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;

/**
 * A fixed-width {@code ORGANIZATION SEQUENTIAL} input file (a QSAM dataset such as DALYTRAN): records are
 * returned in file order. The backing file holds back-to-back records, optionally one per line as in
 * {@code app/data/ASCII/dailytran.txt}. File status semantics: OPEN of a missing file is {@code 35}, a
 * trailing fragment shorter than a record is {@code 39}, end of file is {@code 10}.
 */
public final class SequentialFile {

    private final String ddname;
    private final Path path;
    private final int recordLength;
    private List<byte[]> records;
    private Iterator<byte[]> cursor;

    public SequentialFile(String ddname, Path path, int recordLength) {
        this.ddname = ddname;
        this.path = path;
        this.recordLength = recordLength;
    }

    public boolean isOpen() {
        return records != null;
    }

    /** OPEN INPUT: loads the file. */
    public void open() {
        byte[] bytes;
        try {
            bytes = Files.readAllBytes(path);
        } catch (NoSuchFileException e) {
            throw new FileStatusException(ddname, "OPEN", FileStatusException.FILE_NOT_FOUND, e);
        } catch (IOException e) {
            throw new FileStatusException(ddname, "OPEN", FileStatusException.PERMANENT_ERROR, e);
        }
        records = RecordSplitter.split(ddname, bytes, recordLength, RecordSplitter.Format.FIXED);
        cursor = records.iterator();
    }

    /** Sequential READ: the next record in file order, or empty at end of file (status 10). */
    public Optional<byte[]> readNext() {
        requireOpen("READ");
        if (!cursor.hasNext()) {
            return Optional.empty();
        }
        return Optional.of(cursor.next().clone());
    }

    public int recordCount() {
        requireOpen("READ");
        return records.size();
    }

    public void close() {
        requireOpen("CLOSE");
        records = null;
        cursor = null;
    }

    private void requireOpen(String operation) {
        if (records == null) {
            throw new FileStatusException(ddname, operation, FileStatusException.NOT_OPEN);
        }
    }
}
