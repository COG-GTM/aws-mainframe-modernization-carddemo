package com.carddemo.batch.io;

import com.carddemo.batch.codec.FixedWidth;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;
import java.util.Iterator;
import java.util.Optional;
import java.util.TreeMap;

/**
 * A VSAM KSDS stand-in: fixed-width records loaded from a file (either back-to-back or, as in
 * {@code app/data/ASCII}, one record per line) and kept ordered by the bytes of the primary key, so a
 * sequential READ returns records in ascending key order and a random READ finds a record by key.
 * File status semantics follow COBOL: OPEN of a missing file is {@code 35}, a record whose length does
 * not match the layout is {@code 39}, a duplicate key is {@code 22}, end of file is {@code 10}.
 */
public final class KsdsFile {

    private final String ddname;
    private final Path path;
    private final int recordLength;
    private final int keyOffset;
    private final int keyLength;
    private TreeMap<String, byte[]> records;
    private Iterator<byte[]> cursor;

    public KsdsFile(String ddname, Path path, int recordLength, int keyOffset, int keyLength) {
        this.ddname = ddname;
        this.path = path;
        this.recordLength = recordLength;
        this.keyOffset = keyOffset;
        this.keyLength = keyLength;
    }

    public boolean isOpen() {
        return records != null;
    }

    /** OPEN INPUT: loads and indexes the file. */
    public void open() {
        byte[] bytes;
        try {
            bytes = Files.readAllBytes(path);
        } catch (NoSuchFileException e) {
            throw new FileStatusException(ddname, "OPEN", FileStatusException.FILE_NOT_FOUND, e);
        } catch (IOException e) {
            throw new FileStatusException(ddname, "OPEN", FileStatusException.PERMANENT_ERROR, e);
        }
        TreeMap<String, byte[]> loaded = new TreeMap<>();
        int pos = 0;
        while (pos < bytes.length) {
            if (bytes.length - pos < recordLength) {
                throw new FileStatusException(ddname, "OPEN", FileStatusException.ATTRIBUTE_MISMATCH,
                        new IOException("trailing " + (bytes.length - pos) + " bytes are not a " + recordLength
                                + "-byte record"));
            }
            byte[] rec = new byte[recordLength];
            System.arraycopy(bytes, pos, rec, 0, recordLength);
            pos += recordLength;
            if (pos < bytes.length && bytes[pos] == '\n') {
                pos++;
            }
            String key = new String(rec, keyOffset, keyLength, FixedWidth.CHARSET);
            if (loaded.put(key, rec) != null) {
                throw new FileStatusException(ddname, "OPEN", FileStatusException.DUPLICATE_KEY,
                        new IOException("duplicate key " + key));
            }
        }
        records = loaded;
        cursor = loaded.values().iterator();
    }

    /** Sequential READ: the next record in key order, or empty at end of file (status 10). */
    public Optional<byte[]> readNext() {
        requireOpen("READ");
        if (!cursor.hasNext()) {
            return Optional.empty();
        }
        return Optional.of(cursor.next().clone());
    }

    /** Random READ by key: empty when the key does not exist (status 23). */
    public Optional<byte[]> read(String key) {
        requireOpen("READ");
        byte[] rec = records.get(key);
        return rec == null ? Optional.empty() : Optional.of(rec.clone());
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
