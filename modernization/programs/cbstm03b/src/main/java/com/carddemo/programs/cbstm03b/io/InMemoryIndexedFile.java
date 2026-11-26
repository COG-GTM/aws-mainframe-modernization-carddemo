package com.carddemo.programs.cbstm03b.io;

import com.carddemo.programs.cbstm03b.copybook.FileStatus;

import java.io.IOException;
import java.nio.file.NoSuchFileException;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

/**
 * In-memory KSDS: fixed-length records keyed by a byte range, held in ascending EBCDIC key order
 * ({@link EbcdicCollation}). A deferred file loads its dataset on OPEN, so load failures surface
 * as OPEN file statuses instead of exceptions.
 */
public final class InMemoryIndexedFile implements IndexedFile {

    /** Supplies the dataset's records when the file is first opened. */
    @FunctionalInterface
    public interface Loader {
        List<String> load() throws IOException;
    }

    private static final FileStatus PERMANENT_ERROR = FileStatus.of("30");
    private static final FileStatus ATTRIBUTE_CONFLICT = FileStatus.of("39");

    private final String name;
    private final int recordLength;
    private final int keyOffset;
    private final int keyLength;
    private final Loader loader;

    private TreeMap<String, String> records;
    private boolean open;
    private boolean atEnd;
    private Iterator<Map.Entry<String, String>> cursor;

    private InMemoryIndexedFile(String name, int recordLength, int keyOffset, int keyLength, Loader loader) {
        this.name = name;
        this.recordLength = recordLength;
        this.keyOffset = keyOffset;
        this.keyLength = keyLength;
        this.loader = loader;
    }

    /** Eagerly loaded; invalid records are rejected with {@link IllegalArgumentException}. */
    public static InMemoryIndexedFile of(String name, int recordLength, int keyOffset, int keyLength,
                                         List<String> records) {
        InMemoryIndexedFile f = new InMemoryIndexedFile(name, recordLength, keyOffset, keyLength, () -> records);
        f.records = f.index(records);
        return f;
    }

    /**
     * Loaded on first OPEN. Missing dataset = 35, unreadable = 30, wrong record length or
     * duplicate key = 39.
     */
    public static InMemoryIndexedFile deferred(String name, int recordLength, int keyOffset, int keyLength,
                                               Loader loader) {
        return new InMemoryIndexedFile(name, recordLength, keyOffset, keyLength, loader);
    }

    /** A DD whose dataset is not allocated: OPEN returns status 35. */
    public static InMemoryIndexedFile missing(String name, int recordLength, int keyOffset, int keyLength) {
        return deferred(name, recordLength, keyOffset, keyLength, () -> {
            throw new NoSuchFileException(name);
        });
    }

    private TreeMap<String, String> index(List<String> data) {
        TreeMap<String, String> map = new TreeMap<>(EbcdicCollation.ORDER);
        int recordNo = 0;
        for (String rec : data) {
            recordNo++;
            if (rec.length() != recordLength) {
                throw new IllegalArgumentException(name + ": record " + recordNo + " length " + rec.length()
                        + " != " + recordLength);
            }
            String key = rec.substring(keyOffset, keyOffset + keyLength);
            if (map.putIfAbsent(key, rec) != null) {
                // Key value deliberately omitted: keys hold card numbers and customer/account ids.
                throw new IllegalArgumentException(name + ": duplicate key at record " + recordNo);
            }
        }
        return map;
    }

    public String name() {
        return name;
    }

    public boolean isOpen() {
        return open;
    }

    @Override
    public FileStatus open() {
        if (open) {
            return FileStatus.ALREADY_OPEN;
        }
        if (records == null) {
            try {
                records = index(loader.load());
            } catch (NoSuchFileException e) {
                return FileStatus.FILE_NOT_FOUND;
            } catch (IOException e) {
                return PERMANENT_ERROR;
            } catch (IllegalArgumentException e) {
                return ATTRIBUTE_CONFLICT;
            }
        }
        open = true;
        atEnd = false;
        cursor = records.entrySet().iterator();
        return FileStatus.SUCCESS;
    }

    @Override
    public FileStatus close() {
        if (!open) {
            return FileStatus.NOT_OPEN_ON_CLOSE;
        }
        open = false;
        cursor = null;
        return FileStatus.SUCCESS;
    }

    @Override
    public ReadResult readNext() {
        if (!open) {
            return ReadResult.of(FileStatus.NOT_OPEN_FOR_READ);
        }
        if (atEnd) {
            return ReadResult.of(FileStatus.READ_AFTER_END);
        }
        if (!cursor.hasNext()) {
            atEnd = true;
            return ReadResult.of(FileStatus.END_OF_FILE);
        }
        return new ReadResult(FileStatus.SUCCESS, cursor.next().getValue());
    }

    @Override
    public ReadResult readByKey(String key) {
        if (!open) {
            return ReadResult.of(FileStatus.NOT_OPEN_FOR_READ);
        }
        if (key == null || key.length() != keyLength) {
            throw new IllegalArgumentException(name + ": key must be " + keyLength + " bytes");
        }
        String rec = records.get(key);
        return rec == null ? ReadResult.of(FileStatus.RECORD_NOT_FOUND) : new ReadResult(FileStatus.SUCCESS, rec);
    }
}
