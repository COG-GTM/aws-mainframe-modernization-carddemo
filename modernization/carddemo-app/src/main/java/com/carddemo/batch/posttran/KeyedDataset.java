package com.carddemo.batch.posttran;

import com.carddemo.common.codec.FixedWidthRecord;
import com.carddemo.common.codec.RecordEncoding;
import com.carddemo.common.data.CopybookRecordMapper;
import com.carddemo.common.file.FileStatus;
import com.carddemo.common.file.FileStatusException;
import com.carddemo.common.file.RecordFiles;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.TreeMap;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import org.springframework.dao.DataAccessException;

/**
 * A KSDS opened for random access ({@code ACCESS MODE IS RANDOM}): keyed {@code READ}, {@code WRITE} and
 * {@code REWRITE}. Either a file (loaded on {@code OPEN}, after-image written back in key byte order on {@code CLOSE}
 * for {@code I-O}/{@code OUTPUT}; {@code keyImage} is the key as it sits in the record) or a PostgreSQL table (every call goes to the repository, so it joins the caller's
 * database transaction).
 */
public interface KeyedDataset<K, D extends Record> {

    enum Mode { INPUT, I_O, OUTPUT }

    String ddname();

    void open();

    /** Keyed {@code READ}: empty on {@code INVALID KEY} (status 23). */
    Optional<D> read(K key);

    /** {@code WRITE}: status 22 when the key exists. */
    void write(D data);

    /** {@code REWRITE}: false on {@code INVALID KEY} (status 23). */
    boolean rewrite(D data);

    void close();

    /**
     * A KSDS unload file: fixed-length records for EBCDIC, line sequential for ASCII (the format the GnuCOBOL baseline
     * renders its after-images in). Records not rewritten keep their original bytes.
     */
    static <K, D extends Record> KeyedDataset<K, D> file(String ddname, Path path, Mode mode,
                                                         CopybookRecordMapper<D> mapper, Function<D, K> key,
                                                         Function<K, String> keyImage, RecordEncoding encoding) {
        return new FileDataset<>(ddname, path, mode, mapper, key, keyImage, encoding);
    }

    /** An in-memory KSDS holding {@code records} (tests). */
    static <K, D extends Record> KeyedDataset<K, D> memory(String ddname, CopybookRecordMapper<D> mapper,
                                                           Function<D, K> key, Function<K, String> keyImage,
                                                           List<D> records) {
        FileDataset<K, D> dataset = new FileDataset<>(ddname, null, Mode.I_O, mapper, key, keyImage,
                RecordEncoding.ASCII);
        records.forEach(r -> dataset.records.put(key.apply(r), mapper.toRecord(r, RecordEncoding.ASCII)));
        return dataset;
    }

    /**
     * A table: {@code find} reads by key, {@code insert}/{@code update} write ({@code update} returns false when the row
     * is missing), {@code clear} empties the table on {@code OPEN OUTPUT}.
     */
    static <K, D extends Record> KeyedDataset<K, D> table(String ddname, Mode mode, Function<K, Optional<D>> find,
                                                          Predicate<K> exists, Consumer<D> insert,
                                                          Predicate<D> update, Runnable clear, Function<D, K> key) {
        return new TableDataset<>(ddname, mode, find, exists, insert, update, clear, key);
    }

    /** The current records in key order (tests and file after-images). */
    default List<D> contents() {
        throw new UnsupportedOperationException(ddname() + ": contents() is only available for file datasets");
    }

    final class FileDataset<K, D extends Record> implements KeyedDataset<K, D> {

        private final String ddname;
        private final Path path;
        private final Mode mode;
        private final CopybookRecordMapper<D> mapper;
        private final Function<D, K> key;
        private final RecordEncoding encoding;
        private final TreeMap<K, FixedWidthRecord> records;
        private byte[] recordArea;
        private boolean open;

        private FileDataset(String ddname, Path path, Mode mode, CopybookRecordMapper<D> mapper, Function<D, K> key,
                            Function<K, String> keyImage, RecordEncoding encoding) {
            this.ddname = ddname;
            this.path = path;
            this.mode = mode;
            this.mapper = mapper;
            this.key = key;
            this.encoding = encoding;
            this.records = new TreeMap<>(Comparator.comparing((K k) -> encoding.encode(keyImage.apply(k)),
                    Arrays::compareUnsigned));
        }

        @Override
        public String ddname() {
            return ddname;
        }

        @Override
        public void open() {
            if (open) {
                throw new FileStatusException(ddname, "OPEN", FileStatus.ALREADY_OPEN);
            }
            if (path != null) {
                records.clear();
                if (mode != Mode.OUTPUT) {
                    List<FixedWidthRecord> all = encoding == RecordEncoding.EBCDIC
                            ? RecordFiles.readFixed(ddname, path, mapper.layout(), encoding)
                            : RecordFiles.readLines(ddname, path, mapper.layout(), encoding);
                    for (FixedWidthRecord r : all) {
                        if (!mapper.isLowValues(r)) {
                            records.put(key.apply(mapper.fromRecord(r)), r);
                        }
                    }
                } else if (path.getParent() != null && !Files.isDirectory(path.toAbsolutePath().getParent())) {
                    throw new FileStatusException(ddname, "OPEN", FileStatus.FILE_NOT_FOUND);
                }
            }
            open = true;
        }

        @Override
        public Optional<D> read(K k) {
            requireOpen("READ");
            FixedWidthRecord r = records.get(k);
            if (r == null) {
                return Optional.empty();
            }
            recordArea = r.bytes().clone();
            return Optional.of(mapper.fromRecord(r));
        }

        @Override
        public void write(D data) {
            requireOpen("WRITE");
            if (mode == Mode.INPUT) {
                throw new FileStatusException(ddname, "WRITE", FileStatus.NOT_OPEN_OUTPUT);
            }
            K k = key.apply(data);
            if (records.containsKey(k)) {
                throw new FileStatusException(ddname, "WRITE", FileStatus.DUPLICATE_KEY);
            }
            records.put(k, image(data));
        }

        /**
         * The program's {@code READ ... INTO} working-storage area: unmapped bytes (FILLER) keep what the last
         * successful READ moved there, as {@code INITIALIZE} and field MOVEs never touch them.
         */
        private FixedWidthRecord image(D data) {
            if (recordArea == null) {
                return mapper.toRecord(data, encoding);
            }
            FixedWidthRecord record = new FixedWidthRecord(mapper.layout(), recordArea.clone(), encoding);
            mapper.writeInto(data, record);
            return record;
        }

        @Override
        public boolean rewrite(D data) {
            requireOpen("REWRITE");
            if (mode != Mode.I_O) {
                throw new FileStatusException(ddname, "REWRITE", FileStatus.NOT_OPEN_IO);
            }
            K k = key.apply(data);
            if (!records.containsKey(k)) {
                return false;
            }
            records.put(k, image(data));
            return true;
        }

        @Override
        public void close() {
            requireOpen("CLOSE");
            open = false;
            if (path == null || mode == Mode.INPUT) {
                return;
            }
            List<FixedWidthRecord> all = new ArrayList<>(records.values());
            if (encoding == RecordEncoding.EBCDIC) {
                RecordFiles.writeFixed(ddname, path, all);
            } else {
                RecordFiles.writeLines(ddname, path, all, false);
            }
        }

        @Override
        public List<D> contents() {
            return records.values().stream().map(mapper::fromRecord).toList();
        }

        private void requireOpen(String operation) {
            if (!open) {
                throw new FileStatusException(ddname, operation, FileStatus.NOT_OPEN);
            }
        }
    }

    final class TableDataset<K, D extends Record> implements KeyedDataset<K, D> {

        private final String ddname;
        private final Mode mode;
        private final Function<K, Optional<D>> find;
        private final Predicate<K> exists;
        private final Consumer<D> insert;
        private final Predicate<D> update;
        private final Runnable clear;
        private final Function<D, K> key;
        private boolean open;

        private TableDataset(String ddname, Mode mode, Function<K, Optional<D>> find, Predicate<K> exists,
                             Consumer<D> insert, Predicate<D> update, Runnable clear, Function<D, K> key) {
            this.ddname = ddname;
            this.mode = mode;
            this.find = find;
            this.exists = exists;
            this.insert = insert;
            this.update = update;
            this.clear = clear;
            this.key = key;
        }

        @Override
        public String ddname() {
            return ddname;
        }

        @Override
        public void open() {
            if (open) {
                throw new FileStatusException(ddname, "OPEN", FileStatus.ALREADY_OPEN);
            }
            if (mode == Mode.OUTPUT) {
                guarded("OPEN", () -> {
                    clear.run();
                    return null;
                });
            }
            open = true;
        }

        @Override
        public Optional<D> read(K k) {
            requireOpen("READ");
            return guarded("READ", () -> find.apply(k));
        }

        @Override
        public void write(D data) {
            requireOpen("WRITE");
            if (mode == Mode.INPUT) {
                throw new FileStatusException(ddname, "WRITE", FileStatus.NOT_OPEN_OUTPUT);
            }
            guarded("WRITE", () -> {
                if (exists.test(key.apply(data))) {
                    throw new FileStatusException(ddname, "WRITE", FileStatus.DUPLICATE_KEY);
                }
                insert.accept(data);
                return null;
            });
        }

        @Override
        public boolean rewrite(D data) {
            requireOpen("REWRITE");
            if (mode != Mode.I_O) {
                throw new FileStatusException(ddname, "REWRITE", FileStatus.NOT_OPEN_IO);
            }
            return guarded("REWRITE", () -> update.test(data));
        }

        @Override
        public void close() {
            requireOpen("CLOSE");
            open = false;
        }

        private void requireOpen(String operation) {
            if (!open) {
                throw new FileStatusException(ddname, operation, FileStatus.NOT_OPEN);
            }
        }

        private <T> T guarded(String operation, java.util.function.Supplier<T> call) {
            try {
                return call.get();
            } catch (DataAccessException e) {
                throw new FileStatusException(ddname, operation, FileStatus.VSAM_OTHER_ERROR, e);
            }
        }
    }
}
