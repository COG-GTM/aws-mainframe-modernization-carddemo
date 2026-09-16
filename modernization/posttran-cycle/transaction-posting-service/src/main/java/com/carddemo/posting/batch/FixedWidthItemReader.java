package com.carddemo.posting.batch;

import com.carddemo.recordio.codec.FixedWidthRecord;
import com.carddemo.recordio.codec.RecordEncoding;
import com.carddemo.recordio.store.FixedWidthFile;
import org.springframework.batch.item.ItemReader;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Iterator;
import java.util.function.Function;

/** Sequential READ of a RECFM=F dataset, one decoded record per item; end of file = null. */
public final class FixedWidthItemReader<T> implements ItemReader<T> {

    private final Path path;
    private final int recordLength;
    private final RecordEncoding encoding;
    private final Function<FixedWidthRecord, T> decoder;
    private Iterator<FixedWidthRecord> records;

    public FixedWidthItemReader(Path path, int recordLength, RecordEncoding encoding,
                                Function<FixedWidthRecord, T> decoder) {
        this.path = path;
        this.recordLength = recordLength;
        this.encoding = encoding;
        this.decoder = decoder;
    }

    @Override
    public T read() {
        if (records == null) {
            try {
                records = FixedWidthFile.split(Files.readAllBytes(path), recordLength, encoding).iterator();
            } catch (IOException e) {
                throw new UncheckedIOException("cannot read " + path, e);
            }
        }
        return records.hasNext() ? decoder.apply(records.next()) : null;
    }
}
