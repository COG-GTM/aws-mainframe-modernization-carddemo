package com.carddemo.posting.batch;

import com.carddemo.recordio.codec.RecordEncoding;
import com.carddemo.recordio.layout.RecordLayout;
import com.carddemo.recordio.store.FixedWidthFile;
import org.springframework.batch.item.ItemReader;

import java.nio.file.Path;
import java.util.Iterator;
import java.util.List;

/** Sequential READ of a RECFM=F dataset, one decoded record per item; end of file = null. */
public final class FixedWidthItemReader<T> implements ItemReader<T> {

    private final Path path;
    private final RecordLayout<T> layout;
    private final RecordEncoding encoding;
    private Iterator<T> records;

    public FixedWidthItemReader(Path path, RecordLayout<T> layout, RecordEncoding encoding) {
        this.path = path;
        this.layout = layout;
        this.encoding = encoding;
    }

    @Override
    public T read() {
        if (records == null) {
            List<T> all = FixedWidthFile.readAll(path, layout, encoding);
            records = all.iterator();
        }
        return records.hasNext() ? records.next() : null;
    }
}
