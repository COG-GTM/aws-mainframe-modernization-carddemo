package com.carddemo.batch.io;

import com.carddemo.common.cobol.DataEncoding;
import com.carddemo.common.codec.RecordCodec;
import com.carddemo.common.io.RecordFileReader;
import com.carddemo.common.io.RecordFormat;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemStreamException;
import org.springframework.batch.item.ItemStreamReader;

import java.nio.file.Path;

/**
 * Spring Batch reader for a sequential mainframe dataset described by a copybook (the {@code READ ... AT END}
 * loop of a batch COBOL program). Restartable: the number of records already read is kept in the step context.
 */
public class CopybookRecordItemReader<T extends Record> implements ItemStreamReader<T> {

    private static final String READ_COUNT = "copybook.reader.read.count";

    private final Path dataset;
    private final RecordFormat format;
    private final RecordCodec<T> codec;
    private RecordFileReader reader;
    private long readCount;

    public CopybookRecordItemReader(Path dataset, RecordFormat format, RecordCodec<T> codec) {
        this.dataset = dataset;
        this.format = format;
        this.codec = codec;
    }

    @Override
    public void open(ExecutionContext context) {
        DataEncoding encoding = codec.encoding();
        reader = RecordFileReader.open(dataset, format, codec.layout().length(), encoding);
        long skip = context.getLong(READ_COUNT, 0L);
        for (readCount = 0; readCount < skip && reader.hasNext(); readCount++) {
            reader.next();
        }
    }

    @Override
    public T read() {
        if (reader == null) {
            throw new ItemStreamException("Reader for " + dataset + " is not open");
        }
        if (!reader.hasNext()) {
            return null;
        }
        readCount++;
        return codec.decode(reader.next());
    }

    @Override
    public void update(ExecutionContext context) {
        context.putLong(READ_COUNT, readCount);
    }

    @Override
    public void close() {
        if (reader != null) {
            reader.close();
            reader = null;
        }
    }
}
