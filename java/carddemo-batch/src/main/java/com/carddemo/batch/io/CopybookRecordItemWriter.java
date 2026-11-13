package com.carddemo.batch.io;

import com.carddemo.common.codec.RecordCodec;
import com.carddemo.common.io.RecordFileWriter;
import com.carddemo.common.io.RecordFormat;
import org.springframework.batch.item.Chunk;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemStreamWriter;

import java.nio.file.Path;

/** Spring Batch writer producing a mainframe-compatible sequential dataset (the {@code WRITE} of a batch program). */
public class CopybookRecordItemWriter<T extends Record> implements ItemStreamWriter<T> {

    private final Path dataset;
    private final RecordFormat format;
    private final RecordCodec<T> codec;
    private RecordFileWriter writer;

    public CopybookRecordItemWriter(Path dataset, RecordFormat format, RecordCodec<T> codec) {
        this.dataset = dataset;
        this.format = format;
        this.codec = codec;
    }

    @Override
    public void open(ExecutionContext context) {
        writer = RecordFileWriter.create(dataset, format, codec.layout().length());
    }

    @Override
    public void write(Chunk<? extends T> chunk) {
        for (T item : chunk) {
            writer.write(codec.encode(item));
        }
    }

    @Override
    public void close() {
        if (writer != null) {
            writer.close();
            writer = null;
        }
    }
}
