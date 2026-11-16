package com.carddemo.batch.io;

import org.springframework.batch.item.file.FlatFileItemReader;
import org.springframework.batch.item.file.FlatFileItemWriter;
import org.springframework.batch.item.file.builder.FlatFileItemReaderBuilder;
import org.springframework.batch.item.file.builder.FlatFileItemWriterBuilder;
import org.springframework.core.io.Resource;
import org.springframework.core.io.WritableResource;

/** Spring Batch {@link FlatFileItemReader}/{@link FlatFileItemWriter} factories over a {@link FixedWidthCodec}. */
public final class FixedWidthItemIO {

    private FixedWidthItemIO() {
    }

    /** Reader for a sequential file of {@code codec} records (LF or CRLF line ends). */
    public static <T> FlatFileItemReader<T> reader(String name, Resource resource, FixedWidthCodec<T> codec) {
        return new FlatFileItemReaderBuilder<T>()
                .name(name)
                .resource(resource)
                .lineMapper((line, lineNumber) -> codec.decode(line))
                .build();
    }

    /** Writer emitting full-length {@code codec} records, LF terminated. */
    public static <T> FlatFileItemWriter<T> writer(String name, WritableResource resource, FixedWidthCodec<T> codec) {
        return writer(name, resource, codec, "\n");
    }

    public static <T> FlatFileItemWriter<T> writer(String name, WritableResource resource, FixedWidthCodec<T> codec,
                                                   String lineSeparator) {
        return new FlatFileItemWriterBuilder<T>()
                .name(name)
                .resource(resource)
                .lineAggregator(codec::encode)
                .lineSeparator(lineSeparator)
                .shouldDeleteIfExists(true)
                .build();
    }
}
