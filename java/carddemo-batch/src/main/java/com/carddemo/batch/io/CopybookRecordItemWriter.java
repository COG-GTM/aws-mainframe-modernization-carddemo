package com.carddemo.batch.io;

import com.carddemo.common.codec.RecordCodec;
import com.carddemo.common.io.RecordFileWriter;
import com.carddemo.common.io.RecordFormat;
import org.springframework.batch.item.Chunk;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemStreamException;
import org.springframework.batch.item.ItemStreamWriter;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import java.io.IOException;
import java.nio.channels.Channels;
import java.nio.channels.FileChannel;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.List;

/**
 * Spring Batch writer producing a mainframe-compatible sequential dataset (the {@code WRITE} of a batch program).
 * Records are buffered until the chunk's {@link #update}, discarded if the chunk transaction rolls back, and the
 * committed file length is saved so a restart truncates any uncommitted tail and appends after it.
 */
public class CopybookRecordItemWriter<T extends Record> implements ItemStreamWriter<T> {

    private final Path dataset;
    private final String positionKey;
    private final RecordFormat format;
    private final RecordCodec<T> codec;
    private final List<byte[]> pending = new ArrayList<>();
    private FileChannel channel;
    private RecordFileWriter writer;
    private boolean synchronizationRegistered;

    public CopybookRecordItemWriter(Path dataset, RecordFormat format, RecordCodec<T> codec) {
        this.dataset = dataset;
        this.positionKey = positionKey(dataset);
        this.format = format;
        this.codec = codec;
    }

    /** Execution-context key holding the committed length of {@code dataset}; unique per output file in a step. */
    static String positionKey(Path dataset) {
        return "copybook.writer." + dataset.getFileName() + ".position";
    }

    @Override
    public void open(ExecutionContext context) {
        try {
            channel = FileChannel.open(dataset, StandardOpenOption.CREATE, StandardOpenOption.WRITE);
            long position = context.containsKey(positionKey) ? context.getLong(positionKey) : 0;
            channel.truncate(position);
            channel.position(position);
        } catch (IOException e) {
            throw new ItemStreamException("Cannot open " + dataset, e);
        }
        writer = new RecordFileWriter(Channels.newOutputStream(channel), format, codec.layout().length());
    }

    @Override
    public void write(Chunk<? extends T> chunk) {
        if (TransactionSynchronizationManager.isSynchronizationActive() && !synchronizationRegistered) {
            TransactionSynchronizationManager.registerSynchronization(new TransactionSynchronization() {
                @Override
                public void afterCompletion(int status) {
                    if (status != STATUS_COMMITTED) {
                        pending.clear();
                    }
                    synchronizationRegistered = false;
                }
            });
            synchronizationRegistered = true;
        }
        for (T item : chunk) {
            pending.add(codec.encode(item));
        }
    }

    @Override
    public void update(ExecutionContext context) {
        if (writer == null) {
            return;
        }
        flushPending();
        try {
            context.putLong(positionKey, channel.position());
        } catch (IOException e) {
            throw new ItemStreamException("Cannot read position of " + dataset, e);
        }
    }

    @Override
    public void close() {
        if (writer != null) {
            flushPending();
            writer.close();
            writer = null;
            channel = null;
        }
    }

    private void flushPending() {
        pending.forEach(writer::write);
        pending.clear();
        writer.flush();
    }
}
