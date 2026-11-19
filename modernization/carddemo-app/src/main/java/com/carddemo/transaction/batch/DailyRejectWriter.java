package com.carddemo.transaction.batch;

import com.carddemo.common.cobol.CobolCharsets;
import com.carddemo.transaction.domain.DailyReject;
import org.springframework.batch.item.Chunk;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemStreamException;
import org.springframework.batch.item.ItemStreamWriter;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;

/**
 * DALYREJS writer (boundary B-02): RECFM=F 430-byte records, ISO-8859-1 like the oracle output (D-038), no separators.
 * New execution: the file must not exist ({@code DISP=(NEW,CATLG,DELETE)}, D-016). Each commit records the byte offset in
 * the step ExecutionContext; a restart truncates the file to the last committed offset and appends from there (D-012).
 */
public class DailyRejectWriter implements ItemStreamWriter<DailyReject> {

    static final String OFFSET_KEY = "dalyrejs.offset";

    private final Path file;
    private FileChannel channel;

    public DailyRejectWriter(Path file) {
        this.file = file;
    }

    @Override
    public void open(ExecutionContext context) {
        try {
            if (context.containsKey(OFFSET_KEY)) {
                long committed = context.getLong(OFFSET_KEY);
                channel = FileChannel.open(file, StandardOpenOption.WRITE);
                if (channel.size() < committed) {
                    throw new ItemStreamException(file + " is " + channel.size() + " bytes, shorter than the committed "
                            + committed + " on restart");
                }
                channel.truncate(committed);
                channel.position(committed);
            } else {
                channel = FileChannel.open(file, StandardOpenOption.WRITE, StandardOpenOption.CREATE_NEW);
            }
        } catch (IOException e) {
            throw new ItemStreamException("Cannot open DALYREJS " + file, e);
        }
    }

    @Override
    public void write(Chunk<? extends DailyReject> chunk) throws IOException {
        for (DailyReject reject : chunk) {
            ByteBuffer bytes = ByteBuffer.wrap(reject.toRecord().getBytes(CobolCharsets.ASCII));
            while (bytes.hasRemaining()) {
                channel.write(bytes);
            }
        }
    }

    @Override
    public void update(ExecutionContext context) {
        try {
            channel.force(false);
            context.putLong(OFFSET_KEY, channel.position());
        } catch (IOException e) {
            throw new ItemStreamException("Cannot sync DALYREJS " + file, e);
        }
    }

    @Override
    public void close() {
        if (channel != null) {
            try {
                channel.close();
            } catch (IOException e) {
                throw new ItemStreamException("Cannot close DALYREJS " + file, e);
            } finally {
                channel = null;
            }
        }
    }
}
