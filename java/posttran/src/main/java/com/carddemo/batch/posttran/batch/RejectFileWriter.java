package com.carddemo.batch.posttran.batch;

import com.carddemo.batch.posttran.cobol.Abend;
import com.carddemo.batch.posttran.cobol.RecordFormat;
import com.carddemo.batch.posttran.record.RejectRecord;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.List;
import org.springframework.batch.infrastructure.item.ExecutionContext;
import org.springframework.batch.infrastructure.item.ItemStream;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

/**
 * DALYREJS DD (RECFM=F, LRECL=430): 0300-DALYREJS-OPEN, 2500-WRITE-REJECT-REC, 9300-DALYREJS-CLOSE.
 * Rejects are staged per chunk, written and forced to disk just before the chunk's database commit,
 * and truncated away again if the chunk does not commit. A failed reject write therefore rolls the
 * chunk's postings back, and a failed commit removes the chunk's rejects.
 */
public class RejectFileWriter implements ItemStream {

    private final Path path;
    private final RecordFormat format;
    private final List<String> pending = new ArrayList<>();
    private FileChannel out;
    private long chunkStart = -1;

    public RejectFileWriter(Path path, RecordFormat format) {
        this.path = path;
        this.format = format;
    }

    @Override
    public void open(ExecutionContext executionContext) {
        try {
            if (path.toAbsolutePath().getParent() != null) {
                Files.createDirectories(path.toAbsolutePath().getParent());
            }
            out = FileChannel.open(path, StandardOpenOption.CREATE, StandardOpenOption.WRITE,
                    StandardOpenOption.TRUNCATE_EXISTING);
        } catch (IOException e) {
            throw new Abend("ERROR OPENING DALY REJECTS FILE", "30", e);
        }
    }

    public void write(String record) {
        if (record.length() != RejectRecord.LENGTH) {
            throw new Abend("ERROR WRITING TO REJECTS FILE", "44");
        }
        if (!TransactionSynchronizationManager.isSynchronizationActive()) {
            writeRecord(record);
            return;
        }
        if (pending.isEmpty()) {
            TransactionSynchronizationManager.registerSynchronization(new TransactionSynchronization() {
                @Override
                public void beforeCommit(boolean readOnly) {
                    writePending();
                }

                @Override
                public void afterCompletion(int status) {
                    if (status != STATUS_COMMITTED) {
                        discardChunk();
                    }
                    pending.clear();
                    chunkStart = -1;
                }
            });
        }
        pending.add(record);
    }

    private void writePending() {
        try {
            chunkStart = out.position();
            pending.forEach(this::writeRecord);
            out.force(false);
        } catch (IOException e) {
            throw new Abend("ERROR WRITING TO REJECTS FILE", "30", e);
        }
    }

    private void discardChunk() {
        if (chunkStart < 0) {
            return;
        }
        try {
            out.truncate(chunkStart);
            out.position(chunkStart);
        } catch (IOException e) {
            throw new Abend("ERROR WRITING TO REJECTS FILE", "30", e);
        }
    }

    private void writeRecord(String record) {
        byte[] bytes = record.getBytes(format.charset());
        ByteBuffer buf = ByteBuffer.allocate(bytes.length + (format.lineDelimited() ? 1 : 0)).put(bytes);
        if (format.lineDelimited()) {
            buf.put((byte) '\n');
        }
        buf.flip();
        try {
            while (buf.hasRemaining()) {
                out.write(buf);
            }
        } catch (IOException e) {
            throw new Abend("ERROR WRITING TO REJECTS FILE", "30", e);
        }
    }

    @Override
    public void close() {
        try {
            if (out != null) {
                out.close();
                out = null;
            }
        } catch (IOException e) {
            throw new Abend("ERROR CLOSING DAILY REJECTS FILE", "30", e);
        }
    }
}
