package com.carddemo.batch.posttran.batch;

import com.carddemo.batch.posttran.cobol.Abend;
import com.carddemo.batch.posttran.cobol.RecordFormat;
import com.carddemo.batch.posttran.record.RejectRecord;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import org.springframework.batch.infrastructure.item.ExecutionContext;
import org.springframework.batch.infrastructure.item.ItemStream;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

/**
 * DALYREJS DD (RECFM=F, LRECL=430): 0300-DALYREJS-OPEN, 2500-WRITE-REJECT-REC, 9300-DALYREJS-CLOSE.
 * Rejects are staged per chunk and written and flushed just before the chunk's database commit: a
 * rolled-back chunk leaves no rejects behind, and a failed reject write rolls the chunk's postings
 * back. The only window left is a database commit that fails after the flush.
 */
public class RejectFileWriter implements ItemStream {

    private final Path path;
    private final RecordFormat format;
    private final List<String> pending = new ArrayList<>();
    private OutputStream out;

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
            out = new BufferedOutputStream(Files.newOutputStream(path));
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
                    pending.forEach(RejectFileWriter.this::writeRecord);
                    flush();
                }

                @Override
                public void afterCompletion(int status) {
                    pending.clear();
                }
            });
        }
        pending.add(record);
    }

    private void writeRecord(String record) {
        try {
            out.write(record.getBytes(format.charset()));
            if (format.lineDelimited()) {
                out.write('\n');
            }
        } catch (IOException e) {
            throw new Abend("ERROR WRITING TO REJECTS FILE", "30", e);
        }
    }

    @Override
    public void update(ExecutionContext executionContext) {
        flush();
    }

    private void flush() {
        try {
            if (out != null) {
                out.flush();
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
