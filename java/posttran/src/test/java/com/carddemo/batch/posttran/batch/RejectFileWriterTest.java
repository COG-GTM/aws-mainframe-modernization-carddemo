package com.carddemo.batch.posttran.batch;

import static com.carddemo.batch.posttran.TestData.dalytran;
import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.batch.posttran.cobol.RecordFormat;
import com.carddemo.batch.posttran.record.RejectRecord;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.batch.infrastructure.item.ExecutionContext;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

class RejectFileWriterTest {

    @TempDir
    Path tmp;

    @AfterEach
    void clear() {
        if (TransactionSynchronizationManager.isSynchronizationActive()) {
            TransactionSynchronizationManager.clearSynchronization();
        }
    }

    private static String reject(String id) {
        return RejectRecord.format(dalytran(id, "01", 1, "1.00", "9999999999999999", "2020-01-01 00:00:00.000000"),
                100, "INVALID CARD NUMBER FOUND");
    }

    private static void completeChunk(boolean commitSucceeds) {
        var syncs = TransactionSynchronizationManager.getSynchronizations();
        syncs.forEach(s -> s.beforeCommit(false));
        syncs.forEach(s -> s.afterCompletion(commitSucceeds
                ? TransactionSynchronization.STATUS_COMMITTED : TransactionSynchronization.STATUS_ROLLED_BACK));
        TransactionSynchronizationManager.clearSynchronization();
    }

    @Test
    void commitFailureAfterWriteTruncatesTheChunksRejects() throws Exception {
        Path file = tmp.resolve("dalyrejs");
        RejectFileWriter writer = new RejectFileWriter(file, RecordFormat.EBCDIC);
        writer.open(new ExecutionContext());

        TransactionSynchronizationManager.initSynchronization();
        writer.write(reject("T1"));
        completeChunk(true);

        TransactionSynchronizationManager.initSynchronization();
        writer.write(reject("T2"));
        writer.write(reject("T3"));
        completeChunk(false);

        TransactionSynchronizationManager.initSynchronization();
        writer.write(reject("T4"));
        completeChunk(true);
        writer.close();

        byte[] bytes = Files.readAllBytes(file);
        assertThat(bytes).hasSize(2 * RejectRecord.LENGTH);
        String content = new String(bytes, RecordFormat.EBCDIC.charset());
        assertThat(content.substring(0, 2)).isEqualTo("T1");
        assertThat(content.substring(RejectRecord.LENGTH, RejectRecord.LENGTH + 2)).isEqualTo("T4");
    }
}
