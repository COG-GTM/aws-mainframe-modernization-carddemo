package com.carddemo.batch.io;

import com.carddemo.common.cobol.DataEncoding;
import com.carddemo.common.codec.RecordCodec;
import com.carddemo.common.io.RecordFormat;
import com.carddemo.common.testing.CardDemoDataset;
import com.carddemo.common.testing.GoldenMasterTest;
import com.carddemo.common.testing.MainframeSources;
import com.carddemo.data.catalog.CardDemoFile;
import com.carddemo.data.record.DailyTransactionRecord;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.batch.item.Chunk;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

@GoldenMasterTest
class CopybookRecordItemReaderWriterTest {

    @TempDir
    Path dir;

    @Test
    void copiesDailyTransactionsByteForByteAndRestarts(MainframeSources sources) throws Exception {
        Path input = sources.ebcdicData(CardDemoDataset.DALYTRAN.ebcdicFile());
        RecordCodec<DailyTransactionRecord> codec = CardDemoFile.DAILY_TRANSACTION.codec(DataEncoding.EBCDIC);

        CopybookRecordItemReader<DailyTransactionRecord> reader = new CopybookRecordItemReader<>(input, RecordFormat.FIXED, codec);
        ExecutionContext context = new ExecutionContext();
        reader.open(context);
        List<DailyTransactionRecord> items = new ArrayList<>();
        for (DailyTransactionRecord r; (r = reader.read()) != null; ) {
            items.add(r);
        }
        reader.update(context);
        reader.close();
        assertThat(items).hasSize(300);

        Path output = dir.resolve("DALYTRAN.copy");
        CopybookRecordItemWriter<DailyTransactionRecord> writer = new CopybookRecordItemWriter<>(output, RecordFormat.FIXED, codec);
        writer.open(new ExecutionContext());
        writer.write(new Chunk<>(items));
        writer.close();
        assertThat(Files.readAllBytes(output)).isEqualTo(Files.readAllBytes(input));

        ExecutionContext restart = new ExecutionContext();
        restart.putLong("copybook.reader.read.count", 298);
        reader.open(restart);
        assertThat(reader.read()).isEqualTo(items.get(298));
        assertThat(reader.read()).isEqualTo(items.get(299));
        assertThat(reader.read()).isNull();
        reader.close();
    }

    @Test
    void writerDiscardsRolledBackChunkAndTruncatesUncommittedTailOnRestart(MainframeSources sources) throws Exception {
        Path input = sources.ebcdicData(CardDemoDataset.DALYTRAN.ebcdicFile());
        RecordCodec<DailyTransactionRecord> codec = CardDemoFile.DAILY_TRANSACTION.codec(DataEncoding.EBCDIC);
        List<DailyTransactionRecord> items = new ArrayList<>();
        CopybookRecordItemReader<DailyTransactionRecord> reader = new CopybookRecordItemReader<>(input, RecordFormat.FIXED, codec);
        reader.open(new ExecutionContext());
        for (DailyTransactionRecord r; (r = reader.read()) != null; ) {
            items.add(r);
        }
        reader.close();
        int lrecl = codec.layout().length();
        Path output = dir.resolve("POSTED.out");

        CopybookRecordItemWriter<DailyTransactionRecord> writer = new CopybookRecordItemWriter<>(output, RecordFormat.FIXED, codec);
        ExecutionContext context = new ExecutionContext();
        writer.open(context);
        writer.write(new Chunk<>(items.subList(0, 2)));
        writer.update(context);
        ExecutionContext committed = new ExecutionContext(context);

        TransactionSynchronizationManager.initSynchronization();
        try {
            writer.write(new Chunk<>(items.subList(2, 4)));
            TransactionSynchronizationManager.getSynchronizations()
                    .forEach(s -> s.afterCompletion(TransactionSynchronization.STATUS_ROLLED_BACK));
        } finally {
            TransactionSynchronizationManager.clearSynchronization();
        }
        writer.write(new Chunk<>(items.subList(4, 5)));
        writer.update(context);
        writer.close();
        assertThat(Files.size(output)).isEqualTo(3L * lrecl);

        CopybookRecordItemWriter<DailyTransactionRecord> restarted = new CopybookRecordItemWriter<>(output, RecordFormat.FIXED, codec);
        restarted.open(committed);
        restarted.write(new Chunk<>(items.subList(5, 6)));
        restarted.update(committed);
        restarted.close();
        byte[] expected = new byte[3 * lrecl];
        byte[] all = Files.readAllBytes(input);
        System.arraycopy(all, 0, expected, 0, 2 * lrecl);
        System.arraycopy(all, 5 * lrecl, expected, 2 * lrecl, lrecl);
        assertThat(Files.readAllBytes(output)).isEqualTo(expected);
    }

    @Test
    void writersInOneStepKeepSeparatePositions() {
        assertThat(CopybookRecordItemWriter.positionKey(dir.resolve("A.out")))
                .isNotEqualTo(CopybookRecordItemWriter.positionKey(dir.resolve("B.out")));
    }
}
