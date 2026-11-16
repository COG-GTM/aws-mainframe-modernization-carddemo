package com.carddemo.batch.io;

import com.carddemo.batch.model.DailyTransactionRecord;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.batch.item.Chunk;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.file.FlatFileItemReader;
import org.springframework.batch.item.file.FlatFileItemWriter;
import org.springframework.core.io.FileSystemResource;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

class FixedWidthItemIOTest {

    @Test
    void readerAndWriterRoundTripDailyTransactions(@TempDir Path tmp) throws Exception {
        Path in = CardDemoData.file(CardDemoData.DAILY_TRANSACTIONS);
        FlatFileItemReader<DailyTransactionRecord> reader =
                FixedWidthItemIO.reader("dalytran", new FileSystemResource(in), DailyTransactionRecord.CODEC);
        List<DailyTransactionRecord> items = new ArrayList<>();
        reader.open(new ExecutionContext());
        for (DailyTransactionRecord r; (r = reader.read()) != null; ) {
            items.add(r);
        }
        reader.close();
        assertThat(items).hasSize(300);

        Path out = tmp.resolve("dailytran.txt");
        FlatFileItemWriter<DailyTransactionRecord> writer =
                FixedWidthItemIO.writer("dalytran-out", new FileSystemResource(out), DailyTransactionRecord.CODEC);
        writer.open(new ExecutionContext());
        writer.write(new Chunk<>(items));
        writer.close();

        assertThat(Files.readAllBytes(out)).isEqualTo(Files.readAllBytes(in));
    }
}
