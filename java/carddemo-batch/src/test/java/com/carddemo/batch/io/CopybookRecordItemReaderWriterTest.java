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
}
