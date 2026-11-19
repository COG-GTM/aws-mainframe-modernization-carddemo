package com.carddemo.transaction.batch;

import com.carddemo.app.golden.GoldenCase;
import com.carddemo.common.cobol.CodePage;
import com.carddemo.common.error.FileStatusException;
import org.junit.jupiter.api.Test;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemStreamException;

import java.math.BigDecimal;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class DailyTransactionReaderTest {

    static List<DailyTransaction> readAll(Path file, CodePage codePage, ExecutionContext context) {
        DailyTransactionReader reader = new DailyTransactionReader(file, codePage);
        reader.open(context);
        try {
            List<DailyTransaction> all = new ArrayList<>();
            for (DailyTransaction t = reader.read(); t != null; t = reader.read()) {
                all.add(t);
            }
            reader.update(context);
            return all;
        } finally {
            reader.close();
        }
    }

    @Test
    void readsTheAsciiSample() {
        ExecutionContext context = new ExecutionContext();
        List<DailyTransaction> all = readAll(GoldenCase.of("GC-1").dalytran(), CodePage.ASCII, context);
        assertThat(all).hasSize(300);
        assertThat(all.get(0).inputSeq()).isEqualTo(1);
        assertThat(all.get(0).record().bytes()).hasSize(350);
        assertThat(all.get(0).record().getString("DALYTRAN-ID")).isEqualTo("0000000000683580");
        assertThat(all.get(0).record().getDecimal("DALYTRAN-AMT")).isInstanceOf(BigDecimal.class);
        assertThat(context.getLong(DailyTransactionReader.READ_COUNT)).isEqualTo(300);
    }

    @Test
    void cp037InputDecodesToTheSameFields() {
        List<DailyTransaction> ascii = readAll(GoldenCase.of("GC-1").dalytran(), CodePage.ASCII, new ExecutionContext());
        GoldenCase gc7 = GoldenCase.of("GC-7");
        assertThat(gc7.encoding()).isEqualTo(CodePage.CP037);
        List<DailyTransaction> ebcdic = readAll(gc7.dalytran(), CodePage.CP037, new ExecutionContext());
        assertThat(ebcdic).hasSize(300);
        for (int i = 0; i < 300; i++) {
            for (String field : List.of("DALYTRAN-ID", "DALYTRAN-CARD-NUM", "DALYTRAN-ORIG-TS", "DALYTRAN-DESC")) {
                assertThat(ebcdic.get(i).record().getString(field)).isEqualTo(ascii.get(i).record().getString(field));
            }
            assertThat(ebcdic.get(i).record().getDecimal("DALYTRAN-AMT"))
                    .isEqualByComparingTo(ascii.get(i).record().getDecimal("DALYTRAN-AMT"));
        }
    }

    @Test
    void restartSkipsTheCommittedRecords() {
        ExecutionContext context = new ExecutionContext();
        context.putLong(DailyTransactionReader.READ_COUNT, 150);
        List<DailyTransaction> rest = readAll(GoldenCase.of("GC-1").dalytran(), CodePage.ASCII, context);
        List<DailyTransaction> all = readAll(GoldenCase.of("GC-1").dalytran(), CodePage.ASCII, new ExecutionContext());
        assertThat(rest).hasSize(150);
        assertThat(rest.get(0).inputSeq()).isEqualTo(151);
        assertThat(rest.get(0).record().bytes()).isEqualTo(all.get(150).record().bytes());
        assertThat(context.getLong(DailyTransactionReader.READ_COUNT)).isEqualTo(300);
    }

    @Test
    void restartPastTheEndOfAShorterFileFails() {
        ExecutionContext context = new ExecutionContext();
        context.putLong(DailyTransactionReader.READ_COUNT, 301);
        assertThatThrownBy(() -> readAll(GoldenCase.of("GC-1").dalytran(), CodePage.ASCII, context))
                .isInstanceOf(ItemStreamException.class);
    }

    @Test
    void emptyInputHasNoRecords() {
        assertThat(readAll(GoldenCase.of("GC-5").dalytran(), CodePage.ASCII, new ExecutionContext())).isEmpty();
    }

    @Test
    void aMissingFileIsFileStatus35() {
        assertThatThrownBy(() -> readAll(Path.of("/nonexistent/DALYTRAN"), CodePage.ASCII, new ExecutionContext()))
                .isInstanceOf(FileStatusException.class).hasMessageContaining("35");
    }
}
