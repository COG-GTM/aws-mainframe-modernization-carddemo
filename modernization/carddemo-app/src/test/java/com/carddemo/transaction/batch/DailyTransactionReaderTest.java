package com.carddemo.transaction.batch;

import com.carddemo.app.golden.GoldenCase;
import com.carddemo.common.cobol.CobolCharsets;
import com.carddemo.transaction.domain.DailyTransaction;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemStreamException;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class DailyTransactionReaderTest {

    @Test
    void readsEveryBaselineRecordWithTypedFields() {
        List<DailyTransaction> all = readAll(GoldenCase.named("baseline"));
        assertThat(all).hasSize(300);
        DailyTransaction first = all.get(0);
        assertThat(first.id()).isEqualTo("0000000000683580");
        assertThat(first.typeCd()).isEqualTo("01");
        assertThat(first.catCd()).isEqualTo(1);
        assertThat(first.amount()).isEqualByComparingTo("504.77");
        assertThat(first.cardNum()).isEqualTo("4859452612877065");
        assertThat(first.origTs()).isEqualTo("2022-06-10 19:27:53.000000");
        assertThat(first.procTs()).isBlank();
    }

    @Test
    void cp037InputDecodesToTheSameRecordsAsTheAsciiBaseline() {
        assertThat(readAll(GoldenCase.named("cp037"))).isEqualTo(readAll(GoldenCase.named("baseline")));
    }

    @Test
    void emptyFileIsEndOfInput() {
        assertThat(readAll(GoldenCase.named("empty"))).isEmpty();
    }

    @Test
    void restartSkipsTheCommittedRecords() throws Exception {
        GoldenCase c = GoldenCase.named("syn-all");
        ExecutionContext ctx = new ExecutionContext();
        DailyTransactionReader reader = new DailyTransactionReader(c.dalytran(), c.dalytranCharset());
        reader.open(ctx);
        reader.read();
        reader.read();
        reader.update(ctx);
        reader.close();

        DailyTransactionReader restarted = new DailyTransactionReader(c.dalytran(), c.dalytranCharset());
        restarted.open(ctx);
        List<String> ids = new ArrayList<>();
        for (DailyTransaction t; (t = restarted.read()) != null; ) {
            ids.add(t.id());
        }
        restarted.close();
        assertThat(ids).containsExactly("SYN0000000000003", "SYN0000000000004", "SYN0000000000005", "SYN0000000000006");
    }

    @Test
    void shortTrailingRecordFailsTheRead(@TempDir Path tmp) throws Exception {
        byte[] bytes = Files.readAllBytes(GoldenCase.named("syn-ok").dalytran());
        Path cut = Files.write(tmp.resolve("DALYTRAN"), Arrays.copyOf(bytes, bytes.length - 1));
        DailyTransactionReader reader = new DailyTransactionReader(cut, CobolCharsets.ASCII);
        reader.open(new org.springframework.batch.item.ExecutionContext());
        assertThat(reader.read()).isNotNull();
        assertThatThrownBy(reader::read).isInstanceOf(ItemStreamException.class).hasMessageContaining("short DALYTRAN record");
        reader.close();
    }

    static List<DailyTransaction> readAll(GoldenCase c) {
        DailyTransactionReader reader = new DailyTransactionReader(c.dalytran(), c.dalytranCharset());
        reader.open(new ExecutionContext());
        try {
            List<DailyTransaction> out = new ArrayList<>();
            for (DailyTransaction t; (t = reader.read()) != null; ) {
                out.add(t);
            }
            return out;
        } catch (Exception e) {
            throw new IllegalStateException(e);
        } finally {
            reader.close();
        }
    }
}
