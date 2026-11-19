package com.carddemo.transaction.batch;

import com.carddemo.app.golden.GoldenCase;
import com.carddemo.app.golden.ParityHarness;
import com.carddemo.common.cobol.CobolCharsets;
import com.carddemo.common.record.Copybooks;
import com.carddemo.common.record.FixedRecord;
import com.carddemo.common.record.RecordFiles;
import com.carddemo.transaction.domain.DailyReject;
import com.carddemo.transaction.domain.DailyTransaction;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.batch.item.Chunk;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemStreamException;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class DailyRejectWriterTest {

    @Test
    void rejectsWrittenOneChunkAtATimeReproduceTheOracleDalyrejs(@TempDir Path tmp) throws Exception {
        for (String name : List.of("baseline", "syn-all", "acct-missing", "empty")) {
            GoldenCase c = GoldenCase.named(name);
            Path out = tmp.resolve(name + ".DALYREJS");
            DailyRejectWriter writer = new DailyRejectWriter(out);
            ExecutionContext ctx = new ExecutionContext();
            writer.open(ctx);
            for (DailyReject reject : oracleRejects(c)) {
                writer.write(Chunk.of(reject));
                writer.update(ctx);
            }
            writer.close();
            ParityHarness.assertDalyrejs(c, out);
        }
    }

    @Test
    void restartTruncatesToTheCommittedOffsetAndAppends(@TempDir Path tmp) throws Exception {
        List<DailyReject> rejects = oracleRejects(GoldenCase.named("baseline")).subList(0, 4);
        Path out = tmp.resolve("DALYREJS");
        ExecutionContext ctx = new ExecutionContext();
        DailyRejectWriter first = new DailyRejectWriter(out);
        first.open(ctx);
        first.write(Chunk.of(rejects.get(0)));
        first.update(ctx);
        first.write(Chunk.of(rejects.get(1)));
        first.update(ctx);
        first.write(Chunk.of(rejects.get(2)));   // written but its chunk never committed: the step failed here
        first.close();
        assertThat(Files.size(out)).isEqualTo(3 * 430L);
        assertThat(ctx.getLong(DailyRejectWriter.OFFSET_KEY)).isEqualTo(2 * 430L);

        DailyRejectWriter restarted = new DailyRejectWriter(out);
        restarted.open(ctx);
        restarted.write(Chunk.of(rejects.get(2)));
        restarted.update(ctx);
        restarted.write(Chunk.of(rejects.get(3)));
        restarted.update(ctx);
        restarted.close();

        StringBuilder expected = new StringBuilder();
        rejects.forEach(r -> expected.append(r.toRecord()));
        assertThat(Files.readString(out, CobolCharsets.ASCII)).isEqualTo(expected.toString());
    }

    @Test
    void newRunRefusesAnExistingFileAndRestartRefusesAShorterOne(@TempDir Path tmp) throws Exception {
        Path existing = Files.writeString(tmp.resolve("DALYREJS"), "x");
        assertThatThrownBy(() -> new DailyRejectWriter(existing).open(new ExecutionContext()))
                .isInstanceOf(ItemStreamException.class);
        ExecutionContext ctx = new ExecutionContext();
        ctx.putLong(DailyRejectWriter.OFFSET_KEY, 430L);
        assertThatThrownBy(() -> new DailyRejectWriter(existing).open(ctx)).hasMessageContaining("shorter than the committed");
    }

    @Test
    void reasonDescriptionIsPaddedTo76AndCodeZeroFilled() {
        DailyTransaction t = new DailyTransaction(" ".repeat(16) + "01" + "0001" + " ".repeat(110) + "0000000000{" + "000000000" + " ".repeat(198));
        String record = new DailyReject(t, 101, "ACCOUNT RECORD NOT FOUND").toRecord();
        assertThat(record).hasSize(430).startsWith(t.record());
        assertThat(record.substring(350)).isEqualTo("0101ACCOUNT RECORD NOT FOUND" + " ".repeat(52));
    }

    static List<DailyReject> oracleRejects(GoldenCase c) {
        return RecordFiles.readFixed(c.expected("DALYREJS"), 430, CobolCharsets.ASCII).stream().map(text -> {
            FixedRecord r = Copybooks.DALYREJS.parse(text);
            return new DailyReject(new DailyTransaction(r.raw("REJECT-TRAN-DATA")),
                    (int) r.number("WS-VALIDATION-FAIL-REASON"), r.raw("WS-VALIDATION-FAIL-REASON-DESC"));
        }).toList();
    }
}
