package com.carddemo.transaction.batch;

import com.carddemo.app.PostgresTestBase;
import com.carddemo.app.golden.GoldenCase;
import com.carddemo.app.golden.GoldenCompare;
import com.carddemo.common.batch.GenerationDirectory;
import com.carddemo.common.batch.ReturnCode;
import com.carddemo.common.cobol.CodePage;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.beans.factory.annotation.Autowired;

import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static org.assertj.core.api.Assertions.assertThat;

class RejectFileWriterTest extends PostgresTestBase {

    @Autowired
    RejectStore store;

    @Autowired
    RejectFileWriter writer;

    @TempDir
    Path out;

    @BeforeEach
    void clean() {
        truncateBusinessTables();
    }

    GenerationDirectory generation() {
        return GenerationDirectory.create(out, "POSTTRAN", Clock.systemUTC());
    }

    /**
     * Stages the oracle's rejects (input record + reason + description from its trailer) and checks the published
     * DALYREJS equals the oracle's byte for byte (GC-7: converted to CP037, R-14).
     */
    @ParameterizedTest
    @ValueSource(strings = {"GC-1", "GC-2", "GC-6", "GC-7"})
    void replayingTheOracleRejectsReproducesDalyrejs(String name) throws Exception {
        GoldenCase gc = GoldenCase.of(name);
        byte[] expected = GoldenCompare.expectedDalyrejs(gc.expectedDalyrejs().orElseThrow(), gc.encoding());
        List<DailyTransaction> input = DailyTransactionReaderTest.readAll(gc.dalytran(), gc.encoding(), new ExecutionContext());
        int next = 0;
        for (byte[] reject : GoldenCompare.records(expected, RejectRecordLayout.LENGTH)) {
            var image = RejectRecordLayout.LAYOUT.wrap(reject, gc.encoding());
            byte[] record = Arrays.copyOf(reject, DailyTransactionLayout.LENGTH);
            while (!Arrays.equals(input.get(next).record().bytes(), record)) {
                next++;
            }
            store.stage(7L, input.get(next++), (int) image.getLong("VALIDATION-FAIL-REASON"),
                    image.getString("VALIDATION-FAIL-REASON-DESC"));
        }
        Optional<Path> published = writer.finish(generation(), 7L, gc.oracleReturnCode());
        assertThat(published).isPresent();
        assertThat(GoldenCompare.sha256(Files.readAllBytes(published.get()))).isEqualTo(GoldenCompare.sha256(expected));
    }

    @Test
    void rawBytesSurviveStagingAndAnAbendPublishesNothing() throws Exception {
        byte[] image = CodePage.CP037.encode(" ".repeat(DailyTransactionLayout.LENGTH));
        image[0] = (byte) 0xF1;
        Arrays.fill(image, 330, 350, (byte) 0x00);
        var record = DailyTransactionLayout.LAYOUT.wrap(image, CodePage.CP037);
        store.stage(9L, new DailyTransaction(2, record), 100, "INVALID CARD NUMBER FOUND");
        store.stage(9L, new DailyTransaction(1, record), 102, "TRANSACTION RECEIVED AFTER ACCT EXPIRATION");
        store.stage(10L, new DailyTransaction(1, record), 101, "OTHER INSTANCE");

        GenerationDirectory abended = generation();
        assertThat(writer.finish(abended, 9L, ReturnCode.ABEND)).isEmpty();
        assertThat(abended.resolve(RejectFileWriter.DD)).doesNotExist();
        assertThat(abended.temp(RejectFileWriter.DD)).doesNotExist();

        GenerationDirectory warned = GenerationDirectory.create(out, "POSTTRAN", Clock.offset(Clock.systemUTC(),
                java.time.Duration.ofSeconds(1)));
        Path dalyrejs = writer.finish(warned, 9L, ReturnCode.WARNING).orElseThrow();
        byte[] bytes = Files.readAllBytes(dalyrejs);
        assertThat(bytes).hasSize(2 * RejectRecordLayout.LENGTH);
        assertThat(Arrays.copyOfRange(bytes, 0, 350)).isEqualTo(image);
        assertThat(CodePage.CP037.decode(bytes, 350, 4)).isEqualTo("0102");
        assertThat(CodePage.CP037.decode(bytes, 780, 29)).isEqualTo("0100INVALID CARD NUMBER FOUND");
        assertThat(warned.temp(RejectFileWriter.DD)).doesNotExist();
    }

    @Test
    void noRejectsPublishesAnEmptyDalyrejs() throws Exception {
        Path dalyrejs = writer.finish(generation(), 11L, ReturnCode.OK).orElseThrow();
        assertThat(Files.size(dalyrejs)).isZero();
    }
}
