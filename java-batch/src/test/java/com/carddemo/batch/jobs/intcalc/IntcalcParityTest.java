package com.carddemo.batch.jobs.intcalc;

import com.carddemo.batch.model.AccountRecord;
import com.carddemo.batch.model.TransactionRecord;

import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Primary;

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Runs {@code intcalcJob} on the same inputs GnuCOBOL ran {@code CBACT04C} on and compares TRANSACT and the
 * rewritten ACCTFILE with the golden outputs in {@code golden/intcalc/<dataset>} (see
 * {@code java-batch/parity/intcalc/regenerate-golden.sh}). Only TRAN-ORIG-TS and TRAN-PROC-TS are masked.
 */
@SpringBootTest
class IntcalcParityTest {

    private static final Path REPO = Path.of("..").toAbsolutePath().normalize();
    private static final Path GOLDEN = Path.of("src/test/resources/golden/intcalc");
    private static final int TS_START = 278;
    private static final int TS_END = 330;
    private static final Instant FIXED_NOW = Instant.parse("2022-07-18T12:34:56.78Z");

    @TestConfiguration
    static class FixedClock {
        @Bean
        @Primary
        Clock intcalcTestClock() {
            return Clock.fixed(FIXED_NOW, ZoneOffset.UTC);
        }
    }

    @Autowired
    JobLauncher jobLauncher;

    @Autowired
    @Qualifier(IntcalcJobConfig.JOB_NAME)
    Job intcalcJob;

    @ParameterizedTest(name = "{0}")
    @CsvSource({"base, app/data/ASCII", "edge, java-batch/parity/intcalc/datasets/edge"})
    void matchesCobolGoldenOutput(String dataset, String inputDir, @TempDir Path out) throws Exception {
        JobExecution execution = jobLauncher.run(intcalcJob, new JobParametersBuilder()
                .addString("parmDate", IntcalcJobConfig.DEFAULT_PARM_DATE)
                .addString("inputDir", REPO.resolve(inputDir).toString())
                .addString("outputDir", out.toString())
                .addLong("run", System.nanoTime())
                .toJobParameters());
        assertThat(execution.getStatus()).isEqualTo(BatchStatus.COMPLETED);

        List<String> expectedTx = lines(GOLDEN.resolve(dataset).resolve("transact.dat"));
        List<String> actualTx = lines(out.resolve(IntcalcJobConfig.TRANSACT_FILE));
        assertThat(actualTx).hasSize(expectedTx.size());
        for (int i = 0; i < expectedTx.size(); i++) {
            TransactionRecord expected = TransactionRecord.CODEC.decode(expectedTx.get(i));
            TransactionRecord actual = TransactionRecord.CODEC.decode(actualTx.get(i));
            assertThat(actual.tranId()).as("TRAN-ID #%d", i + 1).isEqualTo(expected.tranId());
            assertThat(actual.tranAmt()).as("TRAN-AMT of %s", expected.tranId()).isEqualTo(expected.tranAmt());
            assertThat(masked(actualTx.get(i))).as("record %s", expected.tranId()).isEqualTo(masked(expectedTx.get(i)));
            assertThat(actual.tranOrigTs()).isEqualTo("2022-07-18-12.34.56.780000").isEqualTo(actual.tranProcTs());
        }

        List<String> expectedAcct = lines(GOLDEN.resolve(dataset).resolve("acctdata-after.dat"));
        List<String> actualAcct = lines(out.resolve("acctdata.txt"));
        assertThat(actualAcct).hasSize(expectedAcct.size());
        for (int i = 0; i < expectedAcct.size(); i++) {
            AccountRecord expected = AccountRecord.CODEC.decode(expectedAcct.get(i));
            AccountRecord actual = AccountRecord.CODEC.decode(actualAcct.get(i));
            assertThat(actual.acctCurrBal()).as("ACCT-CURR-BAL of %d", expected.acctId())
                    .isEqualTo(expected.acctCurrBal());
            assertThat(actualAcct.get(i)).as("account %d", expected.acctId()).isEqualTo(expectedAcct.get(i));
        }
        System.out.printf("INTCALC parity [%s]: %d/%d transactions, %d/%d accounts identical (timestamps masked)%n",
                dataset, actualTx.size(), expectedTx.size(), actualAcct.size(), expectedAcct.size());
    }

    private static List<String> lines(Path file) throws Exception {
        return Files.readAllLines(file, StandardCharsets.US_ASCII).stream().filter(l -> !l.isEmpty()).toList();
    }

    private static String masked(String transaction) {
        return transaction.substring(0, TS_START) + "#".repeat(TS_END - TS_START) + transaction.substring(TS_END);
    }
}
