package com.carddemo.interestcalc.batch;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.interestcalc.parity.GoldenMaster;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Primary;
import org.springframework.boot.test.context.TestConfiguration;

/**
 * End-to-end run of the Spring Batch job over the shipped {@code app/data/ASCII} datasets, with
 * the run date and the clock pinned, comparing the files it writes with the COBOL oracle output.
 */
@SpringBootTest(properties = "spring.batch.job.enabled=false")
class InterestCalculationJobIntegrationTest {

    @TestConfiguration
    static class PinnedClock {
        /** Pins {@code FUNCTION CURRENT-DATE} so TRAN-ORIG-TS / TRAN-PROC-TS are reproducible. */
        @Bean
        @Primary
        Clock pinnedClock() {
            return GoldenMaster.FIXED_CLOCK;
        }
    }

    @Autowired
    private JobLauncher jobLauncher;

    @Autowired
    private Job cbact04cInterestCalculationJob;

    @Test
    @DisplayName("The job runs end to end and writes a TRANSACT file that matches the COBOL oracle")
    void jobProducesTheGoldenTransactFile(@TempDir Path outputDirectory) throws Exception {
        JobParameters parameters = new JobParametersBuilder()
                .addString("runDate", GoldenMaster.RUN_DATE)
                .addString("inputDirectory", GoldenMaster.shippedDataset().toString())
                .addString("outputDirectory", outputDirectory.toString())
                .toJobParameters();

        JobExecution execution = jobLauncher.run(cbact04cInterestCalculationJob, parameters);

        assertThat(execution.getStatus()).isEqualTo(BatchStatus.COMPLETED);

        assertThat(lines(outputDirectory.resolve(InterestCalculationTasklet.ACCTFILE_OUT)))
                .as("the account master after the run is byte identical to the COBOL oracle's")
                .containsExactlyElementsOf(GoldenMaster.goldenLines("base/acctdata-after.dat"));

        var produced = lines(outputDirectory.resolve(InterestCalculationTasklet.TRANSACT_OUT));
        var oracle = GoldenMaster.goldenLines("base/transact.dat");
        assertThat(produced).hasSameSizeAs(oracle);
        for (int i = 0; i < produced.size(); i++) {
            assertThat(produced.get(i)).hasSize(350);
            assertThat(blankTimestamps(produced.get(i)))
                    .as("TRANSACT record %d, timestamps excluded", i + 1)
                    .isEqualTo(blankTimestamps(oracle.get(i)));
            assertThat(produced.get(i).substring(278, 304)).isEqualTo(GoldenMaster.EXPECTED_TIMESTAMP);
            assertThat(produced.get(i).substring(304, 330)).isEqualTo(GoldenMaster.EXPECTED_TIMESTAMP);
        }
    }

    /** Blanks TRAN-ORIG-TS and TRAN-PROC-TS, the job's only nondeterministic output. */
    private static String blankTimestamps(String record) {
        return record.substring(0, 278) + " ".repeat(52) + record.substring(330);
    }

    private static java.util.List<String> lines(Path file) throws IOException {
        return Files.readAllLines(file, StandardCharsets.ISO_8859_1);
    }
}
