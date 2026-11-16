package com.carddemo.batch.jobs.posttran;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.batch.jobs.posttran.PostTranJobConfig.PostTranFiles;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.List;
import java.util.stream.Collectors;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
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

/**
 * Runs posttranJob on the app/data/ASCII samples and compares every output byte for byte with
 * the GnuCOBOL run of CBTRN02C (java-batch/parity/posttran/run-golden.sh). Only TRAN-PROC-TS,
 * the wall-clock CURRENT-DATE, is masked.
 */
@SpringBootTest
class PostTranParityTest {

    static final Path GOLDEN = Path.of("src/test/resources/golden/posttran");
    static final int PROC_TS_OFFSET = 304;
    static final int PROC_TS_LENGTH = 26;
    static final Instant FIXED_NOW = Instant.parse("2026-11-16T04:30:15.420Z");

    @TestConfiguration
    static class FixedClock {
        @Bean
        Clock clock() {
            return Clock.fixed(FIXED_NOW, ZoneOffset.UTC);
        }
    }

    @Autowired
    JobLauncher jobLauncher;

    @Autowired
    @Qualifier(PostTranJobConfig.JOB_NAME)
    Job posttranJob;

    @TempDir
    Path out;

    @Test
    void javaOutputsMatchCobolGoldenFiles() throws Exception {
        JobExecution execution = jobLauncher.run(posttranJob, new JobParametersBuilder()
                .addString("outputDir", out.toString())
                .addLong("run", System.nanoTime())
                .toJobParameters());

        assertThat(execution.getStatus()).isEqualTo(BatchStatus.COMPLETED);
        int expectedRc = Integer.parseInt(read(GOLDEN.resolve("RETURN-CODE.txt")).get(0).trim());
        assertThat(execution.getExecutionContext().getInt(PostTranJobConfig.RETURN_CODE)).isEqualTo(expectedRc);

        List<String> transact = read(out.resolve(PostTranFiles.TRANSACT));
        assertThat(transact).allSatisfy(rec ->
                assertThat(rec.substring(PROC_TS_OFFSET, PROC_TS_OFFSET + PROC_TS_LENGTH))
                        .isEqualTo("2026-11-16-04.30.15.420000"));
        assertThat(maskProcTs(transact)).containsExactlyElementsOf(read(GOLDEN.resolve(PostTranFiles.TRANSACT)));

        for (String file : List.of(PostTranFiles.DALYREJS, PostTranFiles.ACCTFILE, PostTranFiles.TCATBALF,
                PostTranFiles.SYSOUT)) {
            assertThat(read(out.resolve(file))).as(file).containsExactlyElementsOf(read(GOLDEN.resolve(file)));
        }
        assertThat(Files.readAllBytes(out.resolve(PostTranFiles.DALYREJS)))
                .isEqualTo(Files.readAllBytes(GOLDEN.resolve(PostTranFiles.DALYREJS)));
    }

    @Test
    void rejectCodesAndCountsMatchCobol() throws Exception {
        jobLauncher.run(posttranJob, new JobParametersBuilder()
                .addString("outputDir", out.toString())
                .addLong("run", System.nanoTime())
                .toJobParameters());

        assertThat(reasonCounts(out)).isEqualTo(reasonCounts(GOLDEN));
        assertThat(read(out.resolve(PostTranFiles.TRANSACT))).hasSameSizeAs(read(GOLDEN.resolve(PostTranFiles.TRANSACT)));
    }

    private static java.util.Map<String, Long> reasonCounts(Path dir) throws Exception {
        return read(dir.resolve(PostTranFiles.DALYREJS)).stream()
                .collect(Collectors.groupingBy(r -> r.substring(350, 354), Collectors.counting()));
    }

    private static List<String> maskProcTs(List<String> records) {
        String mask = "#".repeat(PROC_TS_LENGTH);
        return records.stream()
                .map(r -> r.substring(0, PROC_TS_OFFSET) + mask + r.substring(PROC_TS_OFFSET + PROC_TS_LENGTH))
                .toList();
    }

    private static List<String> read(Path file) throws Exception {
        return Files.readAllLines(file, StandardCharsets.US_ASCII);
    }
}
