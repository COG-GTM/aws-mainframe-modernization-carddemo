package com.carddemo.interestcalc.batch;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.interestcalc.parity.GoldenMaster;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.batch.core.BatchStatus;

/**
 * The process return code is the only thing a scheduler sees, so it is asserted on a real forked
 * JVM rather than on {@code BatchStatus}: a job that abends must not look like a clean step.
 */
class ProcessExitCodeTest {

    @Test
    @DisplayName("A completed run returns 0")
    void completedRunReturnsZero(@TempDir Path outputDirectory) throws Exception {
        Run run = launch(GoldenMaster.shippedDataset().toString(), outputDirectory);

        assertThat(run.exitCode()).as(run.output()).isZero();
        assertThat(outputDirectory.resolve(InterestCalculationTasklet.TRANSACT_OUT)).exists();
    }

    @Test
    @DisplayName("A missing input dataset returns a non-zero RC and writes no TRANSACT file")
    void missingDatasetReturnsNonZero(@TempDir Path inputDirectory, @TempDir Path outputDirectory) throws Exception {
        // tcatbal.txt is present, acctdata.txt is not: the job abends on ACCTFILE.
        Files.copy(
                GoldenMaster.shippedDataset().resolve("tcatbal.txt"),
                inputDirectory.resolve("tcatbal.txt"));

        Run run = launch(inputDirectory.toString(), outputDirectory);

        assertThat(run.exitCode())
                .as("a failed job must not report the same RC as a clean one%n%s", run.output())
                .isEqualTo(BatchStatus.FAILED.ordinal());
        assertThat(outputDirectory.resolve(InterestCalculationTasklet.TRANSACT_OUT))
                .as("no partial output")
                .doesNotExist();
    }

    private record Run(int exitCode, String output) {}

    private static Run launch(String inputDirectory, Path outputDirectory) throws IOException, InterruptedException {
        List<String> command = new ArrayList<>(List.of(
                Path.of(System.getProperty("java.home"), "bin", "java").toString(),
                "-cp",
                System.getProperty("java.class.path"),
                "com.carddemo.interestcalc.InterestCalculatorApplication",
                "runDate=" + GoldenMaster.RUN_DATE,
                "inputDirectory=" + inputDirectory,
                "outputDirectory=" + outputDirectory));

        Process process = new ProcessBuilder(command).redirectErrorStream(true).start();
        String output = new String(process.getInputStream().readAllBytes());
        assertThat(process.waitFor(5, TimeUnit.MINUTES)).as("the job must terminate").isTrue();
        return new Run(process.exitValue(), output);
    }
}
