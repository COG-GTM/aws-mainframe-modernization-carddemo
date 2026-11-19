package com.carddemo.common.batch;

import org.junit.jupiter.api.Test;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class BatchArgumentsTest {

    @Test
    void parsesJobDdsRunIdAndPassThrough() {
        BatchArguments args = BatchArguments.parse("--job=posttran", "--dd=dalytran=/in/d.txt",
                "--dd=DALYREJS=/out/r", "--run-id=R1", "--spring.datasource.url=jdbc:x");
        assertThat(args.job()).isEqualTo("POSTTRAN");
        assertThat(args.dds()).containsExactly(
                java.util.Map.entry("DALYTRAN", "/in/d.txt"), java.util.Map.entry("DALYREJS", "/out/r"));
        assertThat(args.runId()).isEqualTo("R1");
        assertThat(args.passThrough()).containsExactly("--spring.datasource.url=jdbc:x");
    }

    @Test
    void rejectsInvalidLaunches() {
        assertThatThrownBy(() -> BatchArguments.parse("--dd=A=b")).hasMessageContaining("--job");
        assertThatThrownBy(() -> BatchArguments.parse("--job=TOOLONGNAME")).hasMessageContaining("Invalid job");
        assertThatThrownBy(() -> BatchArguments.parse("--job=A", "--dd=X=1", "--dd=x=2")).hasMessageContaining("more than once");
        assertThatThrownBy(() -> BatchArguments.parse("--job=A", "--dd=X")).hasMessageContaining("--dd must be");
        assertThatThrownBy(() -> BatchArguments.parse("--job=A", "--run-id=a/b")).hasMessageContaining("--run-id");
    }

    @Test
    void cliMapsUsageErrorsTo12AndExceptionsTo16() {
        ByteArrayOutputStream err = new ByteArrayOutputStream();
        PrintStream ps = new PrintStream(err, true, StandardCharsets.UTF_8);
        assertThat(BatchCli.run(new String[] {"--dd=A=b"}, a -> 0, ps)).isEqualTo(ReturnCode.JCL_ERROR);
        assertThat(BatchCli.run(new String[] {"--job=A"}, a -> {
            throw new IllegalStateException("wrapped", new BatchUsageException("unknown"));
        }, ps)).isEqualTo(ReturnCode.JCL_ERROR);
        assertThat(BatchCli.run(new String[] {"--job=A"}, a -> {
            throw new IllegalStateException("boom");
        }, ps)).isEqualTo(ReturnCode.ABEND);
        assertThat(BatchCli.run(new String[] {"--job=A"}, a -> 4, ps)).isEqualTo(ReturnCode.WARNING);
        assertThat(err.toString(StandardCharsets.UTF_8)).contains(BatchArguments.USAGE, "boom");
    }
}
