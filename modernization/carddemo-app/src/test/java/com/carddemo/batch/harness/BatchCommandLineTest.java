package com.carddemo.batch.harness;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import java.time.LocalDate;
import java.util.Map;
import org.junit.jupiter.api.Test;
import org.springframework.boot.DefaultApplicationArguments;

class BatchCommandLineTest {

    private static BatchCommandLine.Request parse(String... args) {
        return BatchCommandLine.parse(new DefaultApplicationArguments(args)).orElseThrow();
    }

    @Test
    void jobRunDateAndDdParameters() {
        BatchCommandLine.Request r = parse("--job=READACCT", "--run-date=2022-07-06", "--ACCTFILE=table",
                "--OUTFILE=/tmp/out", "--spring.profiles.active=golden", "--carddemo.x=1", "--logging.level.root=WARN",
                "--debug", "--flag");
        assertThat(r.jobName()).isEqualTo("READACCT");
        assertThat(r.runDate()).isEqualTo(LocalDate.of(2022, 7, 6));
        assertThat(r.parameters()).containsOnly(Map.entry("ACCTFILE", "table"), Map.entry("OUTFILE", "/tmp/out"),
                Map.entry("flag", "true"));
    }

    @Test
    void bootJobNameIsAnAlias() {
        assertThat(parse("--spring.batch.job.name=readcard").jobName()).isEqualTo("readcard");
        assertThat(parse("--job=readcard").runDate()).isNull();
        assertThat(BatchCommandLine.isBatchLaunch("--spring.batch.job.name=readcard")).isTrue();
        assertThat(BatchCommandLine.isBatchLaunch("--job=x", "--y=z")).isTrue();
        assertThat(BatchCommandLine.isBatchLaunch("--server.port=0")).isFalse();
    }

    @Test
    void noJobMeansNoLaunch() {
        assertThat(BatchCommandLine.parse(new DefaultApplicationArguments("--server.port=0"))).isEmpty();
    }

    @Test
    void malformedRequestsAreRejected() {
        assertThatThrownBy(() -> parse("--job=")).hasMessageContaining("--job needs a job name");
        assertThatThrownBy(() -> parse("--job=a", "--job=b")).hasMessageContaining("given 2 times");
        assertThatThrownBy(() -> parse("--job=a", "--run-date=2022-13-01")).hasMessageContaining("YYYY-MM-DD");
        assertThatThrownBy(() -> parse("--job=a", "stray")).hasMessageContaining("unexpected arguments");
        assertThat(BatchCommandLine.isBatchLaunch("--job")).isTrue();
        assertThat(BatchCommandLine.isBatchLaunch("--spring.batch.job.name")).isTrue();
        assertThat(BatchCommandLine.isBatchLaunch("--jobs=a", "--server.port=0")).isFalse();
        assertThatThrownBy(() -> parse("--job")).hasMessageContaining("--job needs a job name");
    }

    @Test
    void credentialsAreNeverJobParameters() {
        assertThatThrownBy(() -> parse("--job=a", "--db-password=s3cret"))
                .hasMessageContaining("credentials are not job parameters").hasMessageNotContaining("s3cret");
        assertThatThrownBy(() -> parse("--job=a", "--API_KEY=x")).hasMessageContaining("--API_KEY");
        assertThat(BatchCommandLine.isSensitive("OUTFILE")).isFalse();
    }
}
