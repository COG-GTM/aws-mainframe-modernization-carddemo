package com.carddemo.common.batch;

import com.carddemo.common.record.CodePage;
import com.carddemo.common.record.RecordFormat;

import org.junit.jupiter.api.Test;

import java.nio.file.Path;
import java.time.LocalDate;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class BatchArgumentsTest {

    @Test
    void parsesTheGoldenSetLaunch() {
        BatchArguments args = BatchArguments.parse("--job=posttran", "--date=2022-07-18",
                "--dd=DALYTRAN=in/dailytran.txt", "--output-dir=out", "--spring.datasource.url=jdbc:x").orElseThrow();

        assertThat(args.job()).isEqualTo("POSTTRAN");
        assertThat(args.runDate()).isEqualTo(LocalDate.of(2022, 7, 18));
        assertThat(args.ddPath("dalytran")).contains(Path.of("in/dailytran.txt"));
        assertThat(args.outputRoot()).isEqualTo(Path.of("out"));
        assertThat(args.parm()).isNull();
        assertThat(args.codePage()).isEqualTo(CodePage.ASCII);
        assertThat(args.format()).isEqualTo(RecordFormat.FIXED);
    }

    @Test
    void parsesParmBasicDateCodePageAndFormat() {
        BatchArguments args = BatchArguments.parse("--job=INTCALC", "--parm=2022071800", "--date=20220718",
                "--codepage=ebcdic", "--format=line").orElseThrow();

        assertThat(args.parm()).isEqualTo("2022071800");
        assertThat(args.runDate()).isEqualTo(LocalDate.of(2022, 7, 18));
        assertThat(args.codePage()).isEqualTo(CodePage.EBCDIC);
        assertThat(args.format()).isEqualTo(RecordFormat.LINE_SEQUENTIAL);
    }

    @Test
    void noJobMeansTheWebService() {
        assertThat(BatchArguments.parse("--server.port=9000")).isEmpty();
        assertThat(BatchArguments.isBatchLaunch("--server.port=9000")).isFalse();
        assertThat(BatchArguments.isBatchLaunch("--job=POSTTRAN")).isTrue();
    }

    @Test
    void rejectsMalformedOptions() {
        assertThatThrownBy(() -> BatchArguments.parse("--job=POSTTRAN", "--date=18/07/2022"))
                .hasMessageContaining("--date");
        assertThatThrownBy(() -> BatchArguments.parse("--job=POSTTRAN", "--dd=DALYTRAN"))
                .hasMessageContaining("DDNAME=path");
        assertThatThrownBy(() -> BatchArguments.parse("--job=POSTTRAN", "--codepage=utf8"))
                .hasMessageContaining("--codepage");
        assertThatThrownBy(() -> BatchArguments.parse("--job="))
                .hasMessageContaining("--job");
    }
}
