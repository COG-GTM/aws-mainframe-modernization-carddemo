package com.carddemo.common.batch;

import com.carddemo.common.cobol.CodePage;
import com.carddemo.common.error.FileStatus;
import com.carddemo.common.error.FileStatusException;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class BatchCliTest {

    private final ByteArrayOutputStream errBytes = new ByteArrayOutputStream();
    private final PrintStream err = new PrintStream(errBytes, true, StandardCharsets.UTF_8);

    private String err() {
        return errBytes.toString(StandardCharsets.UTF_8);
    }

    @Test
    void parsesTheLaunchContract() {
        BatchArguments args = BatchArguments.parse("--job=posttran", "--dd=dalytran=app/data/ASCII/dailytran.txt",
                "--output-dir=/tmp/out", "--dd-encoding=CP037", "--spring.profiles.active=local");
        assertThat(args.job()).isEqualTo("POSTTRAN");
        assertThat(args.dd("DALYTRAN")).isEqualTo(Path.of("app/data/ASCII/dailytran.txt"));
        assertThat(args.outputDir()).contains(Path.of("/tmp/out"));
        assertThat(args.ddEncoding()).isEqualTo(CodePage.CP037);
        assertThat(args.passThrough()).containsExactly("--spring.profiles.active=local");
        assertThat(BatchArguments.parse("--job=POSTTRAN").ddEncoding()).isEqualTo(CodePage.ASCII);
        assertThatThrownBy(() -> args.dd("TRANFILE")).isInstanceOf(BatchUsageException.class);
    }

    @Test
    void detectsBatchLaunch() {
        assertThat(BatchCli.requested("--job=POSTTRAN")).isTrue();
        assertThat(BatchCli.requested("--spring.profiles.active=local")).isFalse();
    }

    @Test
    void exitCodeIsTheJobConditionCode() {
        assertThat(BatchCli.run(new String[] {"--job=POSTTRAN"}, a -> ReturnCode.OK, err)).isZero();
        assertThat(BatchCli.run(new String[] {"--job=POSTTRAN"}, a -> ReturnCode.WARNING, err)).isEqualTo(4);
    }

    @Test
    void usageErrorsAreRc12() {
        assertThat(BatchCli.run(new String[] {}, a -> 0, err)).isEqualTo(ReturnCode.JCL_ERROR);
        assertThat(err()).contains("--job is required").contains("usage:");
        assertThat(BatchCli.run(new String[] {"--job=TOOLONGNAME"}, a -> 0, err)).isEqualTo(12);
        assertThat(BatchCli.run(new String[] {"--job=A", "--job=B"}, a -> 0, err)).isEqualTo(12);
        assertThat(BatchCli.run(new String[] {"--job=A", "--dd=NOFILE"}, a -> 0, err)).isEqualTo(12);
        assertThat(BatchCli.run(new String[] {"--job=A", "--dd=X=a", "--dd=x=b"}, a -> 0, err)).isEqualTo(12);
        assertThat(BatchCli.run(new String[] {"--job=A", "--dd-encoding=UTF8"}, a -> 0, err)).isEqualTo(12);
        assertThat(BatchCli.run(new String[] {"--job=A", "--output-dir="}, a -> 0, err)).isEqualTo(12);
        assertThat(BatchCli.run(new String[] {"--job"}, a -> 0, err)).isEqualTo(12);
        assertThat(BatchCli.run(new String[] {"--job=A"}, a -> a.dd("DALYTRAN") == null ? 0 : 0, err)).isEqualTo(12);
    }

    @Test
    void abendsAndUnexpectedFailuresAreRc16() {
        assertThat(BatchCli.run(new String[] {"--job=POSTTRAN"}, a -> {
            throw FileStatusException.of(FileStatus.DUPLICATE_KEY, "TRANFILE", "WRITE");
        }, err)).isEqualTo(ReturnCode.ABEND);
        assertThat(err()).contains("abended (ABCODE 999)").contains("FILE STATUS 22");
        assertThat(BatchCli.run(new String[] {"--job=POSTTRAN"}, a -> {
            throw new IllegalStateException("boom");
        }, err)).isEqualTo(16);
        assertThat(BatchCli.run(new String[] {"--job=POSTTRAN"}, a -> {
            throw new OutOfMemoryError("simulated");
        }, err)).isEqualTo(16);
        assertThat(BatchCli.run(new String[] {"--job=POSTTRAN"}, a -> -1, err)).isEqualTo(16);
    }

    @Test
    void returnCodeHelpers() {
        assertThat(ReturnCode.highest(0, 4, 0)).isEqualTo(4);
        assertThat(ReturnCode.highest()).isZero();
        assertThat(ReturnCode.isSuccess(4)).isTrue();
        assertThat(ReturnCode.isSuccess(12)).isFalse();
        assertThat(ReturnCode.isSuccess(16)).isFalse();
        assertThatThrownBy(() -> ReturnCode.validate(4096)).isInstanceOf(IllegalArgumentException.class);
    }
}
