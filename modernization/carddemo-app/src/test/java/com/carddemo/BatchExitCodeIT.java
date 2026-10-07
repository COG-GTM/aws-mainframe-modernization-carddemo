package com.carddemo;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.common.codec.TestData;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

/**
 * {@code main} with {@code --job=} (and Boot's {@code --spring.batch.job.name=}) boots without a web server, runs the
 * job and returns its JCL condition code as the process exit code; a failing job is never 0.
 */
@Testcontainers
class BatchExitCodeIT {

    @Container
    static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:16-alpine");

    @TempDir
    Path dir;

    private int run(String... jobArgs) {
        List<String> args = new ArrayList<>(List.of("--spring.datasource.url=" + postgres.getJdbcUrl(),
                "--spring.datasource.username=" + postgres.getUsername(),
                "--spring.datasource.password=" + postgres.getPassword(),
                "--carddemo.initial-load.on-startup=false", "--spring.main.banner-mode=off"));
        args.addAll(List.of(jobArgs));
        return CardDemoApplication.runBatch(args.toArray(String[]::new));
    }

    @Test
    void successfulJobExitsZero() throws Exception {
        assertThat(run("--job=READCARD", "--CARDFILE=" + TestData.resolve("app/data/ASCII/carddata.txt"),
                "--encoding=ASCII", "--SYSOUT=" + dir.resolve("sysout.txt"))).isZero();
        assertThat(Files.readAllLines(dir.resolve("sysout.txt"))).hasSize(52)
                .startsWith("START OF EXECUTION OF PROGRAM CBACT02C");
    }

    @Test
    void failedJobLaunchedThroughSpringBatchJobNameExitsNonZero() {
        assertThat(run("--spring.batch.job.name=readcard", "--CARDFILE=" + dir.resolve("missing"),
                "--SYSOUT=" + dir.resolve("sysout.txt"))).isEqualTo(16);
        assertThat(run("--job=NOSUCHJOB")).isEqualTo(16);
    }

    @Test
    void contextThatCannotStartExits16() {
        assertThat(CardDemoApplication.runBatch("--job=READCARD", "--spring.main.banner-mode=off",
                "--spring.datasource.url=jdbc:postgresql://127.0.0.1:1/none", "--spring.datasource.password=x",
                "--spring.datasource.hikari.connection-timeout=2000")).isEqualTo(16);
    }
}
