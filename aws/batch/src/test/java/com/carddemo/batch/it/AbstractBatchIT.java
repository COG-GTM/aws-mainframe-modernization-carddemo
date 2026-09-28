package com.carddemo.batch.it;

import com.carddemo.batch.core.JobRunner;
import com.carddemo.batch.core.ReturnCode;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeEach;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Import;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.utility.DockerImageName;

/**
 * Aurora PostgreSQL stand-in (Testcontainers {@code postgres:16-alpine}) with the test schema from
 * {@code src/test/resources/db/schema.sql}, a local directory as the S3 bucket seeded with
 * {@code app/data/ASCII/} under {@code seed/ascii/}, and a fixed clock (2022-07-18T10:00Z).
 */
@SpringBootTest(properties = "spring.main.web-application-type=none")
@Import(AbstractBatchIT.FixedClock.class)
public abstract class AbstractBatchIT {

    static final Path REPO_ROOT = Path.of(System.getProperty("carddemo.repo.root", "../..")).toAbsolutePath()
            .normalize();
    static final Path ASCII = REPO_ROOT.resolve("app/data/ASCII");
    static final Path BUCKET;
    static final PostgreSQLContainer<?> POSTGRES = new PostgreSQLContainer<>(
            DockerImageName.parse("postgres:16-alpine")).withDatabaseName("carddemo");
    static final Instant NOW = Instant.parse("2022-07-18T10:00:00Z");
    static final List<String> SEED_ORDER = List.of("customer", "account", "card", "card_xref", "transaction_type",
            "transaction_category", "disclosure_group", "tran_cat_balance");
    static final List<String> ALL_TABLES = List.of("daily_transaction", "batch_job_run", "transaction",
            "tran_cat_balance", "disclosure_group", "transaction_category", "transaction_type", "card_xref", "card",
            "customer", "account");

    static {
        POSTGRES.start();
        try {
            BUCKET = Files.createTempDirectory("carddemo-bucket");
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
        try (var c = java.sql.DriverManager.getConnection(POSTGRES.getJdbcUrl(), POSTGRES.getUsername(),
                POSTGRES.getPassword()); var st = c.createStatement()) {
            st.execute(Files.readString(Path.of("src/test/resources/db/schema.sql")));
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }
    }

    @DynamicPropertySource
    static void props(DynamicPropertyRegistry r) {
        r.add("DB_HOST", POSTGRES::getHost);
        r.add("DB_PORT", () -> POSTGRES.getMappedPort(5432));
        r.add("DB_NAME", POSTGRES::getDatabaseName);
        r.add("DB_USER", POSTGRES::getUsername);
        r.add("DB_PASSWORD", POSTGRES::getPassword);
        r.add("DB_SCHEMA", () -> "carddemo");
        r.add("carddemo.storage.local-dir", BUCKET::toString);
    }

    @TestConfiguration
    static class FixedClock {
        @Bean
        Clock clock() {
            return Clock.fixed(NOW, ZoneOffset.UTC);
        }
    }

    @Autowired
    protected JobRunner runner;

    @Autowired
    protected JdbcTemplate jdbc;

    @BeforeEach
    void resetState() throws IOException {
        jdbc.execute("TRUNCATE " + String.join(", ", ALL_TABLES));
        try (Stream<Path> files = Files.walk(BUCKET)) {
            files.sorted((a, b) -> b.getNameCount() - a.getNameCount()).filter(p -> !p.equals(BUCKET))
                    .forEach(p -> p.toFile().delete());
        }
        Path seed = Files.createDirectories(BUCKET.resolve("seed/ascii"));
        try (Stream<Path> files = Files.list(ASCII)) {
            for (Path f : files.toList()) {
                Files.copy(f, seed.resolve(f.getFileName()), StandardCopyOption.REPLACE_EXISTING);
            }
        }
        put("input/dalytran/2022-07-18/dalytran.txt", Files.readString(ASCII.resolve("dailytran.txt")));
        for (String table : SEED_ORDER) {
            int rc = run("load-reference-data", "seed-" + table, "--table=" + table);
            if (rc != ReturnCode.OK) {
                throw new IllegalStateException("seed load of " + table + " failed with " + rc);
            }
        }
    }

    protected int run(String job, String runId, String... extra) {
        String[] base = {"--job=" + job, "--runId=" + runId, "--businessDate=2022-07-18"};
        String[] all = Arrays.copyOf(base, base.length + extra.length);
        System.arraycopy(extra, 0, all, base.length, extra.length);
        return runner.run(all);
    }

    protected static void put(String key, String content) throws IOException {
        Path p = BUCKET.resolve(key);
        Files.createDirectories(p.getParent());
        Files.writeString(p, content, StandardCharsets.US_ASCII);
    }

    protected static String read(String key) throws IOException {
        return Files.readString(BUCKET.resolve(key), StandardCharsets.UTF_8);
    }

    protected static byte[] readBytes(String key) throws IOException {
        return Files.readAllBytes(BUCKET.resolve(key));
    }

    protected static boolean exists(String key) {
        return Files.exists(BUCKET.resolve(key));
    }

    protected static List<String> golden(String path) throws IOException {
        return lines(Files.readString(Path.of("src/test/resources/golden").resolve(path)));
    }

    protected static List<String> lines(String content) {
        return content.lines().filter(l -> !l.isEmpty()).toList();
    }
}
