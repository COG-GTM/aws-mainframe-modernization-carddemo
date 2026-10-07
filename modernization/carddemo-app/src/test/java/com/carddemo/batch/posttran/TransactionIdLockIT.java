package com.carddemo.batch.posttran;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.batch.harness.BatchJobLauncher;
import com.carddemo.batch.harness.JobChain;
import com.carddemo.batch.harness.JobStream;
import com.carddemo.batch.harness.ReturnCode;
import com.carddemo.batch.load.InitialLoadJobConfiguration;
import com.carddemo.batch.load.LoadMode;
import com.carddemo.common.codec.TestData;
import com.carddemo.transaction.TransactionRecord;
import com.carddemo.transaction.TransactionRepository;
import com.carddemo.transaction.online.TransactionIds;
import java.math.BigDecimal;
import java.nio.file.Path;
import java.time.Duration;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.testcontainers.service.connection.ServiceConnection;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.support.TransactionTemplate;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

/**
 * s6.4: POSTTRAN's TRANFILE writes take {@link TransactionRepository#TRAN_ID_LOCK}, the advisory lock online
 * {@link TransactionIds} holds from reading the highest id to commit. A held online lock blocks the posting until it
 * is released; online adds racing the postings (after TRANFILE's OPEN OUTPUT emptied the table) all succeed with
 * distinct ids and the run still posts every record.
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.NONE, properties = {
        "carddemo.initial-load.on-startup=false", "carddemo.batch.output-dir=target/tran-id-lock-it-output"})
@ActiveProfiles("golden")
@Testcontainers
class TransactionIdLockIT {

    private static final int POSTED = 262;
    private static final int ONLINE_ADDS = 20;

    @Container
    @ServiceConnection
    static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:16-alpine");

    @Autowired
    BatchJobLauncher launcher;
    @Autowired
    List<JobStream> streams;
    @Autowired
    TransactionRepository transactions;
    @Autowired
    TransactionIds ids;
    @Autowired
    PlatformTransactionManager transactionManager;
    @Autowired
    JdbcTemplate jdbc;

    @TempDir
    Path dir;

    @BeforeEach
    void initialLoad() {
        JobParameters load = new JobParametersBuilder(InitialLoadJobConfiguration.parameters(
                TestData.resolve("app/data/EBCDIC"), LoadMode.REPLACE)).addLong("run.id", System.nanoTime())
                .toJobParameters();
        assertThat(launcher.run("initial-load", load).returnCode()).isEqualTo(ReturnCode.OK);
    }

    private JobChain.Result posttran() {
        JobParameters parameters = new JobParametersBuilder().addLocalDate("run-date", LocalDate.of(2022, 7, 6))
                .addLong("run.id", System.nanoTime()).addString("encoding", "ASCII")
                .addString("STEP10.SYSOUT", dir.resolve("cbtrn01c-" + System.nanoTime() + ".txt").toString())
                .addString("STEP15.SYSOUT", dir.resolve("cbtrn02c-" + System.nanoTime() + ".txt").toString())
                .toJobParameters();
        JobStream stream = streams.stream().filter(s -> s.name().equals(PosttranJobConfiguration.POSTTRAN))
                .findFirst().orElseThrow();
        return stream.chain(launcher, parameters).run();
    }

    private int waitingAdvisoryLocks() {
        return jdbc.queryForObject("select count(*) from pg_locks where locktype = 'advisory' and not granted",
                Integer.class);
    }

    private static TransactionRecord online(String id) {
        return new TransactionRecord(id, "01", 1, "POS TERM", "Online add racing POSTTRAN", new BigDecimal("-1.00"),
                0, "", "", "", "0500024453765740", "2022-07-06 10:00:00.000000", "2022-07-06 10:00:00.000000");
    }

    @Test
    void anOnlineIdLockBlocksThePostingUntilItCommits() throws Exception {
        CountDownLatch locked = new CountDownLatch(1);
        CountDownLatch release = new CountDownLatch(1);
        ExecutorService pool = Executors.newFixedThreadPool(2);
        try {
            Future<?> onlineWriter = pool.submit(() -> new TransactionTemplate(transactionManager)
                    .executeWithoutResult(s -> {
                        transactions.lockIdAssignment(TransactionRepository.TRAN_ID_LOCK);
                        locked.countDown();
                        try {
                            release.await();
                        } catch (InterruptedException e) {
                            Thread.currentThread().interrupt();
                        }
                    }));
            assertThat(locked.await(30, TimeUnit.SECONDS)).isTrue();
            Future<JobChain.Result> run = pool.submit(this::posttran);

            long deadline = System.nanoTime() + Duration.ofSeconds(60).toNanos();
            while (waitingAdvisoryLocks() == 0 && System.nanoTime() < deadline) {
                Thread.sleep(50);
            }
            assertThat(waitingAdvisoryLocks()).as("POSTTRAN waits for the id lock").isEqualTo(1);
            assertThat(run).isNotDone();
            assertThat(transactions.count()).isZero();

            release.countDown();
            onlineWriter.get(30, TimeUnit.SECONDS);
            assertThat(run.get(120, TimeUnit.SECONDS).maxReturnCode()).isEqualTo(ReturnCode.WARNING);
            assertThat(transactions.count()).isEqualTo(POSTED);
        } finally {
            release.countDown();
            pool.shutdownNow();
        }
    }

    @Test
    void onlineAddsRacingPosttranAllSucceed() throws Exception {
        ExecutorService pool = Executors.newFixedThreadPool(5);
        try {
            CompletableFuture<JobChain.Result> run = CompletableFuture.supplyAsync(this::posttran, pool);
            // TRANFILE is opened OUTPUT (the step replaces TRANSACT), so race the adds against the postings
            long deadline = System.nanoTime() + Duration.ofSeconds(60).toNanos();
            while (transactions.count() == 0 && !run.isDone() && System.nanoTime() < deadline) {
                Thread.sleep(5);
            }
            TransactionTemplate tx = new TransactionTemplate(transactionManager);
            List<Future<String>> adds = new ArrayList<>();
            for (int i = 0; i < ONLINE_ADDS; i++) {
                adds.add(pool.submit(() -> tx.execute(s -> ids.write(online(ids.next()), "add").getTranId())));
            }
            List<String> onlineIds = new ArrayList<>();
            for (Future<String> add : adds) {
                onlineIds.add(add.get(120, TimeUnit.SECONDS));
            }
            assertThat(run.get(120, TimeUnit.SECONDS).maxReturnCode()).isEqualTo(ReturnCode.WARNING);
            assertThat(new HashSet<>(onlineIds)).hasSize(ONLINE_ADDS);
            assertThat(transactions.count()).isEqualTo(POSTED + ONLINE_ADDS);
            assertThat(jdbc.queryForObject("select count(*) from transaction where description like 'Online add%'",
                    Integer.class)).isEqualTo(ONLINE_ADDS);
        } finally {
            pool.shutdownNow();
        }
    }
}
