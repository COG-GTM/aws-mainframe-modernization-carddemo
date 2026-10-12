package com.carddemo.zerotrust.posting;

import com.carddemo.posting.domain.RejectRecordLayout;
import com.carddemo.recordio.codec.RecordEncoding;
import com.carddemo.recordio.layout.Transaction;
import com.carddemo.recordio.layout.TransactionLayout;
import com.carddemo.recordio.store.FixedWidthFile;
import com.carddemo.zerotrust.audit.AuditChainVerifier;
import com.carddemo.zerotrust.audit.AuditEntry;
import com.carddemo.zerotrust.audit.VerificationResult;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.test.JobLauncherTestUtils;
import org.springframework.batch.test.context.SpringBatchTest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Runs the secured posting job end to end on the shipped EBCDIC datasets: the real posttran
 * steps execute under token verification, per-item authorization and the hash-chained audit
 * log. Leaves the audit log at {@code target/zero-trust-evidence/posting-audit.jsonl} for
 * verify.sh to re-verify from the CLI.
 */
@SpringBatchTest
@SpringBootTest(classes = SecuredPostingApplication.class, properties = {
        "spring.batch.job.enabled=false",
        "carddemo.posting.encoding=EBCDIC",
        "carddemo.posting.daily-transactions=${carddemo.test.data}/AWS.M2.CARDDEMO.DALYTRAN.PS",
        "carddemo.posting.card-xref=${carddemo.test.data}/AWS.M2.CARDDEMO.CARDXREF.PS",
        "carddemo.posting.account-master=${carddemo.test.work}/ACCTDATA",
        "carddemo.posting.category-balances=${carddemo.test.work}/TCATBALF",
        "carddemo.posting.transaction-master=${carddemo.test.work}/TRANSACT",
        "carddemo.posting.rejects=${carddemo.test.work}/DALYREJS",
        "carddemo.zerotrust.audit-log=${carddemo.test.audit}"
})
class SecuredPostingJobTest {

    static final Path DATA = Path.of("../../app/data/EBCDIC").toAbsolutePath().normalize();
    static final Path WORK = Path.of("target/zt-posting-work").toAbsolutePath();
    static final Path AUDIT = Path.of("target/zero-trust-evidence/posting-audit.jsonl").toAbsolutePath();

    /** Reject count produced by the wrapped posttran job on the shipped data. */
    static final long EXPECTED_REJECTS = 38L;

    @Autowired
    JobLauncherTestUtils jobLauncherTestUtils;

    @BeforeAll
    static void stageMasters() throws Exception {
        Files.createDirectories(WORK);
        Files.createDirectories(AUDIT.getParent());
        Files.copy(DATA.resolve("AWS.M2.CARDDEMO.ACCTDATA.PS"), WORK.resolve("ACCTDATA"),
                java.nio.file.StandardCopyOption.REPLACE_EXISTING);
        Files.copy(DATA.resolve("AWS.M2.CARDDEMO.TCATBALF.PS"), WORK.resolve("TCATBALF"),
                java.nio.file.StandardCopyOption.REPLACE_EXISTING);
        Files.deleteIfExists(AUDIT);
        Files.deleteIfExists(WORK.resolve("TRANSACT"));
        Files.deleteIfExists(WORK.resolve("DALYREJS"));
        System.setProperty("carddemo.test.data", DATA.toString());
        System.setProperty("carddemo.test.work", WORK.toString());
        System.setProperty("carddemo.test.audit", AUDIT.toString());
    }

    @Test
    void postsShippedDailyTransactionsUnderZeroTrustControls() throws Exception {
        JobExecution exec = jobLauncherTestUtils.launchJob(new JobParametersBuilder()
                .addLong("run", System.nanoTime()).toJobParameters());
        assertThat(exec.getStatus()).isEqualTo(BatchStatus.COMPLETED);

        var ctx = exec.getExecutionContext();
        long total = ctx.getLong("transactionCount");
        long rejects = ctx.getLong("rejectCount");
        assertThat(total).isEqualTo(300);
        assertThat(rejects).isEqualTo(EXPECTED_REJECTS);

        List<Transaction> posted = FixedWidthFile.readAll(WORK.resolve("TRANSACT"),
                TransactionLayout.INSTANCE, RecordEncoding.EBCDIC);
        assertThat(posted).hasSize((int) (total - rejects));
        assertThat(Files.size(WORK.resolve("TRANSACT")) % Transaction.LENGTH).isZero();
        assertThat(Files.size(WORK.resolve("DALYREJS")))
                .isEqualTo(rejects * RejectRecordLayout.LENGTH);

        // Audit chain: 1 JOB_STARTED + one entry per item + 1 JOB_COMPLETED.
        List<AuditEntry> entries = Files.readAllLines(AUDIT).stream()
                .map(AuditEntry::fromJson).toList();
        assertThat(entries).hasSize((int) total + 2);
        assertThat(entries.get(0).action()).isEqualTo("JOB_STARTED");
        assertThat(entries.get(entries.size() - 1).action()).isEqualTo("JOB_COMPLETED");
        assertThat(entries.stream()
                .filter(e -> e.action().equals("TRANSACTION_POSTED")).count())
                .isEqualTo(total - rejects);
        assertThat(entries.stream()
                .filter(e -> e.action().equals("TRANSACTION_REJECTED")).count())
                .isEqualTo(rejects);

        VerificationResult result = AuditChainVerifier.verify(AUDIT);
        assertThat(result.valid()).as(result.problem()).isTrue();
        assertThat(ctx.getString("auditHeadHash"))
                .isEqualTo(entries.get(entries.size() - 1).hash());

        // No full card number from the shipped CARDXREF appears anywhere in the audit log.
        String auditText = Files.readString(AUDIT, StandardCharsets.UTF_8);
        List<String> cards = FixedWidthFile.readAll(
                        DATA.resolve("AWS.M2.CARDDEMO.CARDXREF.PS"),
                        com.carddemo.recordio.layout.CardXrefLayout.INSTANCE,
                        RecordEncoding.EBCDIC)
                .stream().map(com.carddemo.recordio.layout.CardXref::cardNumber).toList();
        assertThat(cards).isNotEmpty();
        for (String card : cards) {
            assertThat(auditText).doesNotContain(card);
        }

        // Posted-amount control total: sum of posted transaction amounts is in JOB_COMPLETED.
        BigDecimal postedSum = posted.stream().map(Transaction::amount)
                .reduce(BigDecimal.ZERO, BigDecimal::add);
        assertThat(entries.get(entries.size() - 1).detail())
                .contains("postedAmount=" + postedSum.toPlainString());
    }
}
