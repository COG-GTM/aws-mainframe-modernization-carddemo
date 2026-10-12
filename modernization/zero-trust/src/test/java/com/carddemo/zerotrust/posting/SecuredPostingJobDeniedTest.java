package com.carddemo.zerotrust.posting;

import com.carddemo.zerotrust.audit.AuditEntry;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.test.JobLauncherTestUtils;
import org.springframework.batch.test.context.SpringBatchTest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Same job, but the workload is {@code svc-posttran-report}: its token verifies yet it has
 * no {@code run-posting} grant, so the job is FAILED before any step runs and nothing is
 * written to the masters or output datasets.
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
        "carddemo.zerotrust.workload-id=svc-posttran-report",
        "carddemo.zerotrust.audit-log=${carddemo.test.audit}"
})
class SecuredPostingJobDeniedTest {

    static final Path DATA = Path.of("../../app/data/EBCDIC").toAbsolutePath().normalize();
    static final Path WORK = Path.of("target/zt-denied-work").toAbsolutePath();
    static final Path AUDIT = Path.of("target/zt-denied-work/audit.jsonl").toAbsolutePath();

    @Autowired
    JobLauncherTestUtils jobLauncherTestUtils;

    @BeforeAll
    static void stageMasters() throws Exception {
        Files.createDirectories(WORK);
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
    void unprivilegedWorkloadCannotRunTheJob() throws Exception {
        JobExecution exec = jobLauncherTestUtils.launchJob(new JobParametersBuilder()
                .addLong("run", System.nanoTime()).toJobParameters());
        assertThat(exec.getStatus()).isEqualTo(BatchStatus.FAILED);
        assertThat(exec.getExitStatus().getExitDescription())
                .contains("zero trust enforcement");

        // Nothing was posted: no output datasets were written.
        assertThat(Files.exists(WORK.resolve("TRANSACT"))).isFalse();
        assertThat(Files.exists(WORK.resolve("DALYREJS"))).isFalse();

        List<AuditEntry> entries = Files.readAllLines(AUDIT).stream()
                .map(AuditEntry::fromJson).toList();
        assertThat(entries).hasSize(1);
        assertThat(entries.get(0).action()).isEqualTo("AUTHZ_DECISION");
        assertThat(entries.get(0).outcome()).isEqualTo("DENY");
        assertThat(entries.get(0).actor()).isEqualTo("svc-posttran-report");
    }
}
