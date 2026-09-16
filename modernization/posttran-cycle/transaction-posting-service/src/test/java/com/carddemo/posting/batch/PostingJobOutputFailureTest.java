package com.carddemo.posting.batch;

import com.carddemo.posting.PostingApplication;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.test.JobLauncherTestUtils;
import org.springframework.batch.test.context.SpringBatchTest;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * CBTRN02C abends (CEE3ABD 999) when an output dataset cannot be written or closed. The Java job must
 * likewise end FAILED, not COMPLETED, when persisting one of the four outputs fails after posting.
 * Here TRANSACT is placed under a path whose parent is a regular file, so the write cannot succeed.
 */
@SpringBatchTest
@SpringBootTest(classes = PostingApplication.class, properties = {
        "spring.batch.job.enabled=false",
        "carddemo.posting.encoding=EBCDIC",
        "carddemo.posting.daily-transactions=${carddemo.test.data}/AWS.M2.CARDDEMO.DALYTRAN.PS",
        "carddemo.posting.card-xref=${carddemo.test.data}/AWS.M2.CARDDEMO.CARDXREF.PS",
        "carddemo.posting.account-master=${carddemo.test.failwork}/ACCTDATA",
        "carddemo.posting.category-balances=${carddemo.test.failwork}/TCATBALF",
        "carddemo.posting.transaction-master=${carddemo.test.failwork}/not-a-directory/TRANSACT",
        "carddemo.posting.rejects=${carddemo.test.failwork}/DALYREJS"
})
class PostingJobOutputFailureTest {

    static final Path DATA = Path.of("../../../app/data/EBCDIC").toAbsolutePath().normalize();
    static final Path WORK = Path.of("target/posting-fail-work").toAbsolutePath();

    @Autowired
    JobLauncherTestUtils jobLauncherTestUtils;

    @BeforeAll
    static void stageMasters() throws IOException {
        Files.createDirectories(WORK);
        Files.copy(DATA.resolve("AWS.M2.CARDDEMO.ACCTDATA.PS"), WORK.resolve("ACCTDATA"), java.nio.file.StandardCopyOption.REPLACE_EXISTING);
        Files.copy(DATA.resolve("AWS.M2.CARDDEMO.TCATBALF.PS"), WORK.resolve("TCATBALF"), java.nio.file.StandardCopyOption.REPLACE_EXISTING);
        Files.write(WORK.resolve("not-a-directory"), new byte[0]);
        System.setProperty("carddemo.test.data", DATA.toString());
        System.setProperty("carddemo.test.failwork", WORK.toString());
    }

    @Test
    void unwritableOutputFailsTheJobInsteadOfReportingSuccess() throws Exception {
        JobExecution exec = jobLauncherTestUtils.launchJob(new JobParametersBuilder()
                .addLong("run", System.nanoTime()).toJobParameters());
        assertThat(exec.getStatus()).isEqualTo(BatchStatus.FAILED);
        assertThat(exec.getExitStatus().getExitCode()).isEqualTo("FAILED");
        assertThat(exec.getStepExecutions()).extracting(s -> s.getStepName() + ":" + s.getStatus())
                .containsExactly("postDailyTransactions:COMPLETED", "closeDatasets:FAILED");
    }
}
