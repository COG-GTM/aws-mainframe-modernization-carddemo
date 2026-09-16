package com.carddemo.posting.batch;

import com.carddemo.posting.PostingApplication;
import com.carddemo.posting.domain.RejectRecordLayout;
import com.carddemo.recordio.codec.RecordEncoding;
import com.carddemo.recordio.layout.Account;
import com.carddemo.recordio.layout.AccountLayout;
import com.carddemo.recordio.layout.Transaction;
import com.carddemo.recordio.layout.TransactionLayout;
import com.carddemo.recordio.store.FixedWidthFile;
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
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Runs POSTTRAN STEP15 end to end on the shipped EBCDIC datasets (app/data/EBCDIC). The expected
 * counts below are what this Java implementation produces; they have NOT been confirmed against a
 * mainframe run of CBTRN02C (see docs/modernization/equivalence-evidence.md).
 */
@SpringBatchTest
@SpringBootTest(classes = PostingApplication.class, properties = {
        "spring.batch.job.enabled=false",
        "carddemo.posting.encoding=EBCDIC",
        "carddemo.posting.daily-transactions=${carddemo.test.data}/AWS.M2.CARDDEMO.DALYTRAN.PS",
        "carddemo.posting.card-xref=${carddemo.test.data}/AWS.M2.CARDDEMO.CARDXREF.PS",
        "carddemo.posting.account-master=${carddemo.test.work}/ACCTDATA",
        "carddemo.posting.category-balances=${carddemo.test.work}/TCATBALF",
        "carddemo.posting.transaction-master=${carddemo.test.work}/TRANSACT",
        "carddemo.posting.rejects=${carddemo.test.work}/DALYREJS"
})
class PostingJobTest {

    static final Path DATA = Path.of("../../../app/data/EBCDIC").toAbsolutePath().normalize();
    static final Path WORK = Path.of("target/posting-work").toAbsolutePath();

    @Autowired
    JobLauncherTestUtils jobLauncherTestUtils;

    @BeforeAll
    static void stageMasters() throws IOException {
        Files.createDirectories(WORK);
        Files.copy(DATA.resolve("AWS.M2.CARDDEMO.ACCTDATA.PS"), WORK.resolve("ACCTDATA"), java.nio.file.StandardCopyOption.REPLACE_EXISTING);
        Files.copy(DATA.resolve("AWS.M2.CARDDEMO.TCATBALF.PS"), WORK.resolve("TCATBALF"), java.nio.file.StandardCopyOption.REPLACE_EXISTING);
        System.setProperty("carddemo.test.data", DATA.toString());
        System.setProperty("carddemo.test.work", WORK.toString());
    }

    @Test
    void postsShippedDailyTransactionsAndWritesFixedWidthOutputs() throws Exception {
        JobExecution exec = jobLauncherTestUtils.launchJob(new JobParametersBuilder()
                .addLong("run", System.nanoTime()).toJobParameters());
        assertThat(exec.getStatus()).isEqualTo(BatchStatus.COMPLETED);

        var ctx = exec.getExecutionContext();
        long total = ctx.getLong("transactionCount");
        long rejects = ctx.getLong("rejectCount");
        assertThat(total).isEqualTo(300);

        List<Transaction> posted = FixedWidthFile.readAll(WORK.resolve("TRANSACT"), TransactionLayout.INSTANCE, RecordEncoding.EBCDIC);
        assertThat(posted).hasSize((int) (total - rejects));
        assertThat(posted).allSatisfy(t -> assertThat(t.processingTimestamp()).matches("\\d{4}-\\d{2}-\\d{2}-\\d{2}\\.\\d{2}\\.\\d{2}\\.\\d{6}"));
        assertThat(Files.size(WORK.resolve("TRANSACT")) % Transaction.LENGTH).isZero();
        assertThat(Files.size(WORK.resolve("DALYREJS"))).isEqualTo(rejects * RejectRecordLayout.LENGTH);

        List<Account> accounts = FixedWidthFile.readAll(WORK.resolve("ACCTDATA"), AccountLayout.INSTANCE, RecordEncoding.EBCDIC);
        assertThat(accounts).hasSize(50);
        assertThat(Files.size(WORK.resolve("ACCTDATA"))).isEqualTo(50L * Account.LENGTH);

        String expectedExit = rejects > 0 ? PostingJobConfiguration.COMPLETED_WITH_REJECTS : "COMPLETED";
        assertThat(exec.getExitStatus().getExitCode()).isEqualTo(expectedExit);
    }
}
