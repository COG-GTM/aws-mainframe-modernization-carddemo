package com.carddemo.report.batch;

import com.carddemo.recordio.codec.RecordEncoding;
import com.carddemo.recordio.layout.Transaction;
import com.carddemo.recordio.layout.TransactionLayout;
import com.carddemo.recordio.store.FixedWidthFile;
import com.carddemo.report.ReportApplication;
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
 * CBTRN03C reads DATEPARM once (0550-DATEPARM-READ, lines 220-229); status '10' on that read sets
 * END-OF-FILE = 'Y' (lines 235-236) so the main loop never runs and the job ends normally with an
 * empty TRANREPT. The Java job must complete, not fail, and write a zero-record report.
 */
@SpringBatchTest
@SpringBootTest(classes = ReportApplication.class, properties = {
        "spring.batch.job.enabled=false",
        "carddemo.report.encoding=EBCDIC",
        "carddemo.report.transaction-master=${carddemo.test.emptywork}/TRANSACT",
        "carddemo.report.sort-start-date=2022-01-01",
        "carddemo.report.sort-end-date=2022-07-06",
        "carddemo.report.date-parm=${carddemo.test.emptywork}/DATEPARM",
        "carddemo.report.card-xref=${carddemo.test.data}/AWS.M2.CARDDEMO.CARDXREF.PS",
        "carddemo.report.transaction-types=${carddemo.test.data}/AWS.M2.CARDDEMO.TRANTYPE.PS",
        "carddemo.report.transaction-categories=${carddemo.test.data}/AWS.M2.CARDDEMO.TRANCATG.PS",
        "carddemo.report.report=${carddemo.test.emptywork}/TRANREPT",
        "carddemo.report.out-of-range-policy=STOP_LIKE_COBOL"
})
class ReportJobEmptyDateParmTest {

    static final Path DATA = Path.of("../../../app/data/EBCDIC").toAbsolutePath().normalize();
    static final Path WORK = Path.of("target/report-empty-work").toAbsolutePath();

    @Autowired
    JobLauncherTestUtils jobLauncherTestUtils;

    @BeforeAll
    static void stage() throws IOException {
        Files.createDirectories(WORK);
        RecordEncoding enc = RecordEncoding.EBCDIC;
        List<Transaction> daily = FixedWidthFile.readAll(DATA.resolve("AWS.M2.CARDDEMO.DALYTRAN.PS"), TransactionLayout.INSTANCE, enc);
        FixedWidthFile.write(WORK.resolve("TRANSACT"), daily.stream()
                .map(t -> t.withProcessingTimestamp("2022-07-01-10.00.00.000000"))
                .map(t -> TransactionLayout.INSTANCE.encode(t, enc)).toList());
        Files.write(WORK.resolve("DATEPARM"), new byte[0]);
        Files.write(WORK.resolve("TRANREPT"), new byte[133]);
        System.setProperty("carddemo.test.data", DATA.toString());
        System.setProperty("carddemo.test.emptywork", WORK.toString());
    }

    @Test
    void emptyDateParmCompletesWithAnEmptyReport() throws Exception {
        JobExecution exec = jobLauncherTestUtils.launchJob(new JobParametersBuilder().addLong("run", System.nanoTime()).toJobParameters());
        assertThat(exec.getStatus()).isEqualTo(BatchStatus.COMPLETED);
        assertThat(Files.size(WORK.resolve("TRANREPT"))).isZero();
    }
}
