package com.carddemo.report.batch;

import com.carddemo.recordio.codec.FixedWidthRecord;
import com.carddemo.recordio.codec.RecordEncoding;
import com.carddemo.recordio.layout.Transaction;
import com.carddemo.recordio.layout.TransactionLayout;
import com.carddemo.recordio.store.FixedWidthFile;
import com.carddemo.report.ReportApplication;
import com.carddemo.report.domain.ReportDateRange;
import com.carddemo.report.domain.ReportLines;
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
 * TRANREPT on the shipped EBCDIC data. The shipped DALYTRAN has blank processing timestamps and there is no
 * shipped TRANSACT master, so a master is synthesised here by stamping every daily record with a processing
 * date inside the SORT window; DATEPARM is the shipped app/data/EBCDIC/AWS.M2.CARDDEMO.DATEPARM.PS if present,
 * otherwise a 2022-01-01..2022-07-06 record.
 */
@SpringBatchTest
@SpringBootTest(classes = ReportApplication.class, properties = {
        "spring.batch.job.enabled=false",
        "carddemo.report.encoding=EBCDIC",
        "carddemo.report.transaction-master=${carddemo.test.work}/TRANSACT",
        "carddemo.report.sort-start-date=2022-01-01",
        "carddemo.report.sort-end-date=2022-07-06",
        "carddemo.report.date-parm=${carddemo.test.work}/DATEPARM",
        "carddemo.report.card-xref=${carddemo.test.data}/AWS.M2.CARDDEMO.CARDXREF.PS",
        "carddemo.report.transaction-types=${carddemo.test.data}/AWS.M2.CARDDEMO.TRANTYPE.PS",
        "carddemo.report.transaction-categories=${carddemo.test.data}/AWS.M2.CARDDEMO.TRANCATG.PS",
        "carddemo.report.report=${carddemo.test.work}/TRANREPT",
        "carddemo.report.out-of-range-policy=STOP_LIKE_COBOL"
})
class ReportJobTest {

    static final Path DATA = Path.of("../../../app/data/EBCDIC").toAbsolutePath().normalize();
    static final Path WORK = Path.of("target/report-work").toAbsolutePath();

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
        FixedWidthRecord dateParm = FixedWidthRecord.blank(ReportDateRange.RECORD_LENGTH, enc);
        dateParm.setText(0, 21, "2022-01-01 2022-07-06");
        FixedWidthFile.write(WORK.resolve("DATEPARM"), List.of(dateParm));
        System.setProperty("carddemo.test.data", DATA.toString());
        System.setProperty("carddemo.test.work", WORK.toString());
    }

    @Test
    void producesA133ByteEbcdicReportForTheShippedTransactions() throws Exception {
        JobExecution exec = jobLauncherTestUtils.launchJob(new JobParametersBuilder().addLong("run", System.nanoTime()).toJobParameters());
        assertThat(exec.getStatus()).isEqualTo(BatchStatus.COMPLETED);

        byte[] report = Files.readAllBytes(WORK.resolve("TRANREPT"));
        assertThat(report.length % ReportLines.WIDTH).isZero();
        List<String> lines = FixedWidthFile.split(report, ReportLines.WIDTH, RecordEncoding.EBCDIC).stream()
                .map(r -> r.text(0, ReportLines.WIDTH)).toList();
        assertThat(lines.get(0)).startsWith("DALYREPT");
        assertThat(lines).filteredOn(l -> l.matches("\\d{16} \\d{11} .*")).hasSize(300);
        assertThat(lines.get(lines.size() - 1)).startsWith("Grand Total");
        assertThat(Files.exists(WORK.resolve("TRANREPT.TRANSACT.DALY"))).isTrue();
    }
}
