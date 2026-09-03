package com.carddemo.interest.batch;

import com.carddemo.interest.InterestApplication;
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
import java.nio.file.StandardCopyOption;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

/** INTCALC STEP15 on the shipped EBCDIC datasets. Counts are Java results, not mainframe-confirmed. */
@SpringBatchTest
@SpringBootTest(classes = InterestApplication.class, properties = {
        "spring.batch.job.enabled=false",
        "carddemo.interest.encoding=EBCDIC",
        "carddemo.interest.run-date=2022071800",
        "carddemo.interest.category-balances=${carddemo.test.data}/AWS.M2.CARDDEMO.TCATBALF.PS",
        "carddemo.interest.card-xref=${carddemo.test.data}/AWS.M2.CARDDEMO.CARDXREF.PS",
        "carddemo.interest.account-master=${carddemo.test.work}/ACCTDATA",
        "carddemo.interest.disclosure-groups=${carddemo.test.data}/AWS.M2.CARDDEMO.DISCGRP.PS",
        "carddemo.interest.system-transactions=${carddemo.test.work}/SYSTRAN"
})
class InterestJobTest {

    static final Path DATA = Path.of("../../../app/data/EBCDIC").toAbsolutePath().normalize();
    static final Path WORK = Path.of("target/interest-work").toAbsolutePath();

    @Autowired
    JobLauncherTestUtils jobLauncherTestUtils;

    @BeforeAll
    static void stage() throws IOException {
        Files.createDirectories(WORK);
        Files.copy(DATA.resolve("AWS.M2.CARDDEMO.ACCTDATA.PS"), WORK.resolve("ACCTDATA"), StandardCopyOption.REPLACE_EXISTING);
        System.setProperty("carddemo.test.data", DATA.toString());
        System.setProperty("carddemo.test.work", WORK.toString());
    }

    @Test
    void generatesOneInterestTransactionPerCategoryBalanceAndRewritesAccounts() throws Exception {
        JobExecution exec = jobLauncherTestUtils.launchJob(new JobParametersBuilder().addLong("run", System.nanoTime()).toJobParameters());
        assertThat(exec.getStatus()).isEqualTo(BatchStatus.COMPLETED);

        List<Transaction> systran = FixedWidthFile.readAll(WORK.resolve("SYSTRAN"), TransactionLayout.INSTANCE, RecordEncoding.EBCDIC);
        assertThat(systran).hasSize(50);
        assertThat(systran).allSatisfy(t -> {
            assertThat(t.id()).startsWith("2022071800");
            assertThat(t.typeCode()).isEqualTo("01");
            assertThat(t.categoryCode()).isEqualTo(5);
            assertThat(t.source().trim()).isEqualTo("System");
        });
        List<Account> accounts = FixedWidthFile.readAll(WORK.resolve("ACCTDATA"), AccountLayout.INSTANCE, RecordEncoding.EBCDIC);
        assertThat(accounts).hasSize(50);
        assertThat(accounts).allSatisfy(a -> {
            assertThat(a.currentCycleCredit()).isZero();
            assertThat(a.currentCycleDebit()).isZero();
        });
    }
}
