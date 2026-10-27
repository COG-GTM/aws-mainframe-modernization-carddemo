package com.carddemo.transaction.batch;

import com.carddemo.transaction.batch.posting.TransactionPostingJobConfig;
import com.carddemo.transaction.batch.validation.DailyTransactionValidationJobConfig;
import com.carddemo.transaction.cobol.CobolFileException;
import com.carddemo.transaction.domain.Account;
import com.carddemo.transaction.domain.CardXref;
import com.carddemo.transaction.domain.Transaction;
import com.carddemo.transaction.domain.TransactionCategoryBalanceId;
import com.carddemo.transaction.support.TestData;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;
import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.StepExecution;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.system.CapturedOutput;
import org.springframework.boot.test.system.OutputCaptureExtension;
import org.springframework.context.annotation.Import;

import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static com.carddemo.transaction.support.TestData.dalytran;
import static org.assertj.core.api.Assertions.assertThat;

@SpringBootTest
@Import(TestData.class)
@ExtendWith(OutputCaptureExtension.class)
class BatchJobsTest {

    @Autowired
    TestData.Fixtures data;
    @Autowired
    @Qualifier(DailyTransactionValidationJobConfig.JOB_NAME)
    Job validationJob;
    @Autowired
    @Qualifier(TransactionPostingJobConfig.JOB_NAME)
    Job postingJob;

    @TempDir
    Path tmp;

    private Account acct1;
    private String card1;

    @BeforeEach
    void reseed() {
        data.reseed();
        acct1 = data.accounts().findById(1L).orElseThrow();
        card1 = data.xrefs().findFirstByAcctIdOrderByCardNumAsc(1L).orElseThrow().getCardNum();
    }

    private static StepExecution step(JobExecution e) {
        return e.getStepExecutions().iterator().next();
    }

    private static List<String> rejects(JobExecution e) throws Exception {
        Path dir = Path.of("target/test-dalyrejs");
        String name = "DALYREJS.G" + String.format("%04d", e.getId()) + "V00.txt";
        return Files.readAllLines(dir.resolve(name));
    }

    // ---------------------------------------------------------------- CBTRN01C

    @Test
    void validationJobVerifiesEverySampleDalytranRecordAgainstXrefAndAccount() throws Exception {
        JobExecution e = data.run(validationJob, TestData.SAMPLE_DALYTRAN);
        assertThat(e.getStatus()).isEqualTo(BatchStatus.COMPLETED);
        assertThat(step(e).getReadCount()).isEqualTo(300);
        assertThat(step(e).getExecutionContext().getLong("VERIFIED")).isEqualTo(300);
    }

    @Test
    void validationJobLogsCardNumberCouldNotBeVerifiedWhenXrefLookupFails(CapturedOutput out) throws Exception {
        Path f = TestData.writeDalytran(tmp, List.of(
                dalytran("9000000000000001", "01", 1, new BigDecimal("1.00"), "9999999999999999", "2022-06-10 19:27:53.000000")));
        JobExecution e = data.run(validationJob, f);
        assertThat(e.getStatus()).isEqualTo(BatchStatus.COMPLETED);
        assertThat(step(e).getExecutionContext().getLong("CARD_NOT_VERIFIED")).isEqualTo(1);
        assertThat(out).contains("CARD NUMBER 9999999999999999 COULD NOT BE VERIFIED. SKIPPING TRANSACTION ID-9000000000000001");
    }

    @Test
    void validationJobLogsAccountNotFoundWhenXrefPointsToAMissingAccount(CapturedOutput out) throws Exception {
        orphanXref("1111222233334444", 99999999999L);
        Path f = TestData.writeDalytran(tmp, List.of(
                dalytran("9000000000000002", "01", 1, new BigDecimal("1.00"), "1111222233334444", "2022-06-10 19:27:53.000000")));
        JobExecution e = data.run(validationJob, f);
        assertThat(step(e).getExecutionContext().getLong("ACCOUNT_NOT_FOUND")).isEqualTo(1);
        assertThat(out).contains("ACCOUNT 99999999999 NOT FOUND");
    }

    @Test
    void missingDalytranFileAbendsTheJobWithFileStatus35() throws Exception {
        JobExecution e = data.run(validationJob, tmp.resolve("does-not-exist.txt"));
        assertThat(e.getStatus()).isEqualTo(BatchStatus.FAILED);
        assertThat(JclReturnCodes.returnCode(e)).isEqualTo(12);
        CobolFileException abend = cobolAbend(e);
        assertThat(abend.getFileStatus()).isEqualTo("35");
        assertThat(abend.getDdName()).isEqualTo("DALYTRAN");
    }

    @Test
    void dalytranRecordLongerThan350BytesAbendsTheJob() throws Exception {
        Path f = TestData.writeDalytran(tmp, List.of("X".repeat(351)));
        JobExecution e = data.run(postingJob, f);
        assertThat(e.getStatus()).isEqualTo(BatchStatus.FAILED);
        assertThat(e.getAllFailureExceptions()).anySatisfy(ex -> assertThat(ex).isInstanceOf(CobolFileException.class));
    }

    // ---------------------------------------------------------------- CBTRN02C

    @Test
    void postingJobOnSampleDataPostsValidTransactionsRejectsTheRestAndEndsWithReturnCode4() throws Exception {
        JobExecution e = data.run(postingJob, TestData.SAMPLE_DALYTRAN);
        StepExecution s = step(e);
        assertThat(e.getStatus()).isEqualTo(BatchStatus.COMPLETED);
        assertThat(s.getReadCount()).isEqualTo(300);
        assertThat(s.getWriteCount()).isEqualTo(38);
        assertThat(data.transactions().count()).isEqualTo(262);
        assertThat(e.getExitStatus().getExitCode()).isEqualTo(JclReturnCodes.COMPLETED_WITH_REJECTS);
        assertThat(JclReturnCodes.returnCode(e)).isEqualTo(4);
        assertThat(rejects(e)).hasSize(38).allSatisfy(line -> {
            assertThat(line).hasSize(430);
            assertThat(line.substring(350, 354)).isIn("0100", "0101", "0102", "0103");
        });
    }

    @Test
    void validTransactionIsPostedToTransactTcatbalAndAccountWithDb2ProcessTimestamp() throws Exception {
        Path f = TestData.writeDalytran(tmp, List.of(
                dalytran("9000000000000010", "01", 1, new BigDecimal("100.25"), card1, "2024-01-15 10:00:00.000000")));
        JobExecution e = data.run(postingJob, f);
        assertThat(JclReturnCodes.returnCode(e)).isZero();

        Transaction t = data.transactions().findById("9000000000000010").orElseThrow();
        assertThat(t.getAmt()).isEqualByComparingTo("100.25");
        assertThat(t.getCardNum()).isEqualTo(card1);
        assertThat(t.getOrigTs()).isEqualTo("2024-01-15 10:00:00.000000");
        assertThat(t.getProcTs()).isEqualTo("2026-10-27-10.15.30.120000");
        assertThat(t.getDesc()).hasSize(100);

        Account a = data.accounts().findById(1L).orElseThrow();
        assertThat(a.getCurrBal()).isEqualByComparingTo(acct1.getCurrBal().add(new BigDecimal("100.25")));
        assertThat(a.getCurrCycCredit()).isEqualByComparingTo(acct1.getCurrCycCredit().add(new BigDecimal("100.25")));
        assertThat(a.getCurrCycDebit()).isEqualByComparingTo(acct1.getCurrCycDebit());

        BigDecimal bal = data.tcatbals().findById(new TransactionCategoryBalanceId(1L, "01", 1)).orElseThrow().getBalance();
        assertThat(bal).isEqualByComparingTo("100.25");
        assertThat(rejects(e)).isEmpty();
    }

    @Test
    void negativeAmountIsAddedToCurrentCycleDebit() throws Exception {
        Path f = TestData.writeDalytran(tmp, List.of(
                dalytran("9000000000000011", "02", 1, new BigDecimal("-40.10"), card1, "2024-01-15 10:00:00.000000")));
        data.run(postingJob, f);
        Account a = data.accounts().findById(1L).orElseThrow();
        assertThat(a.getCurrBal()).isEqualByComparingTo(acct1.getCurrBal().subtract(new BigDecimal("40.10")));
        assertThat(a.getCurrCycDebit()).isEqualByComparingTo(acct1.getCurrCycDebit().subtract(new BigDecimal("40.10")));
        assertThat(a.getCurrCycCredit()).isEqualByComparingTo(acct1.getCurrCycCredit());
    }

    @Test
    void missingTcatbalRecordIsCreatedAndExistingOneIsAccumulated(CapturedOutput out) throws Exception {
        TransactionCategoryBalanceId key = new TransactionCategoryBalanceId(1L, "07", 9);
        assertThat(data.tcatbals().findById(key)).isEmpty();
        Path f = TestData.writeDalytran(tmp, List.of(
                dalytran("9000000000000012", "07", 9, new BigDecimal("10.00"), card1, "2024-01-15 10:00:00.000000"),
                dalytran("9000000000000013", "07", 9, new BigDecimal("2.50"), card1, "2024-01-15 10:00:00.000000")));
        data.run(postingJob, f);
        assertThat(data.tcatbals().findById(key).orElseThrow().getBalance()).isEqualByComparingTo("12.50");
    }

    @Test
    void unknownCardNumberIsRejectedWithReason100() throws Exception {
        Path f = TestData.writeDalytran(tmp, List.of(
                dalytran("9000000000000020", "01", 1, new BigDecimal("1.00"), "9999999999999999", "2024-01-15 10:00:00.000000")));
        JobExecution e = data.run(postingJob, f);
        assertThat(rejects(e)).singleElement().satisfies(r ->
                assertThat(r.substring(350).stripTrailing()).isEqualTo("0100INVALID CARD NUMBER FOUND"));
        assertThat(data.transactions().count()).isZero();
        assertThat(JclReturnCodes.returnCode(e)).isEqualTo(4);
    }

    @Test
    void xrefWithoutAccountIsRejectedWithReason101() throws Exception {
        orphanXref("1111222233334444", 99999999999L);
        Path f = TestData.writeDalytran(tmp, List.of(
                dalytran("9000000000000021", "01", 1, new BigDecimal("1.00"), "1111222233334444", "2024-01-15 10:00:00.000000")));
        JobExecution e = data.run(postingJob, f);
        assertThat(rejects(e)).singleElement().satisfies(r ->
                assertThat(r.substring(350).stripTrailing()).isEqualTo("0101ACCOUNT RECORD NOT FOUND"));
    }

    @Test
    void transactionPushingCycleBalanceAboveCreditLimitIsRejectedWithReason102() throws Exception {
        BigDecimal overLimit = acct1.getCreditLimit().subtract(acct1.getCurrCycCredit()).add(acct1.getCurrCycDebit())
                .add(new BigDecimal("0.01"));
        BigDecimal atLimit = overLimit.subtract(new BigDecimal("0.01"));
        Path f = TestData.writeDalytran(tmp, List.of(
                dalytran("9000000000000022", "01", 1, overLimit, card1, "2024-01-15 10:00:00.000000"),
                dalytran("9000000000000023", "01", 1, atLimit, card1, "2024-01-15 10:00:00.000000")));
        JobExecution e = data.run(postingJob, f);
        assertThat(rejects(e)).singleElement().satisfies(r -> {
            assertThat(r).startsWith("9000000000000022");
            assertThat(r.substring(350).stripTrailing()).isEqualTo("0102OVERLIMIT TRANSACTION");
        });
        assertThat(data.transactions().existsById("9000000000000023")).isTrue();
    }

    @Test
    void transactionDatedAfterAccountExpirationIsRejectedWithReason103AndWinsOverOverlimit() throws Exception {
        String exp = acct1.getExpirationDate();                                   // 2025-05-20
        Path f = TestData.writeDalytran(tmp, List.of(
                dalytran("9000000000000024", "01", 1, new BigDecimal("1.00"), card1, "2025-05-21 00:00:00.000000"),
                dalytran("9000000000000025", "01", 1, new BigDecimal("1.00"), card1, exp + " 23:59:59.000000"),
                dalytran("9000000000000026", "01", 1, new BigDecimal("999999.00"), card1, "2025-05-21 00:00:00.000000")));
        JobExecution e = data.run(postingJob, f);
        assertThat(rejects(e)).hasSize(2).allSatisfy(r ->
                assertThat(r.substring(350).stripTrailing()).isEqualTo("0103TRANSACTION RECEIVED AFTER ACCT EXPIRATION"));
        assertThat(data.transactions().existsById("9000000000000025")).isTrue();
    }

    @Test
    void duplicateTranIdAbendsThePostingJobWithFileStatus22() throws Exception {
        String rec = dalytran("9000000000000030", "01", 1, new BigDecimal("1.00"), card1, "2024-01-15 10:00:00.000000");
        JobExecution e = data.run(postingJob, TestData.writeDalytran(tmp, List.of(rec, rec)));
        assertThat(e.getStatus()).isEqualTo(BatchStatus.FAILED);
        assertThat(JclReturnCodes.returnCode(e)).isEqualTo(12);
        assertThat(e.getAllFailureExceptions()).anySatisfy(ex ->
                assertThat(((CobolFileException) ex).getFileStatus()).isEqualTo("22"));
    }

    private static CobolFileException cobolAbend(JobExecution e) {
        for (Throwable t : e.getAllFailureExceptions()) {
            for (Throwable c = t; c != null; c = c.getCause()) {
                if (c instanceof CobolFileException cfe) {
                    return cfe;
                }
            }
        }
        throw new AssertionError("no CobolFileException in " + e.getAllFailureExceptions());
    }

    private void orphanXref(String card, long acctId) {
        CardXref x = new CardXref();
        x.setCardNum(card);
        x.setCustId(1);
        x.setAcctId(acctId);
        data.xrefs().save(x);
    }
}
