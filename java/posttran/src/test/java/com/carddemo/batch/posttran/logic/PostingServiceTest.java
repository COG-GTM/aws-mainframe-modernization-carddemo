package com.carddemo.batch.posttran.logic;

import static com.carddemo.batch.posttran.TestData.dalytran;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.carddemo.batch.posttran.TestClockConfig;
import com.carddemo.batch.posttran.cobol.Abend;
import com.carddemo.batch.posttran.domain.Account;
import com.carddemo.batch.posttran.domain.CardXref;
import com.carddemo.batch.posttran.domain.TranCatBalance;
import com.carddemo.batch.posttran.domain.TranCatBalanceKey;
import com.carddemo.batch.posttran.domain.TransactionRecord;
import com.carddemo.batch.posttran.record.DailyTransaction;
import jakarta.persistence.EntityManager;
import java.math.BigDecimal;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Import;
import org.springframework.transaction.annotation.Transactional;

@SpringBootTest(properties = {"posttran.run-on-startup=false", "spring.datasource.url=jdbc:h2:mem:posting-test"})
@Import(TestClockConfig.class)
@Transactional
class PostingServiceTest {

    private static final String CARD = "4111111111111111";
    private static final String ORPHAN_CARD = "4222222222222222";
    private static final String UNKNOWN_CARD = "4999999999999999";
    private static final long ACCT = 12345678901L;
    private static final String TS = "2026-06-01 10:11:12.000000";

    @Autowired
    private PostingService service;
    @Autowired
    private EntityManager em;

    private Account account;

    @BeforeEach
    void seed() {
        service.resetWorkingStorage();
        account = new Account(ACCT);
        account.setCurrBal(new BigDecimal("100.00"));
        account.setCreditLimit(new BigDecimal("1000.00"));
        account.setCurrCycCredit(new BigDecimal("300.00"));
        account.setCurrCycDebit(new BigDecimal("50.00"));
        account.setExpirationDate("2026-12-31");
        em.persist(account);
        em.persist(new CardXref(CARD, 1, ACCT));
        em.persist(new CardXref(ORPHAN_CARD, 2, 99999999999L));
        em.flush();
    }

    @Test
    void reason100WhenCardNotInXref() {
        ValidationResult r = service.validate(dalytran("T1", "01", 1, "10.00", UNKNOWN_CARD, TS));
        assertThat(r.reasonCode()).isEqualTo(100);
        assertThat(r.description()).isEqualTo("INVALID CARD NUMBER FOUND");
    }

    @Test
    void reason101WhenXrefPointsAtMissingAccount() {
        ValidationResult r = service.validate(dalytran("T1", "01", 1, "10.00", ORPHAN_CARD, TS));
        assertThat(r.reasonCode()).isEqualTo(101);
        assertThat(r.description()).isEqualTo("ACCOUNT RECORD NOT FOUND");
    }

    @Test
    void reason102WhenCycleBalancePlusAmountExceedsLimit() {
        // 300 - 50 + 750.01 = 1000.01 > 1000.00
        ValidationResult r = service.validate(dalytran("T1", "01", 1, "750.01", CARD, TS));
        assertThat(r.reasonCode()).isEqualTo(102);
        assertThat(r.description()).isEqualTo("OVERLIMIT TRANSACTION");
    }

    @Test
    void exactlyAtLimitIsAccepted() {
        assertThat(service.validate(dalytran("T1", "01", 1, "750.00", CARD, TS)).valid()).isTrue();
    }

    @Test
    void reason103WhenReceivedAfterExpiration() {
        ValidationResult r = service.validate(dalytran("T1", "01", 1, "10.00", CARD, "2027-01-01 00:00:00.000000"));
        assertThat(r.reasonCode()).isEqualTo(103);
        assertThat(r.description()).isEqualTo("TRANSACTION RECEIVED AFTER ACCT EXPIRATION");
    }

    @Test
    void expirationDayItselfIsAccepted() {
        assertThat(service.validate(dalytran("T1", "01", 1, "10.00", CARD, "2026-12-31 23:59:59.000000")).valid())
                .isTrue();
    }

    @Test
    void reason103OverwritesReason102() {
        ValidationResult r = service.validate(dalytran("T1", "01", 1, "5000.00", CARD, "2027-01-01 00:00:00.000000"));
        assertThat(r.reasonCode()).isEqualTo(103);
    }

    @Test
    void tcatbalCreatedWhenMissing() {
        DailyTransaction tran = dalytran("T1", "01", 5, "25.50", CARD, TS);
        TranCatBalanceKey key = new TranCatBalanceKey(ACCT, "01", 5);
        assertThat(em.find(TranCatBalance.class, key)).isNull();

        service.post(tran, service.validate(tran));
        em.flush();
        em.clear();

        TranCatBalance created = em.find(TranCatBalance.class, key);
        assertThat(created.getBalance()).isEqualByComparingTo("25.50");
        assertThat(created.toRecord()).hasSize(50).startsWith("123456789010100050000000255{");
    }

    @Test
    void tcatbalUpdatedWhenPresent() {
        TranCatBalanceKey key = new TranCatBalanceKey(ACCT, "01", 5);
        em.persist(new TranCatBalance(key, new BigDecimal("100.00"), " ".repeat(22)));
        DailyTransaction tran = dalytran("T1", "01", 5, "-30.25", CARD, TS);

        service.post(tran, service.validate(tran));
        em.flush();
        em.clear();

        assertThat(em.find(TranCatBalance.class, key).getBalance()).isEqualByComparingTo("69.75");
        assertThat(em.createQuery("select count(t) from TranCatBalance t", Long.class).getSingleResult()).isOne();
    }

    @Test
    void creditUpdatesBalanceAndCycleCredit() {
        DailyTransaction tran = dalytran("T1", "01", 1, "40.00", CARD, TS);
        service.post(tran, service.validate(tran));
        assertThat(account.getCurrBal()).isEqualByComparingTo("140.00");
        assertThat(account.getCurrCycCredit()).isEqualByComparingTo("340.00");
        assertThat(account.getCurrCycDebit()).isEqualByComparingTo("50.00");
    }

    @Test
    void zeroAmountCountsAsCredit() {
        DailyTransaction tran = dalytran("T1", "01", 1, "0.00", CARD, TS);
        service.post(tran, service.validate(tran));
        assertThat(account.getCurrCycCredit()).isEqualByComparingTo("300.00");
        assertThat(account.getCurrCycDebit()).isEqualByComparingTo("50.00");
    }

    @Test
    void debitUpdatesBalanceAndCycleDebit() {
        DailyTransaction tran = dalytran("T1", "02", 1, "-20.00", CARD, TS);
        service.post(tran, service.validate(tran));
        assertThat(account.getCurrBal()).isEqualByComparingTo("80.00");
        assertThat(account.getCurrCycCredit()).isEqualByComparingTo("300.00");
        assertThat(account.getCurrCycDebit()).isEqualByComparingTo("30.00");
    }

    @Test
    void postedTransactionCopiesAllFieldsAndStampsDb2Timestamp() {
        DailyTransaction tran = dalytran("T1", "01", 7, "12.34", CARD, TS);
        service.post(tran, service.validate(tran));
        em.flush();
        em.clear();

        String posted = em.find(TransactionRecord.class, tran.id()).toRecord();
        assertThat(posted.substring(0, 304)).isEqualTo(tran.raw().substring(0, 304));
        assertThat(posted.substring(304, 330)).isEqualTo("2026-10-14-09.08.07.120000");
        assertThat(posted.substring(330)).isEqualTo(tran.raw().substring(330));
    }

    @Test
    void duplicateTranIdAbendsLikeWriteInvalidKey() {
        DailyTransaction tran = dalytran("T1", "01", 1, "1.00", CARD, TS);
        service.post(tran, service.validate(tran));
        assertThatThrownBy(() -> service.post(tran, service.validate(tran)))
                .isInstanceOf(Abend.class)
                .hasMessage("ERROR WRITING TO TRANSACTION FILE")
                .extracting(e -> ((Abend) e).fileStatus()).isEqualTo("22");
    }
}
