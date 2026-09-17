package com.carddemo.account.service;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.account.domain.Account;
import com.carddemo.account.repository.AccountRepository;
import java.math.BigDecimal;
import java.time.LocalDate;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

/** Parity tests for the CBTRN02C and CBACT04C account paragraphs. */
@SpringBootTest
class AccountPostingTest {

    private static final long ACCT_ID = 99999999999L;
    private static final LocalDate TRAN_DATE = LocalDate.of(2024, 5, 1);

    @Autowired
    private AccountService accountService;

    @Autowired
    private AccountRepository accounts;

    @BeforeEach
    void setUp() {
        accounts.deleteAll();
        Account account = new Account(ACCT_ID);
        account.setCurrBal(new BigDecimal("100.00"));
        account.setCreditLimit(new BigDecimal("500.00"));
        account.setCashCreditLimit(new BigDecimal("100.00"));
        account.setOpenDate(LocalDate.of(2020, 1, 1));
        account.setExpirationDate(LocalDate.of(2030, 1, 1));
        accounts.save(account);
    }

    @Test
    void postsCreditAmountToBalanceAndCycleCredit() {
        PostingOutcome outcome = accountService.post(ACCT_ID, new BigDecimal("50.00"), TRAN_DATE);

        assertThat(outcome.posted()).isTrue();
        assertThat(outcome.currentBalance()).isEqualByComparingTo("150.00");
        assertThat(outcome.currentCycleCredit()).isEqualByComparingTo("50.00");
        assertThat(outcome.currentCycleDebit()).isEqualByComparingTo("0.00");
    }

    @Test
    void addsNegativeAmountToCycleDebitJustLikeParagraph2800() {
        PostingOutcome outcome = accountService.post(ACCT_ID, new BigDecimal("-20.00"), TRAN_DATE);

        assertThat(outcome.posted()).isTrue();
        assertThat(outcome.currentBalance()).isEqualByComparingTo("80.00");
        assertThat(outcome.currentCycleDebit()).isEqualByComparingTo("-20.00");
    }

    @Test
    void rejectsOverlimitWithReason102() {
        PostingOutcome outcome = accountService.post(ACCT_ID, new BigDecimal("500.01"), TRAN_DATE);

        assertThat(outcome.posted()).isFalse();
        assertThat(outcome.reasonCode()).isEqualTo(102);
        assertThat(outcome.reasonDescription()).isEqualTo("OVERLIMIT TRANSACTION");
        assertThat(accounts.findById(ACCT_ID).orElseThrow().getCurrBal()).isEqualByComparingTo("100.00");
    }

    @Test
    void rejectsTransactionAfterAccountExpirationWithReason103() {
        PostingOutcome outcome = accountService.post(ACCT_ID, new BigDecimal("10.00"),
                LocalDate.of(2031, 1, 1));

        assertThat(outcome.posted()).isFalse();
        assertThat(outcome.reasonCode()).isEqualTo(103);
    }

    @Test
    void rejectsUnknownAccountWithReason101() {
        PostingOutcome outcome = accountService.post(12345678901L, new BigDecimal("10.00"), TRAN_DATE);

        assertThat(outcome.posted()).isFalse();
        assertThat(outcome.reasonCode()).isEqualTo(101);
    }

    @Test
    void interestSettlementAddsInterestAndResetsTheCycle() {
        accountService.post(ACCT_ID, new BigDecimal("50.00"), TRAN_DATE);

        Account settled = accountService.settleInterest(ACCT_ID, new BigDecimal("12.34"));

        assertThat(settled.getCurrBal()).isEqualByComparingTo("162.34");
        assertThat(settled.getCurrCycCredit()).isEqualByComparingTo("0.00");
        assertThat(settled.getCurrCycDebit()).isEqualByComparingTo("0.00");
    }
}
