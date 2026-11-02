package com.carddemo.account.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.carddemo.account.domain.Account;
import com.carddemo.account.repository.AccountPostingRepository;
import com.carddemo.account.repository.AccountRepository;
import com.carddemo.common.error.DuplicateKeyException;
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

    @Autowired
    private AccountPostingRepository postings;

    @BeforeEach
    void setUp() {
        postings.deleteAll();
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
        PostingOutcome outcome = accountService.post(ACCT_ID, "T000000000000001", new BigDecimal("50.00"), TRAN_DATE);

        assertThat(outcome.posted()).isTrue();
        assertThat(outcome.currentBalance()).isEqualByComparingTo("150.00");
        assertThat(outcome.currentCycleCredit()).isEqualByComparingTo("50.00");
        assertThat(outcome.currentCycleDebit()).isEqualByComparingTo("0.00");
    }

    @Test
    void addsNegativeAmountToCycleDebitJustLikeParagraph2800() {
        PostingOutcome outcome = accountService.post(ACCT_ID, "T000000000000002", new BigDecimal("-20.00"), TRAN_DATE);

        assertThat(outcome.posted()).isTrue();
        assertThat(outcome.currentBalance()).isEqualByComparingTo("80.00");
        assertThat(outcome.currentCycleDebit()).isEqualByComparingTo("-20.00");
    }

    @Test
    void rejectsOverlimitWithReason102() {
        PostingOutcome outcome = accountService.post(ACCT_ID, "T000000000000003", new BigDecimal("500.01"), TRAN_DATE);

        assertThat(outcome.posted()).isFalse();
        assertThat(outcome.reasonCode()).isEqualTo(102);
        assertThat(outcome.reasonDescription()).isEqualTo("OVERLIMIT TRANSACTION");
        assertThat(accounts.findById(ACCT_ID).orElseThrow().getCurrBal()).isEqualByComparingTo("100.00");
    }

    @Test
    void rejectsTransactionAfterAccountExpirationWithReason103() {
        PostingOutcome outcome = accountService.post(ACCT_ID, "T000000000000004", new BigDecimal("10.00"),
                LocalDate.of(2031, 1, 1));

        assertThat(outcome.posted()).isFalse();
        assertThat(outcome.reasonCode()).isEqualTo(103);
    }

    @Test
    void rejectsUnknownAccountWithReason101() {
        PostingOutcome outcome = accountService.post(12345678901L, "T000000000000005", new BigDecimal("10.00"), TRAN_DATE);

        assertThat(outcome.posted()).isFalse();
        assertThat(outcome.reasonCode()).isEqualTo(101);
    }

    @Test
    void interestSettlementAddsInterestAndResetsTheCycle() {
        accountService.post(ACCT_ID, "T000000000000006", new BigDecimal("50.00"), TRAN_DATE);

        Account settled = accountService.settleInterest(ACCT_ID, new BigDecimal("12.34"));

        assertThat(settled.getCurrBal()).isEqualByComparingTo("162.34");
        assertThat(settled.getCurrCycCredit()).isEqualByComparingTo("0.00");
        assertThat(settled.getCurrCycDebit()).isEqualByComparingTo("0.00");
    }

    @Test
    void overlimitAndExpiredRejectsWith103BecauseCbtrn02cChecksExpirationLast() {
        PostingOutcome outcome = accountService.post(ACCT_ID, "0000000000000201", new BigDecimal("900.00"),
                LocalDate.of(2031, 1, 1));

        assertThat(outcome.posted()).isFalse();
        assertThat(outcome.reasonCode()).isEqualTo(103);
        assertThat(outcome.reasonDescription()).isEqualTo("TRANSACTION RECEIVED AFTER ACCT EXPIRATION");
    }

    @Test
    void blankExpirationDateRejectsWith103BecauseSpacesCompareLowerThanAnyDate() {
        Account account = accounts.findById(ACCT_ID).orElseThrow();
        account.setExpirationDate(null);
        accounts.save(account);

        PostingOutcome outcome = accountService.post(ACCT_ID, "0000000000000202", new BigDecimal("10.00"), TRAN_DATE);

        assertThat(outcome.reasonCode()).isEqualTo(103);
    }

    @Test
    void transactionOnTheExpirationDateIsPosted() {
        PostingOutcome outcome = accountService.post(ACCT_ID, "0000000000000203", new BigDecimal("10.00"),
                LocalDate.of(2030, 1, 1));

        assertThat(outcome.posted()).isTrue();
    }

    @Test
    void creditLimitIsComparedWithCycleCreditMinusCycleDebitPlusAmount() {
        Account account = accounts.findById(ACCT_ID).orElseThrow();
        account.setCurrCycCredit(new BigDecimal("300.00"));
        account.setCurrCycDebit(new BigDecimal("-100.00"));
        accounts.save(account);

        // 300.00 - (-100.00) + 100.00 = 500.00 is not over the 500.00 limit; one cent more is.
        assertThat(accountService.post(ACCT_ID, "0000000000000204", new BigDecimal("100.00"), TRAN_DATE).posted())
                .isTrue();
        assertThat(accountService.post(ACCT_ID, "0000000000000205", new BigDecimal("0.01"), TRAN_DATE).reasonCode())
                .isEqualTo(102);
    }

    @Test
    void retryWithTheSameTransactionIdAfterACommittedPostingDoesNotPostTwice() {
        PostingOutcome first = accountService.post(ACCT_ID, "0000000000000301", new BigDecimal("50.00"), TRAN_DATE);
        PostingOutcome retry = accountService.post(ACCT_ID, "0000000000000301", new BigDecimal("50.00"), TRAN_DATE);

        assertThat(retry).isEqualTo(first);
        Account account = accounts.findById(ACCT_ID).orElseThrow();
        assertThat(account.getCurrBal()).isEqualByComparingTo("150.00");
        assertThat(account.getCurrCycCredit()).isEqualByComparingTo("50.00");
        assertThat(postings.count()).isEqualTo(1);
    }

    @Test
    void retryReturnsTheRecordedFiguresEvenAfterLaterPostings() {
        PostingOutcome first = accountService.post(ACCT_ID, "0000000000000302", new BigDecimal("50.00"), TRAN_DATE);
        accountService.post(ACCT_ID, "0000000000000303", new BigDecimal("-20.00"), TRAN_DATE);

        PostingOutcome retry = accountService.post(ACCT_ID, "0000000000000302", new BigDecimal("50.00"), TRAN_DATE);

        assertThat(retry).isEqualTo(first);
        assertThat(accounts.findById(ACCT_ID).orElseThrow().getCurrBal()).isEqualByComparingTo("130.00");
    }

    @Test
    void sameTransactionIdWithADifferentAmountIsAConflict() {
        accountService.post(ACCT_ID, "0000000000000304", new BigDecimal("50.00"), TRAN_DATE);

        assertThatThrownBy(() -> accountService.post(ACCT_ID, "0000000000000304", new BigDecimal("60.00"), TRAN_DATE))
                .isInstanceOf(DuplicateKeyException.class);
        assertThat(accounts.findById(ACCT_ID).orElseThrow().getCurrBal()).isEqualByComparingTo("150.00");
    }

    @Test
    void rejectedPostingIsNotRecordedSoTheIdCanBeRetried() {
        accountService.post(ACCT_ID, "0000000000000305", new BigDecimal("900.00"), TRAN_DATE);

        assertThat(postings.count()).isZero();
    }
}
