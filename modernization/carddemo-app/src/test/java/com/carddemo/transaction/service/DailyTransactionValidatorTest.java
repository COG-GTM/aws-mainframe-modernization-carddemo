package com.carddemo.transaction.service;

import com.carddemo.account.domain.Account;
import com.carddemo.account.repository.AccountRepository;
import com.carddemo.card.domain.CardXref;
import com.carddemo.card.repository.CardXrefRepository;
import com.carddemo.transaction.domain.DailyTransaction;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.util.Optional;

import static com.carddemo.transaction.service.DailyTransactions.CARD_OF_ACCOUNT_1;
import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * {@code CBTRN02C 1500-VALIDATE-TRAN} (cbl:370-420). Expectations derived from the COBOL, not from the implementation:
 * reason codes and descriptions are the literals of cbl:385-386, 396-397, 409-410, 416-417.
 */
class DailyTransactionValidatorTest {

    final CardXrefRepository xrefs = mock(CardXrefRepository.class);
    final AccountRepository accounts = mock(AccountRepository.class);
    final DailyTransactionValidator validator = new DailyTransactionValidator(xrefs, accounts);

    @BeforeEach
    void account1() {
        CardXref xref = new CardXref();
        xref.setCardNum(CARD_OF_ACCOUNT_1);
        xref.setAcctId(1L);
        when(xrefs.findById(CARD_OF_ACCOUNT_1)).thenReturn(Optional.of(xref));
        when(accounts.findById(1L)).thenReturn(Optional.of(account("1000.00", "300.00", "100.00", "2025-05-20")));
    }

    static Account account(String creditLimit, String cycCredit, String cycDebit, String expiration) {
        Account a = new Account();
        a.setAcctId(1L);
        a.setCreditLimit(new BigDecimal(creditLimit));
        a.setCurrCycCredit(new BigDecimal(cycCredit));
        a.setCurrCycDebit(new BigDecimal(cycDebit));
        a.setExpirationDate(expiration);
        return a;
    }

    @Test
    void PR_CBTRN02C_03_everyRecordStartsFromReasonZeroAndSpaces() {
        assertThat(validator.validate(DailyTransactions.of("T1", "0000000000000000", "1.00")).reasonCode()).isEqualTo(100);
        ValidationResult next = validator.validate(DailyTransactions.of("T2", CARD_OF_ACCOUNT_1, "1.00"));
        assertThat(next).isEqualTo(new ValidationResult(0, "", 1L));
        assertThat(next.isValid()).isTrue();
    }

    @Test
    void PR_CBTRN02C_04_unknownCardIsRejected100AndTheAccountIsNotRead() {
        when(xrefs.findById("4444333322221111")).thenReturn(Optional.empty());
        assertThat(validator.validate(DailyTransactions.of("T1", "4444333322221111", "1.00")))
                .isEqualTo(new ValidationResult(100, "INVALID CARD NUMBER FOUND", 0L));
        verify(accounts, never()).findById(anyLong());
    }

    @Test
    void PR_CBTRN02C_05_xrefAccountMissingIsRejected101() {
        when(accounts.findById(1L)).thenReturn(Optional.empty());
        assertThat(validator.validate(DailyTransactions.of("T1", CARD_OF_ACCOUNT_1, "1.00")))
                .isEqualTo(new ValidationResult(101, "ACCOUNT RECORD NOT FOUND", 1L));
    }

    @Test
    void PR_CBTRN02C_06_overLimitWhenCreditLimitBelowCycCreditMinusCycDebitPlusAmount() {
        // 300.00 - 100.00 + 800.00 = 1000.00 = limit: accepted (ACCT-CREDIT-LIMIT >= WS-TEMP-BAL)
        assertThat(validator.validate(DailyTransactions.of("T1", CARD_OF_ACCOUNT_1, "800.00")).reasonCode()).isZero();
        // one cent more: 102
        assertThat(validator.validate(DailyTransactions.of("T1", CARD_OF_ACCOUNT_1, "800.01")))
                .isEqualTo(new ValidationResult(102, "OVERLIMIT TRANSACTION", 1L));
        // a negative amount lowers the balance
        assertThat(validator.validate(DailyTransactions.of("T1", CARD_OF_ACCOUNT_1, "-5000.00")).reasonCode()).isZero();
    }

    @Test
    void PR_CBTRN02C_06_wsTempBalKeepsOnlyNineIntegerDigits() {
        // CYC-CREDIT 1,000,000,000.00: COMPUTE into S9(09)V99 without ON SIZE ERROR stores 0.00, so limit 0.00 passes
        when(accounts.findById(1L)).thenReturn(Optional.of(account("0.00", "1000000000.00", "0.00", "2025-05-20")));
        assertThat(validator.validate(DailyTransactions.of("T1", CARD_OF_ACCOUNT_1, "0.00")).reasonCode()).isZero();
    }

    @Test
    void PR_CBTRN02C_07_receivedAfterExpirationIsRejected103AsAText10Comparison() {
        assertThat(validator.validate(DailyTransactions.of("T1", CARD_OF_ACCOUNT_1, "1.00", "2025-05-20 23:59:59.999999"))
                .reasonCode()).as("same day").isZero();
        assertThat(validator.validate(DailyTransactions.of("T1", CARD_OF_ACCOUNT_1, "1.00", "2025-05-21 00:00:00.000000")))
                .isEqualTo(new ValidationResult(103, "TRANSACTION RECEIVED AFTER ACCT EXPIRATION", 1L));
    }

    @Test
    void PR_CBTRN02C_07_expirationOverwritesOverLimit() {
        DailyTransaction both = DailyTransactions.of("T1", CARD_OF_ACCOUNT_1, "999.99", "2026-01-01 00:00:00.000000");
        assertThat(validator.validate(both).reasonCode()).isEqualTo(103);
    }

    @Test
    void PR_CBTRN02C_07_blankExpirationIsSpacesAndLowerThanAnyDate() {
        when(accounts.findById(1L)).thenReturn(Optional.of(account("1000.00", "0.00", "0.00", "")));
        assertThat(validator.validate(DailyTransactions.of("T1", CARD_OF_ACCOUNT_1, "1.00")).reasonCode()).isEqualTo(103);
    }
}
