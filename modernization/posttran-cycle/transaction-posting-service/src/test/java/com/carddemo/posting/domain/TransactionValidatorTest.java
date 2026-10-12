package com.carddemo.posting.domain;

import com.carddemo.posting.Fixtures;
import com.carddemo.recordio.layout.Account;
import com.carddemo.recordio.layout.Transaction;
import com.carddemo.recordio.store.KeyedRecordStore;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.util.Optional;

import static com.carddemo.posting.Fixtures.*;
import static org.assertj.core.api.Assertions.assertThat;

/** CBTRN02C 1500-VALIDATE-TRAN rules, app/cbl/CBTRN02C.cbl lines 371-421. */
class TransactionValidatorTest {

    private final KeyedRecordStore<Account> accounts = Fixtures.accounts();

    private Optional<RejectReason> validate(Transaction t) {
        return TransactionValidator.validate(t, Fixtures.xref().read(t.cardNumber()), accounts::read);
    }

    @Test
    void unknownCardIsReason100AndAccountIsNeverConsulted() {
        assertThat(validate(daily("T1", CARD_UNKNOWN, "1.00", "2022-06-10"))).contains(RejectReason.INVALID_CARD);
    }

    @Test
    void cardWithoutAccountIsReason101() {
        assertThat(validate(daily("T1", CARD_NO_ACCOUNT, "1.00", "2022-06-10"))).contains(RejectReason.ACCOUNT_NOT_FOUND);
    }

    @Test
    void amountExactlyAtCreditLimitPosts_oneCentOverIsReason102() {
        // limit 1000.00 >= 600.00 - 100.00 + 500.00 -> allowed
        assertThat(validate(daily("T1", CARD_OK, "500.00", "2022-06-10"))).isEmpty();
        assertThat(validate(daily("T2", CARD_OK, "500.01", "2022-06-10"))).contains(RejectReason.OVER_LIMIT);
    }

    @Test
    void currentBalanceIsNotPartOfTheCreditCheck() {
        Account a = account("X", "1000.00", "0.00", "0.00", "2030-01-01");
        Account deepInDebt = new Account(a.accountId(), a.activeStatus(), new BigDecimal("999999.99"), a.creditLimit(),
                a.cashCreditLimit(), a.openDate(), a.expirationDate(), a.reissueDate(), a.currentCycleCredit(),
                a.currentCycleDebit(), a.addressZip(), a.groupId());
        assertThat(TransactionValidator.exceedsCreditLimit(deepInDebt, new BigDecimal("1000.00"))).isFalse();
    }

    @Test
    void negativeDebitAlwaysPassesTheCreditCheck() {
        assertThat(validate(daily("T1", CARD_OK, "-5000.00", "2022-06-10"))).isEmpty();
    }

    @Test
    void wsTempBalIsNarrowerThanItsOperands_highOrderDigitIsLost() {
        // exposure 1,000,000,050.00 does not fit S9(09)V99; COBOL keeps 000,000,050.00 and the check passes
        Account a = account("X", "100.00", "1000000000.00", "0.00", "2030-01-01");
        assertThat(TransactionValidator.exceedsCreditLimit(a, new BigDecimal("50.00"))).isFalse();
        // whereas an exposure that does fit is rejected
        assertThat(TransactionValidator.exceedsCreditLimit(a.withBalances(a.currentBalance(), new BigDecimal("999999999.00"), BigDecimal.ZERO),
                new BigDecimal("0.99"))).isTrue();
    }

    @Test
    void transactionOnExpiryDatePosts_dayAfterIsReason103() {
        assertThat(validate(daily("T1", CARD_EXPIRED, "1.00", "2022-06-15"))).isEmpty();
        assertThat(validate(daily("T2", CARD_EXPIRED, "1.00", "2022-06-16"))).contains(RejectReason.ACCOUNT_EXPIRED);
    }

    @Test
    void expiryReasonOverwritesOverLimitReasonWhenBothFail() {
        assertThat(validate(daily("T1", CARD_EXPIRED, "5000.00", "2022-06-16"))).contains(RejectReason.ACCOUNT_EXPIRED);
    }
}
