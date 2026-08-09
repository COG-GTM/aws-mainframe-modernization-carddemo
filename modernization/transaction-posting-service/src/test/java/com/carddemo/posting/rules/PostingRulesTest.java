package com.carddemo.posting.rules;

import com.carddemo.posting.domain.Account;
import com.carddemo.posting.domain.AccountId;
import com.carddemo.posting.domain.CardNumber;
import com.carddemo.posting.domain.DailyTransaction;
import com.carddemo.posting.domain.TransactionCategoryBalance;
import com.carddemo.posting.domain.TransactionCategoryKey;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Unit tests for the individual business rules, stated in the terms an SME would use.
 *
 * <p>The parity harness proves the whole program matches the mainframe; these tests pin the
 * individual rules, including the ones that only look right once you know they are faithful
 * copies of a mainframe defect.
 */
class PostingRulesTest {

    private static Account account(String creditLimit, String cycleCredit, String cycleDebit,
                                   String expiry) {
        return new Account(AccountId.of("1"), "Y",
                new BigDecimal("100.00"), new BigDecimal(creditLimit), new BigDecimal("500.00"),
                "2000-01-01", expiry, "2020-01-01",
                new BigDecimal(cycleCredit), new BigDecimal(cycleDebit), "12345", "");
    }

    private static DailyTransaction transaction(String amount, String originTimestamp) {
        return new DailyTransaction("TX", "01", "0001", "POS", "desc", new BigDecimal(amount),
                "000000001", "merchant", "city", "12345", CardNumber.of("4111111111111111"),
                originTimestamp, " ".repeat(26));
    }

    @Test
    @DisplayName("BR-4: a transaction is over limit when this cycle's exposure exceeds the limit")
    void creditLimitComparesCycleExposure() {
        Account account = account("1000.00", "900.00", "0.00", "2099-12-31");
        assertTrue(CreditLimitRule.permits(account, new BigDecimal("100.00")));
        assertFalse(CreditLimitRule.permits(account, new BigDecimal("100.01")));
    }

    @Test
    @DisplayName("BR-4a: the limit check ignores the account balance and re-adds cycle debits")
    void creditLimitIgnoresBalanceAndSubtractsNegativeDebits() {
        // A cycle debit of -300.00 makes the projection larger, not smaller, because the debit
        // bucket holds negative amounts and the COBOL subtracts it.
        Account account = account("1000.00", "0.00", "-300.00", "2099-12-31");
        assertEquals(new BigDecimal("400.00"),
                CreditLimitRule.projectedCycleBalance(account, new BigDecimal("100.00")));
    }

    @Test
    @DisplayName("BR-4b: the projected exposure truncates into PIC S9(09)V99")
    void projectedBalanceTruncatesToNineIntegerDigits() {
        // 999,999,999.99 + 999,999,999.99 = 1,999,999,999.98, of which the field keeps nine
        // integer digits: the leading 1 is lost, silently.
        Account account = account("9999999999.99", "999999999.99", "0.00", "2099-12-31");
        assertEquals(new BigDecimal("999999999.98"),
                CreditLimitRule.projectedCycleBalance(account, new BigDecimal("999999999.99")));
    }

    @Test
    @DisplayName("BR-5: a transaction originating after the expiry date is rejected")
    void expiryComparesOriginationDate() {
        Account account = account("1000.00", "0.00", "0.00", "2022-06-30");
        assertTrue(AccountExpiryRule.expiredFor(account,
                transaction("10.00", "2022-07-01-11.22.33.444444")));
        assertFalse(AccountExpiryRule.expiredFor(account,
                transaction("10.00", "2022-06-30-23.59.59.999999")));
    }

    @Test
    @DisplayName("BR-10: a positive amount moves the credit bucket, a negative one the debit bucket")
    void postingSplitsCreditAndDebit() {
        Account account = account("1000.00", "10.00", "-5.00", "2099-12-31");

        Account credited = account.withTransactionPosted(new BigDecimal("20.00"));
        assertEquals(new BigDecimal("120.00"), credited.currentBalance());
        assertEquals(new BigDecimal("30.00"), credited.currentCycleCredit());
        assertEquals(new BigDecimal("-5.00"), credited.currentCycleDebit());

        Account debited = account.withTransactionPosted(new BigDecimal("-20.00"));
        assertEquals(new BigDecimal("80.00"), debited.currentBalance());
        assertEquals(new BigDecimal("10.00"), debited.currentCycleCredit());
        assertEquals(new BigDecimal("-25.00"), debited.currentCycleDebit());
    }

    @Test
    @DisplayName("BR-8: a category balance opens at zero and accumulates with truncation")
    void categoryBalanceAccumulates() {
        TransactionCategoryKey key =
                new TransactionCategoryKey(AccountId.of("1"), "01", "0001");
        TransactionCategoryBalance opened = TransactionCategoryBalance.opened(key);
        assertEquals(new BigDecimal("0.00"), opened.balance());
        assertEquals(new BigDecimal("999999999.99"),
                opened.withAmountAdded(new BigDecimal("999999999.99")).balance());
        assertEquals(new BigDecimal("999999999.98"),
                opened.withAmountAdded(new BigDecimal("999999999.99"))
                        .withAmountAdded(new BigDecimal("999999999.99")).balance());
    }

    @Test
    @DisplayName("BR-11: eleven-digit account ids are zero padded so keys compare as they do in VSAM")
    void accountIdIsPadded() {
        assertEquals("00000000001", AccountId.of("1").value());
        assertEquals("0000000000101" + "0001",
                new TransactionCategoryKey(AccountId.of("1"), "01", "0001").keyText());
    }
}
