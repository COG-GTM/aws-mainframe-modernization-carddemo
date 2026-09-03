package com.carddemo.interest.rules;

import com.carddemo.interest.domain.Account;
import com.carddemo.interest.domain.AccountId;
import com.carddemo.interest.domain.CardXref;
import com.carddemo.interest.domain.TransactionCategory;
import com.carddemo.interest.domain.TransactionCategoryBalance;
import com.carddemo.interest.exception.AccountNotFoundException;
import com.carddemo.interest.repository.InMemoryAccountRepository;
import com.carddemo.interest.repository.InMemoryCardXrefRepository;
import com.carddemo.interest.service.FinalAccountPolicy;
import com.carddemo.interest.service.InterestAccrualResult;
import com.carddemo.interest.service.InterestAccrualService;
import com.carddemo.mainframe.cobol.Db2TimestampFormatter;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/** Behavioural tests for the interest cycle itself (BR-1, BR-2, BR-6, BR-7, BR-8). */
class InterestAccrualServiceTest {

    private static final TransactionCategory PURCHASES = TransactionCategory.of("01", 1);
    private static final TransactionCategory CASH_ADVANCES = TransactionCategory.of("01", 2);

    private static Account account(String id, String balance) {
        BigDecimal zero = new BigDecimal("0.00");
        return new Account(AccountId.of(id), "Y", new BigDecimal(balance), new BigDecimal("5000.00"),
                new BigDecimal("1000.00"), "2020-01-01", "2030-01-01", "2030-01-01",
                new BigDecimal("100.00"), new BigDecimal("200.00"), "12345", "A000000001");
    }

    private InterestAccrualService serviceFor(List<Account> accounts, FinalAccountPolicy policy) {
        List<CardXref> xrefs = accounts.stream()
                .map(a -> new CardXref("4111" + a.id().value() + "9", "000000001", a.id())).toList();
        return new InterestAccrualService(
                new InMemoryAccountRepository(accounts),
                new InMemoryCardXrefRepository(xrefs),
                (group, category) -> new BigDecimal("12.00"),
                new MonthlyInterestCalculator(),
                new InterestTransactionFactory(new TransactionIdSequence("2022071800"),
                        new Db2TimestampFormatter(Clock.fixed(Instant.parse("2022-07-18T23:14:05.120Z"),
                                ZoneOffset.UTC))),
                policy);
    }

    @Test
    @DisplayName("Interest accrues per category and posts once per account")
    void accruesPerCategoryAndPostsOnce() {
        List<Account> accounts = List.of(account("00000000001", "100.00"), account("00000000002", "50.00"));
        InterestAccrualResult result = serviceFor(accounts, FinalAccountPolicy.POST_FINAL_ACCOUNT)
                .accrue(List.of(
                        new TransactionCategoryBalance(AccountId.of("00000000001"), PURCHASES, new BigDecimal("1000.00")),
                        new TransactionCategoryBalance(AccountId.of("00000000001"), CASH_ADVANCES, new BigDecimal("2000.00")),
                        new TransactionCategoryBalance(AccountId.of("00000000002"), PURCHASES, new BigDecimal("100.00"))));

        assertEquals(3, result.transactions().size());
        assertEquals(2, result.updatedAccounts().size());
        // 1000 * 12 / 1200 = 10.00 and 2000 * 12 / 1200 = 20.00, posted onto a 100.00 balance.
        assertEquals(new BigDecimal("130.00"), result.updatedAccounts().get(0).currentBalance());
        assertEquals(new BigDecimal("0.00"), result.updatedAccounts().get(0).currentCycleCredit());
        assertEquals(new BigDecimal("0.00"), result.updatedAccounts().get(0).currentCycleDebit());
        assertEquals("2022071800000001", result.transactions().get(0).transactionId());
        assertEquals("Int. for a/c 00000000001", result.transactions().get(0).description());
        assertEquals("2022-07-18-23.14.05.120000", result.transactions().get(0).originTimestamp());
    }

    @Test
    @DisplayName("Under mainframe parity the last account of the run is not balance updated")
    void finalAccountIsNotPostedUnderParityPolicy() {
        List<Account> accounts = List.of(account("00000000001", "100.00"), account("00000000002", "50.00"));
        InterestAccrualResult result = serviceFor(accounts, FinalAccountPolicy.MAINFRAME_PARITY)
                .accrue(List.of(
                        new TransactionCategoryBalance(AccountId.of("00000000001"), PURCHASES, new BigDecimal("1000.00")),
                        new TransactionCategoryBalance(AccountId.of("00000000002"), PURCHASES, new BigDecimal("1000.00"))));

        assertEquals(2, result.transactions().size(), "both accounts still raise interest transactions");
        assertEquals(1, result.updatedAccounts().size(), "but only the non-final account is rewritten");
        assertEquals(AccountId.of("00000000001"), result.updatedAccounts().get(0).id());
    }

    @Test
    @DisplayName("A category balance without an account master record fails the run")
    void missingAccountFailsTheRun() {
        InterestAccrualService service = serviceFor(List.of(account("00000000001", "100.00")),
                FinalAccountPolicy.POST_FINAL_ACCOUNT);
        assertThrows(AccountNotFoundException.class, () -> service.accrue(List.of(
                new TransactionCategoryBalance(AccountId.of("00000000099"), PURCHASES, new BigDecimal("10.00")))));
    }
}
