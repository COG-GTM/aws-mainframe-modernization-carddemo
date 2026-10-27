package com.carddemo.modernization.interest;

import com.carddemo.modernization.records.AccountRecord;
import com.carddemo.modernization.records.CardXrefRecord;
import com.carddemo.modernization.records.DisclosureGroupRecord;
import com.carddemo.modernization.records.TranCatBalanceRecord;
import com.carddemo.modernization.records.TransactionRecord;
import com.carddemo.modernization.records.codec.ZonedDecimal;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.charset.StandardCharsets;
import java.time.Clock;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * Calculates interest from the supplied records. Per INT-05, transactions for the last account are
 * written but that account is not rewritten.
 */
public final class InterestCalculator {
    private static final BigDecimal ZERO_MONEY = new BigDecimal("0.00");
    private static final BigDecimal TRANSACTION_DIVISOR = BigDecimal.valueOf(1200);
    private static final String DEFAULT_GROUP_ID = "DEFAULT   ";
    private static final DateTimeFormatter TIMESTAMP_FORMAT =
            DateTimeFormatter.ofPattern("uuuu-MM-dd-HH.mm.ss.", Locale.ROOT);

    private final Clock clock;
    private final String parmDate;

    public InterestCalculator(Clock clock, String parmDate) {
        this.clock = Objects.requireNonNull(clock, "clock");
        // INT-01: PARM-DATE is PIC X(10) in the linkage parameter (app/cbl/CBACT04C.cbl:175-178).
        if (parmDate == null || parmDate.length() != 10) {
            throw new IllegalArgumentException("parmDate must be exactly 10 characters");
        }
        this.parmDate = parmDate;
    }

    public InterestRun run(InterestInput input) {
        Objects.requireNonNull(input, "input");
        List<TranCatBalanceRecord> categoryBalances = new ArrayList<>(input.categoryBalances());
        List<AccountRecord> sourceAccounts = List.copyOf(input.accounts());
        List<CardXrefRecord> crossReferences = List.copyOf(input.crossReferences());
        List<DisclosureGroupRecord> disclosureGroups = List.copyOf(input.disclosureGroups());

        // INT-03: TCATBALF is consumed in account/type/category key order (app/cbl/CBACT04C.cbl:188-193;
        // app/cpy/CVTRA01Y.cpy:4-10).
        // INT-03: A KSDS primary key is unique, so duplicate category-balance keys are rejected.
        requireUniqueCategoryKeys(categoryBalances);
        categoryBalances.sort(Comparator.comparing(TranCatBalanceRecord::accountId)
                .thenComparing(TranCatBalanceRecord::typeCode)
                .thenComparing(TranCatBalanceRecord::categoryCode));
        // INT-03/06/07/08: Model KSDS access with primary-key indexes; account ids and disclosure
        // keys correspond to app/cpy/CVACT01Y.cpy:5, app/cpy/CVTRA02Y.cpy:6-9.
        Map<String, AccountRecord> accountsById = indexAccounts(sourceAccounts);
        Map<String, CardXrefRecord> crossReferencesByAccount = indexCrossReferences(crossReferences);
        Map<DisclosureKey, DisclosureGroupRecord> ratesByKey = indexDisclosureGroups(disclosureGroups);
        Map<String, AccountRecord> rewrittenAccounts = new HashMap<>();
        List<TransactionRecord> transactions = new ArrayList<>();
        List<String> sysout = new ArrayList<>();
        sysout.add("START OF EXECUTION OF PROGRAM CBACT04C");

        String currentAccountId = null;
        AccountRecord currentAccount = null;
        CardXrefRecord currentCrossReference = null;
        BigDecimal accountTotal = ZERO_MONEY;
        int transactionSuffix = 0;

        for (TranCatBalanceRecord categoryBalance : categoryBalances) {
            sysout.add(new String(TranCatBalanceRecord.CODEC.encode(categoryBalance), StandardCharsets.US_ASCII));
            // INT-04: At an account break, rewrite the previous account and reset its total
            // (app/cbl/CBACT04C.cbl:194-206).
            if (!categoryBalance.accountId().equals(currentAccountId)) {
                if (currentAccount != null) {
                    rewrittenAccounts.put(currentAccount.acctId(), updateAccount(currentAccount, accountTotal));
                }
                currentAccountId = categoryBalance.accountId();
                accountTotal = ZERO_MONEY;
                currentAccount = accountsById.get(currentAccountId);
                if (currentAccount == null) {
                    throw new InterestAbendException(
                            List.of("ACCOUNT NOT FOUND: " + currentAccountId, "ERROR READING ACCOUNT FILE"), "23");
                }
                currentCrossReference = crossReferencesByAccount.get(currentAccountId);
                if (currentCrossReference == null) {
                    throw new InterestAbendException(
                            List.of("ACCOUNT NOT FOUND: " + currentAccountId, "ERROR READING XREF FILE"), "23");
                }
            }

            DisclosureGroupRecord disclosure = ratesByKey.get(disclosureKey(
                    currentAccount.groupId(), categoryBalance.typeCode(), categoryBalance.categoryCode()));
            if (disclosure == null) {
                // INT-09: Retry only a missing group key, retaining the transaction type/category
                // (app/cbl/CBACT04C.cbl:417-460).
                sysout.add("DISCLOSURE GROUP RECORD MISSING");
                sysout.add("TRY WITH DEFAULT GROUP CODE");
                disclosure = ratesByKey.get(disclosureKey(
                        DEFAULT_GROUP_ID, categoryBalance.typeCode(), categoryBalance.categoryCode()));
                if (disclosure == null) {
                    throw new InterestAbendException(List.of("ERROR READING DEFAULT DISCLOSURE GROUP"), "23");
                }
            }

            BigDecimal rate = disclosure.interestRate().value();
            if (rate.signum() != 0) {
                // INT-11: Division truncates to cents without ROUNDED, then PIC S9(09)V99
                // discards high-order digits (app/cbl/CBACT04C.cbl:462-465; app/cpy/CVTRA01Y.cpy:9).
                // Computing the exact product/division before the cent truncation is equivalent to
                // truncating any intermediate with at least two fractional digits.
                BigDecimal monthlyInterest = categoryBalance.balance().value()
                        .multiply(rate)
                        .divide(TRANSACTION_DIVISOR, 2, RoundingMode.DOWN);
                monthlyInterest = truncateToPic(monthlyInterest, 9);
                // INT-12: Keep the account total in the same nine-integer-digit PIC (CBACT04C.cbl:169,467).
                accountTotal = truncateToPic(accountTotal.add(monthlyInterest), 9);
                transactionSuffix = nextTranIdSuffix(transactionSuffix);
                String timestamp = timestamp();
                // INT-13/14/15: The counter is run-wide and incremented per write; the literal '05'
                // is moved to PIC 9(04), and untouched TRAN-RECORD storage is space-initialized in
                // GnuCOBOL as the golden SYSTRAN shows; a mainframe runtime may differ
                // (app/cbl/CBACT04C.cbl:468,474-495; app/cpy/CVTRA05Y.cpy:9,18).
                transactions.add(new TransactionRecord(
                        parmDate + String.format(Locale.ROOT, "%06d", transactionSuffix),
                        "01",
                        "0005",
                        padText("System", 10),
                        padText("Int. for a/c " + currentAccount.acctId(), 100),
                        // INT-11: A cent-truncated zero is encoded as positive zero (CBACT04C.cbl:462-465).
                        new ZonedDecimal(monthlyInterest),
                        "000000000",
                        " ".repeat(50),
                        " ".repeat(50),
                        " ".repeat(10),
                        padText(currentCrossReference.cardNumber(), 16),
                        timestamp,
                        timestamp,
                        " ".repeat(20)));
                computeFees();
            } else {
                // INT-10: A zero rate skips both transaction writing and the fee paragraph (CBACT04C.cbl:214-217).
            }
        }

        skipLastAccountUpdatePerInt05();
        List<AccountRecord> resultAccounts = sourceAccounts.stream()
                .sorted(Comparator.comparing(AccountRecord::acctId))
                .map(account -> rewrittenAccounts.getOrDefault(account.acctId(), account))
                .toList();
        sysout.add("END OF EXECUTION OF PROGRAM CBACT04C");
        return new InterestRun(transactions, resultAccounts, sysout);
    }

    static BigDecimal truncateToPic(BigDecimal value, int integerDigits) {
        if (integerDigits < 1) throw new IllegalArgumentException("integerDigits must be positive");
        BigDecimal cents = value.setScale(2, RoundingMode.DOWN);
        BigDecimal magnitude = cents.abs().remainder(BigDecimal.TEN.pow(integerDigits)).setScale(2);
        return cents.signum() < 0 ? magnitude.negate() : magnitude;
    }

    static int nextTranIdSuffix(int suffix) {
        return (suffix + 1) % 1_000_000;
    }

    private static void requireUniqueCategoryKeys(List<TranCatBalanceRecord> categoryBalances) {
        Set<CategoryKey> primaryKeys = new HashSet<>();
        for (TranCatBalanceRecord categoryBalance : categoryBalances) {
            CategoryKey key = new CategoryKey(
                    categoryBalance.accountId(), categoryBalance.typeCode(), categoryBalance.categoryCode());
            if (!primaryKeys.add(key)) {
                throw new IllegalArgumentException("Duplicate category-balance primary key: " + key);
            }
        }
    }

    private static Map<String, AccountRecord> indexAccounts(List<AccountRecord> accounts) {
        Map<String, AccountRecord> indexed = new HashMap<>();
        for (AccountRecord account : accounts) {
            if (indexed.putIfAbsent(account.acctId(), account) != null) {
                throw new IllegalArgumentException("Duplicate account primary key: " + account.acctId());
            }
        }
        return indexed;
    }

    private static Map<String, CardXrefRecord> indexCrossReferences(List<CardXrefRecord> crossReferences) {
        Set<String> primaryKeys = new HashSet<>();
        List<CardXrefRecord> ordered = new ArrayList<>(crossReferences);
        for (CardXrefRecord crossReference : ordered) {
            String cardNumber = padText(crossReference.cardNumber(), 16);
            if (!primaryKeys.add(cardNumber)) {
                throw new IllegalArgumentException("Duplicate xref primary key: " + crossReference.cardNumber());
            }
        }
        // INT-07: The source reads by alternate account key but gives no multiple-card selection rule;
        // choosing the first primary-key (card number) match is an explicit deterministic assumption.
        ordered.sort(Comparator.comparing(crossReference -> padText(crossReference.cardNumber(), 16)));
        Map<String, CardXrefRecord> byAccount = new HashMap<>();
        for (CardXrefRecord crossReference : ordered) {
            byAccount.putIfAbsent(crossReference.accountId(), crossReference);
        }
        return byAccount;
    }

    private static Map<DisclosureKey, DisclosureGroupRecord> indexDisclosureGroups(
            List<DisclosureGroupRecord> disclosureGroups) {
        Map<DisclosureKey, DisclosureGroupRecord> indexed = new HashMap<>();
        for (DisclosureGroupRecord disclosure : disclosureGroups) {
            DisclosureKey key = disclosureKey(
                    disclosure.groupId(), disclosure.typeCode(), disclosure.categoryCode());
            if (indexed.putIfAbsent(key, disclosure) != null) {
                throw new IllegalArgumentException("Duplicate disclosure primary key: " + key);
            }
        }
        return indexed;
    }

    private static DisclosureKey disclosureKey(String groupId, String typeCode, String categoryCode) {
        // INT-08/09: DISCGRP keys use a ten-byte, space-padded group id (app/cpy/CVTRA02Y.cpy:6-9).
        String paddedGroupId = padText(groupId, 10);
        return new DisclosureKey(paddedGroupId, typeCode, categoryCode);
    }

    private static String padText(String value, int width) {
        if (value.length() > width) throw new IllegalArgumentException("Text exceeds " + width + " characters");
        return value + " ".repeat(width - value.length());
    }

    private static AccountRecord updateAccount(AccountRecord account, BigDecimal total) {
        // INT-17: Update the balance and clear both cycle totals, preserving all other fields
        // (app/cbl/CBACT04C.cbl:350-370; app/cpy/CVACT01Y.cpy:7,13-14).
        BigDecimal currentBalance = truncateToPic(account.currentBalance().value().add(total), 10);
        // INT-11/17: Constructing zero without a negative-zero flag ensures positive overpunch on write.
        return new AccountRecord(
                account.acctId(),
                account.activeStatus(),
                new ZonedDecimal(currentBalance),
                account.creditLimit(),
                account.cashCreditLimit(),
                account.openDate(),
                account.expirationDate(),
                account.reissueDate(),
                new ZonedDecimal(ZERO_MONEY),
                new ZonedDecimal(ZERO_MONEY),
                account.addressZip(),
                account.groupId(),
                account.filler());
    }

    private String timestamp() {
        // INT-16: CURRENT-DATE contributes hundredths in the injected clock's zone (app/cbl/CBACT04C.cbl:613-626).
        ZonedDateTime now = ZonedDateTime.now(clock);
        return TIMESTAMP_FORMAT.format(now)
                + String.format(Locale.ROOT, "%02d0000", now.getNano() / 10_000_000);
    }

    private void computeFees() {
        // INT-18: The called paragraph is intentionally empty (app/cbl/CBACT04C.cbl:216,518-520).
    }

    private void skipLastAccountUpdatePerInt05() {
        // INT-05: EOF is tested before the loop body; the last account's update branch is unreachable
        // (app/cbl/CBACT04C.cbl:188-191,219-222).
    }

    private record DisclosureKey(String groupId, String typeCode, String categoryCode) {}
    private record CategoryKey(String accountId, String typeCode, String categoryCode) {}
}
