package com.carddemo.transaction.batch;

import com.carddemo.common.cobol.CobolValues;
import com.carddemo.transaction.client.AccountGateway;
import com.carddemo.transaction.client.AccountView;
import com.carddemo.transaction.client.CardGateway;
import com.carddemo.transaction.client.CardXrefView;
import com.carddemo.transaction.domain.CategoryBalance;
import com.carddemo.transaction.domain.DisclosureGroup;
import com.carddemo.transaction.domain.DisclosureGroupId;
import com.carddemo.transaction.domain.Transaction;
import com.carddemo.transaction.repository.CategoryBalanceRepository;
import com.carddemo.transaction.repository.DisclosureGroupRepository;
import com.carddemo.transaction.repository.TransactionRepository;
import java.math.BigDecimal;
import java.time.Clock;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * Port of CBACT04C (JCL INTCALC), paragraph by paragraph:
 *
 * <ul>
 *   <li>main loop: TCATBAL is read in key order (account, type, category). On a new account the
 *       previous one is settled ({@code 1050-UPDATE-ACCOUNT}) and the account and its XREF are
 *       read ({@code 1100-GET-ACCT-DATA}, {@code 1110-GET-XREF-DATA}); either one missing abends.
 *       The last account is never settled: the {@code PERFORM UNTIL END-OF-FILE = 'Y'} test runs
 *       before each iteration, so once the read sets END-OF-FILE the loop ends and its
 *       {@code ELSE PERFORM 1050-UPDATE-ACCOUNT} branch is unreachable. Its interest transactions
 *       are still written. This is a COBOL defect kept for parity (see docs/migration/jcl-mapping.md).</li>
 *   <li>{@code 1200-GET-INTEREST-RATE}: DISCGRP by (ACCT-GROUP-ID, type, category); when that
 *       key is absent, {@code 1200-A-GET-DEFAULT-INT-RATE} reads the {@code DEFAULT} group, and a
 *       missing DEFAULT row abends.</li>
 *   <li>{@code 1300-COMPUTE-INTEREST} only when DIS-INT-RATE is not zero:
 *       {@code COMPUTE WS-MONTHLY-INT = (TRAN-CAT-BAL * DIS-INT-RATE) / 1200} into
 *       {@code S9(09)V99} without ROUNDED (truncated), added to WS-TOTAL-INT, and one transaction
 *       written ({@code 1300-B-WRITE-TX}), even when the truncated amount is zero.</li>
 *   <li>{@code 1400-COMPUTE-FEES} is {@code * To be implemented} in the COBOL and is not ported.</li>
 *   <li>{@code 1050-UPDATE-ACCOUNT}: {@code ADD WS-TOTAL-INT TO ACCT-CURR-BAL}, cycle credit and
 *       debit to zero, at every account break, interest or not.</li>
 * </ul>
 *
 * TRAN-ID is {@code PARM-DATE} (the 10-byte EXEC PARM) followed by a 6-digit suffix that counts
 * the run's transactions from 1. Both timestamps are CURRENT-DATE at the time of the write, which
 * COBOL has to the hundredth of a second ({@code Z-GET-DB2-FORMAT-TIMESTAMP}).
 */
@Service
public class InterestCalculationService {

    private static final String INTEREST_TYPE_CD = "01";
    private static final int INTEREST_CAT_CD = 5;
    private static final String SOURCE = "System";
    private static final String DESCRIPTION_PREFIX = "Int. for a/c ";
    private static final BigDecimal MONTHS_TIMES_PERCENT = new BigDecimal("1200");
    private static final int PARM_DATE_LENGTH = 10;
    private static final int SUFFIX_DIGITS = 6;
    private static final int SUFFIX_MODULUS = 1_000_000;
    private static final int ACCT_ID_DIGITS = 11;
    private static final int HUNDREDTH_NANOS = 10_000_000;
    private static final DateTimeFormatter DEFAULT_PARM = DateTimeFormatter.ofPattern("yyyyMMdd'00'");

    private final CategoryBalanceRepository categoryBalances;
    private final DisclosureGroupRepository disclosureGroups;
    private final TransactionRepository transactions;
    private final AccountGateway accounts;
    private final CardGateway cards;
    private final Clock clock;

    public InterestCalculationService(CategoryBalanceRepository categoryBalances,
                                      DisclosureGroupRepository disclosureGroups,
                                      TransactionRepository transactions,
                                      AccountGateway accounts,
                                      CardGateway cards,
                                      Clock clock) {
        this.categoryBalances = categoryBalances;
        this.disclosureGroups = disclosureGroups;
        this.transactions = transactions;
        this.accounts = accounts;
        this.cards = cards;
        this.clock = clock;
    }

    /** INTCALC with the JCL default PARM of today's date ({@code yyyyMMdd00}). */
    @Transactional
    public InterestReport calculateInterest() {
        return calculateInterest(LocalDateTime.now(clock).format(DEFAULT_PARM));
    }

    /**
     * Runs CBACT04C with {@code parm} as PARM-DATE. TRANSACT here is the shared table rather than a
     * fresh SYSTRAN(+1) data set, so a PARM whose TRAN-IDs are already present is refused before
     * anything is written: re-running it would overwrite those transactions.
     */
    @Transactional
    public InterestReport calculateInterest(String parm) {
        String parmDate = CobolValues.padRight(parm == null ? "" : parm, PARM_DATE_LENGTH);
        if (transactions.existsByTranIdStartingWith(parmDate)) {
            throw new IllegalStateException("TRANSACT already has interest transactions for PARM-DATE '"
                    + parmDate + "'");
        }
        Map<Long, BigDecimal> interestByAccount = new LinkedHashMap<>();
        List<Transaction> written = new ArrayList<>();
        AccountView account = null;
        String cardNumber = null;
        int suffix = 0;

        for (CategoryBalance balance : categoryBalances.findAllByOrderByIdAcctIdAscIdTypeCdAscIdCatCdAsc()) {
            long accountId = balance.getId().getAcctId();
            if (account == null || account.accountId() != accountId) {
                account = readAccount(accountId);
                cardNumber = readXref(accountId);
                interestByAccount.put(accountId, BigDecimal.ZERO);
            }
            BigDecimal rate = interestRate(account.groupId(), balance);
            if (rate.signum() != 0) {
                BigDecimal monthlyInterest = CobolValues.divide(
                        balance.getBalance().multiply(rate), MONTHS_TIMES_PERCENT, 2);
                interestByAccount.merge(accountId, monthlyInterest, BigDecimal::add);
                suffix = (suffix + 1) % SUFFIX_MODULUS;
                written.add(interestTransaction(parmDate + CobolValues.zeroPad(suffix, SUFFIX_DIGITS),
                        accountId, cardNumber, monthlyInterest));
            }
        }

        transactions.saveAllAndFlush(written);
        BigDecimal totalInterest = interestByAccount.values().stream().reduce(BigDecimal.ZERO, BigDecimal::add);
        List<Map.Entry<Long, BigDecimal>> settled = List.copyOf(interestByAccount.entrySet());
        settled = settled.subList(0, Math.max(0, settled.size() - 1));
        settled.forEach(entry -> accounts.settleInterest(entry.getKey(), entry.getValue()));
        return new InterestReport(settled.size(), written.size(), totalInterest);
    }

    private AccountView readAccount(long accountId) {
        return accounts.find(accountId).orElseThrow(() -> new IllegalStateException(
                "ACCOUNT NOT FOUND: " + CobolValues.zeroPad(accountId, ACCT_ID_DIGITS)));
    }

    private String readXref(long accountId) {
        return cards.xrefByAccount(accountId).map(CardXrefView::cardNumber).orElseThrow(() ->
                new IllegalStateException("ACCOUNT NOT FOUND IN XREF: "
                        + CobolValues.zeroPad(accountId, ACCT_ID_DIGITS)));
    }

    private BigDecimal interestRate(String accountGroupId, CategoryBalance balance) {
        String typeCd = balance.getId().getTypeCd();
        Integer catCd = balance.getId().getCatCd();
        String groupId = accountGroupId == null ? "" : accountGroupId.strip();
        return disclosureGroups.findById(new DisclosureGroupId(groupId, typeCd, catCd))
                .or(() -> disclosureGroups.findById(
                        new DisclosureGroupId(DisclosureGroup.DEFAULT_GROUP_ID, typeCd, catCd)))
                .map(DisclosureGroup::getInterestRate)
                .orElseThrow(() -> new IllegalStateException("ERROR READING DEFAULT DISCLOSURE GROUP: "
                        + DisclosureGroup.DEFAULT_GROUP_ID + " " + typeCd + " " + catCd));
    }

    private Transaction interestTransaction(String tranId, long accountId, String cardNumber,
                                            BigDecimal monthlyInterest) {
        LocalDateTime now = LocalDateTime.now(clock);
        LocalDateTime timestamp = now.withNano(now.getNano() / HUNDREDTH_NANOS * HUNDREDTH_NANOS);
        Transaction transaction = new Transaction(tranId, cardNumber, INTEREST_TYPE_CD, INTEREST_CAT_CD,
                monthlyInterest);
        transaction.setSource(SOURCE);
        transaction.setDescription(DESCRIPTION_PREFIX + CobolValues.zeroPad(accountId, ACCT_ID_DIGITS));
        transaction.setMerchantId(0L);
        transaction.setMerchantName("");
        transaction.setMerchantCity("");
        transaction.setMerchantZip("");
        transaction.setOrigTs(timestamp);
        transaction.setProcTs(timestamp);
        return transaction;
    }

    public record InterestReport(int accountsSettled, int transactionsWritten, BigDecimal totalInterest) {
    }
}
