package com.carddemo.interest;

import com.carddemo.interest.model.AccountRecord;
import com.carddemo.interest.model.CardXrefRecord;
import com.carddemo.interest.model.DisclosureGroupKey;
import com.carddemo.interest.model.DisclosureGroupRecord;
import com.carddemo.interest.model.TranCatBalRecord;
import com.carddemo.interest.model.TransactionRecord;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.Iterator;
import java.util.Optional;
import java.util.function.Supplier;

/**
 * PROCEDURE DIVISION of CBACT04C: a control-break walk over the TCATBALF file, accumulating interest
 * per account and posting it to the account when the account id changes.
 *
 * <p>File access is expressed through the four ports below so the logic can be exercised without
 * VSAM; a production adapter supplies KSDS, JDBC or repository implementations.
 */
public final class InterestCalculationBatch {

    /** TCATBALF — read sequentially in key order (1000-TCATBALF-GET-NEXT). */
    public interface TranCatBalReader extends Iterable<TranCatBalRecord> {
    }

    /** ACCTFILE — random read by ACCT-ID and REWRITE (1100-GET-ACCT-DATA, 1050-UPDATE-ACCOUNT). */
    public interface AccountStore {
        Optional<AccountRecord> read(String acctId);

        void rewrite(AccountRecord account);
    }

    /** XREFFILE — random read on the ACCT-ID alternate index (1110-GET-XREF-DATA). */
    public interface CardXrefStore {
        Optional<CardXrefRecord> readByAccount(String acctId);
    }

    /** DISCGRP — random read by group/type/category key (1200-GET-INTEREST-RATE). */
    public interface DisclosureGroupStore {
        Optional<DisclosureGroupRecord> read(DisclosureGroupKey key);
    }

    /** TRANSACT — sequential WRITE of the generated interest transactions (1300-B-WRITE-TX). */
    public interface TransactionWriter {
        void write(TransactionRecord transaction);
    }

    /** Result counters mirroring WS-COUNTERS plus the account rewrites performed. */
    public record Result(long recordsRead, long transactionsWritten, long accountsUpdated) {
    }

    private final InterestCalculator calculator = new InterestCalculator();
    private final TranCatBalReader categoryBalances;
    private final AccountStore accounts;
    private final CardXrefStore xrefs;
    private final DisclosureGroupStore disclosureGroups;
    private final TransactionWriter transactions;
    private final Supplier<LocalDateTime> clock;
    private final boolean postFinalAccountGroup;

    public InterestCalculationBatch(
            TranCatBalReader categoryBalances,
            AccountStore accounts,
            CardXrefStore xrefs,
            DisclosureGroupStore disclosureGroups,
            TransactionWriter transactions,
            Supplier<LocalDateTime> clock) {
        this(categoryBalances, accounts, xrefs, disclosureGroups, transactions, clock, false);
    }

    /**
     * @param postFinalAccountGroup when {@code false} the port reproduces CBACT04C exactly: the
     *     1050-UPDATE-ACCOUNT call for the last account in the file sits in an {@code ELSE} branch
     *     that the {@code PERFORM UNTIL END-OF-FILE = 'Y'} loop can never reach, so the final
     *     account's interest is written to TRANSACT but never added to its balance. Set to
     *     {@code true} to post it, once the business has confirmed the legacy behaviour is a defect.
     */
    public InterestCalculationBatch(
            TranCatBalReader categoryBalances,
            AccountStore accounts,
            CardXrefStore xrefs,
            DisclosureGroupStore disclosureGroups,
            TransactionWriter transactions,
            Supplier<LocalDateTime> clock,
            boolean postFinalAccountGroup) {
        this.categoryBalances = categoryBalances;
        this.accounts = accounts;
        this.xrefs = xrefs;
        this.disclosureGroups = disclosureGroups;
        this.transactions = transactions;
        this.clock = clock;
        this.postFinalAccountGroup = postFinalAccountGroup;
    }

    /** Equivalent of the main PERFORM loop; {@code runDate} is the {@code PARM-DATE} from the JCL. */
    public Result run(String runDate) {
        String lastAcctId = null;
        AccountRecord account = null;
        CardXrefRecord xref = null;
        BigDecimal totalInterest = CobolDecimal.ZERO_MONEY;
        long recordsRead = 0;
        long transactionsWritten = 0;
        long accountsUpdated = 0;
        long tranIdSuffix = 0;

        Iterator<TranCatBalRecord> iterator = categoryBalances.iterator();
        while (iterator.hasNext()) {
            TranCatBalRecord categoryBalance = iterator.next();
            recordsRead++;

            if (!categoryBalance.acctId().equals(lastAcctId)) {
                if (lastAcctId != null) {
                    accounts.rewrite(calculator.applyInterestToAccount(account, totalInterest));
                    accountsUpdated++;
                }
                totalInterest = CobolDecimal.ZERO_MONEY;
                lastAcctId = categoryBalance.acctId();
                account = accounts.read(lastAcctId)
                        .orElseThrow(() -> new IllegalStateException("ACCOUNT NOT FOUND: " + categoryBalance.acctId()));
                xref = xrefs.readByAccount(lastAcctId)
                        .orElseThrow(() -> new IllegalStateException("ACCOUNT NOT FOUND: " + categoryBalance.acctId()));
            }

            BigDecimal rate = resolveInterestRate(account.groupId(), categoryBalance);
            if (rate.signum() != 0) {
                BigDecimal monthlyInterest = calculator.computeMonthlyInterest(categoryBalance.balance(), rate);
                totalInterest = calculator.accumulate(totalInterest, monthlyInterest);
                tranIdSuffix++;
                transactions.write(calculator.buildInterestTransaction(
                        runDate, tranIdSuffix, account, xref, monthlyInterest, clock.get()));
                transactionsWritten++;
                calculator.computeFees(categoryBalance, rate);
            }
        }

        if (postFinalAccountGroup && lastAcctId != null) {
            accounts.rewrite(calculator.applyInterestToAccount(account, totalInterest));
            accountsUpdated++;
        }

        return new Result(recordsRead, transactionsWritten, accountsUpdated);
    }

    /**
     * 1200-GET-INTEREST-RATE with its 1200-A-GET-DEFAULT-INT-RATE fallback: a missing disclosure
     * group record (file status 23) is retried under the group id {@code DEFAULT}.
     */
    BigDecimal resolveInterestRate(String acctGroupId, TranCatBalRecord categoryBalance) {
        DisclosureGroupKey key =
                new DisclosureGroupKey(acctGroupId, categoryBalance.typeCd(), categoryBalance.categoryCd());
        DisclosureGroupRecord group = disclosureGroups.read(key)
                .orElseGet(() -> disclosureGroups.read(key.withDefaultGroup())
                        .orElseThrow(() -> new IllegalStateException("ERROR READING DEFAULT DISCLOSURE GROUP")));
        return group.interestRate();
    }
}
