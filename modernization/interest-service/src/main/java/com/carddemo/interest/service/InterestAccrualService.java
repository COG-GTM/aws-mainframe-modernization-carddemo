package com.carddemo.interest.service;

import com.carddemo.interest.domain.Account;
import com.carddemo.interest.domain.CardXref;
import com.carddemo.interest.domain.InterestTransaction;
import com.carddemo.interest.domain.TransactionCategoryBalance;
import com.carddemo.interest.exception.AccountNotFoundException;
import com.carddemo.interest.exception.CardXrefNotFoundException;
import com.carddemo.interest.repository.AccountRepository;
import com.carddemo.interest.repository.CardXrefRepository;
import com.carddemo.interest.rules.InterestTransactionFactory;
import com.carddemo.interest.rules.MonthlyInterestCalculator;
import com.carddemo.interest.rules.RateResolver;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

/**
 * The monthly interest cycle: for every account, price each transaction-category balance, raise an
 * interest transaction for it and post the total to the account.
 *
 * <p>Modernised from COBOL batch program {@code CBACT04C} ({@code app/cbl/CBACT04C.cbl}), run by
 * JCL job {@code INTCALC} ({@code app/jcl/INTCALC.jcl}). The COBOL paragraph structure is not
 * mirrored: file opens/closes, status checking and abend handling disappear into the repositories
 * and typed exceptions, and each surviving business rule lives in its own collaborator
 * ({@link RateResolver}, {@link MonthlyInterestCalculator}, {@link InterestTransactionFactory},
 * {@link Account#withInterestPosted(BigDecimal)}).
 */
public final class InterestAccrualService {

    private static final BigDecimal NO_INTEREST = BigDecimal.ZERO.setScale(2);

    private final AccountRepository accounts;
    private final CardXrefRepository cardXrefs;
    private final RateResolver rateResolver;
    private final MonthlyInterestCalculator calculator;
    private final InterestTransactionFactory transactionFactory;
    private final FinalAccountPolicy finalAccountPolicy;

    public InterestAccrualService(AccountRepository accounts,
                                  CardXrefRepository cardXrefs,
                                  RateResolver rateResolver,
                                  MonthlyInterestCalculator calculator,
                                  InterestTransactionFactory transactionFactory,
                                  FinalAccountPolicy finalAccountPolicy) {
        this.accounts = accounts;
        this.cardXrefs = cardXrefs;
        this.rateResolver = rateResolver;
        this.calculator = calculator;
        this.transactionFactory = transactionFactory;
        this.finalAccountPolicy = finalAccountPolicy;
    }

    /**
     * Runs the interest cycle over a key-ordered stream of transaction-category balances.
     *
     * <p>Drives business rules BR-1 (account break), BR-2 (account and cross-reference lookup),
     * BR-3 to BR-5 (rate selection, interest computation, accumulation), BR-7 (transaction
     * creation) and BR-6 (account posting), mirroring the control flow of the COBOL main loop
     * ({@code app/cbl/CBACT04C.cbl:188-222}) without its paragraph structure.
     */
    public InterestAccrualResult accrue(List<TransactionCategoryBalance> categoryBalances) {
        List<AccountBalanceGroup> groups = AccountBalanceGrouping.groupConsecutively(categoryBalances);
        List<InterestTransaction> transactions = new ArrayList<>();
        List<Account> updatedAccounts = new ArrayList<>();

        for (int index = 0; index < groups.size(); index++) {
            AccountBalanceGroup group = groups.get(index);
            Account account = accounts.findById(group.accountId())
                    .orElseThrow(() -> new AccountNotFoundException(group.accountId()));
            CardXref cardXref = cardXrefs.findByAccountId(group.accountId())
                    .orElseThrow(() -> new CardXrefNotFoundException(group.accountId()));

            BigDecimal totalInterest = accrueAccount(account, cardXref, group, transactions);

            boolean lastGroup = index == groups.size() - 1;
            if (!lastGroup || finalAccountPolicy == FinalAccountPolicy.POST_FINAL_ACCOUNT) {
                updatedAccounts.add(account.withInterestPosted(totalInterest));
            }
        }
        return new InterestAccrualResult(List.copyOf(transactions), List.copyOf(updatedAccounts),
                categoryBalances.size());
    }

    /**
     * Accrues one account's interest across its category balances.
     *
     * <p>Business rule BR-4a — a disclosed rate of zero suppresses accrual entirely: no interest
     * is added and no transaction is written ({@code IF DIS-INT-RATE NOT = 0} at
     * {@code app/cbl/CBACT04C.cbl:214-217}). This is what the {@code ZEROAPR} disclosure group
     * exists for.
     */
    private BigDecimal accrueAccount(Account account,
                                     CardXref cardXref,
                                     AccountBalanceGroup group,
                                     List<InterestTransaction> transactions) {
        BigDecimal totalInterest = NO_INTEREST;
        for (TransactionCategoryBalance balance : group.balances()) {
            BigDecimal ratePercent = rateResolver.annualRatePercent(
                    account.pricingGroupId(), balance.category());
            if (ratePercent.signum() == 0) {
                continue;
            }
            BigDecimal monthlyInterest = calculator.monthlyInterest(balance.balance(), ratePercent);
            totalInterest = calculator.accumulate(totalInterest, monthlyInterest);
            transactions.add(transactionFactory.create(account, cardXref, monthlyInterest));
        }
        return totalInterest;
    }
}
