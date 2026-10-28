package com.carddemo.interest.domain;

import com.carddemo.recordio.layout.Account;
import com.carddemo.recordio.layout.CardXref;
import com.carddemo.recordio.layout.Transaction;
import com.carddemo.recordio.layout.TransactionCategoryBalance;
import com.carddemo.recordio.store.KeyedRecordStore;
import com.carddemo.recordio.store.RecordNotFoundException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * The account-level control break of CBACT04C's main loop (lines 187-232): category balances arrive
 * in key order (account id first), so the per-account interest total is accumulated and applied to
 * the account when the account id changes and once more at end of file.
 *
 * <p>1050-UPDATE-ACCOUNT: {@code ACCT-CURR-BAL += total interest; CYC-CREDIT = CYC-DEBIT = 0}.
 * The reset happens even for accounts whose categories all carry a zero rate.
 */
public final class InterestPostingRun {

    private static final Logger LOG = LoggerFactory.getLogger(InterestPostingRun.class);

    private final KeyedRecordStore<Account> accounts;
    private final KeyedRecordStore<CardXref> cardXref;
    private final InterestRateLookup rates;
    private final InterestTransactionFactory transactions;
    private final List<Transaction> systemTransactions = new ArrayList<>();

    private Account currentAccount;
    private String currentCard;
    private BigDecimal totalInterest = BigDecimal.ZERO;
    private long recordCount;

    public InterestPostingRun(KeyedRecordStore<Account> accounts, KeyedRecordStore<CardXref> cardXref,
                              InterestRateLookup rates, InterestTransactionFactory transactions) {
        this.accounts = accounts;
        this.cardXref = cardXref;
        this.rates = rates;
        this.transactions = transactions;
    }

    /** One iteration of the read loop for a non-EOF record. */
    public void accept(TransactionCategoryBalance balance) {
        recordCount++;
        if (currentAccount == null || !currentAccount.accountId().equals(balance.accountId())) {
            if (currentAccount != null) {
                applyInterestToAccount();
            }
            totalInterest = BigDecimal.ZERO;
            currentAccount = accounts.read(balance.accountId()).orElseThrow(() -> {
                LOG.error("ACCOUNT NOT FOUND: {}", balance.accountId());
                return new RecordNotFoundException("ACCTFILE", balance.accountId(), "ERROR READING ACCOUNT FILE");
            });
            currentCard = firstCardOf(balance.accountId()).orElseThrow(() -> {
                LOG.error("ACCOUNT NOT FOUND: {}", balance.accountId());
                return new RecordNotFoundException("XREFFILE", balance.accountId(), "ERROR READING XREF FILE");
            });
        }
        BigDecimal rate = rates.rateFor(currentAccount.groupId(), balance.typeCode(), balance.categoryCode());
        if (rate.signum() != 0) {
            BigDecimal monthly = InterestCalculator.monthlyInterest(balance.balance(), rate);
            totalInterest = totalInterest.add(monthly);
            systemTransactions.add(transactions.next(currentAccount.accountId(), currentCard, monthly));
        }
    }

    /**
     * End of file. The COBOL performs 1050-UPDATE-ACCOUNT unconditionally here; with an empty
     * input that REWRITEs an uninitialised ACCOUNT-RECORD (open question). Here an empty run is a no-op.
     */
    public void finish() {
        if (currentAccount != null) {
            applyInterestToAccount();
        }
    }

    private void applyInterestToAccount() {
        Account updated = currentAccount.withBalances(
                com.carddemo.recordio.codec.CobolNumeric.truncate(
                        currentAccount.currentBalance().add(totalInterest), Account.MONEY_INT_DIGITS, Account.MONEY_SCALE),
                BigDecimal.ZERO, BigDecimal.ZERO);
        accounts.rewrite(updated);
    }

    /**
     * READ XREF-FILE KEY IS FD-XREF-ACCT-ID through the non-unique alternate index: VSAM returns the
     * first record in base-key (card number) order among those with that account id.
     */
    private Optional<String> firstCardOf(String accountId) {
        return cardXref.readAll()
                .filter(x -> x.accountId().equals(accountId))
                .map(CardXref::cardNumber)
                .findFirst();
    }

    public List<Transaction> systemTransactions() {
        return systemTransactions;
    }

    public long recordCount() {
        return recordCount;
    }
}
