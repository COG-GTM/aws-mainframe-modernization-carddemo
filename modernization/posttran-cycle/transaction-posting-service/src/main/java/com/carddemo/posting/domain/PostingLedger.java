package com.carddemo.posting.domain;

import com.carddemo.recordio.layout.Account;
import com.carddemo.recordio.layout.CardXref;
import com.carddemo.recordio.layout.Transaction;
import com.carddemo.recordio.layout.TransactionCategoryBalance;
import com.carddemo.recordio.store.KeyedRecordStore;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigDecimal;
import java.util.Optional;

/**
 * The masters the posting run reads and updates in place: card cross-reference (read-only),
 * account master and transaction-category balances (both rewritten record by record, exactly
 * as CBTRN02C does with {@code OPEN I-O} + {@code REWRITE}).
 *
 * <p>Because validation of transaction N+1 reads the cycle totals updated by transaction N, the
 * updates are applied immediately, not deferred to a commit point. That is the COBOL contract.
 */
public final class PostingLedger {

    private static final Logger LOG = LoggerFactory.getLogger(PostingLedger.class);

    private final KeyedRecordStore<CardXref> cardXref;
    private final KeyedRecordStore<Account> accounts;
    private final KeyedRecordStore<TransactionCategoryBalance> categoryBalances;

    public PostingLedger(KeyedRecordStore<CardXref> cardXref, KeyedRecordStore<Account> accounts,
                         KeyedRecordStore<TransactionCategoryBalance> categoryBalances) {
        this.cardXref = cardXref;
        this.accounts = accounts;
        this.categoryBalances = categoryBalances;
    }

    /** CBTRN02C 1500-A-LOOKUP-XREF: READ XREF-FILE by card number. */
    public Optional<CardXref> findCard(String cardNumber) {
        return cardXref.read(cardNumber);
    }

    /** CBTRN02C 1500-B-LOOKUP-ACCT: READ ACCOUNT-FILE by account id. */
    public Optional<Account> findAccount(String accountId) {
        return accounts.read(accountId);
    }

    /**
     * CBTRN02C 2700-UPDATE-TCATBAL (lines 467-531): add the amount to the (account, type,
     * category) balance, creating the row when absent. The balance field is {@code S9(09)V99}, so
     * the sum is stored with COBOL truncation semantics.
     */
    public TransactionCategoryBalance addToCategoryBalance(String accountId, Transaction t) {
        TransactionCategoryBalance current = categoryBalances
                .read(accountId + t.typeCode() + String.format("%04d", t.categoryCode()))
                .orElse(null);
        if (current == null) {
            LOG.info("TCATBAL record not found for key : {}{}{}.. Creating.", accountId, t.typeCode(),
                    String.format("%04d", t.categoryCode()));
            TransactionCategoryBalance created = new TransactionCategoryBalance(accountId, t.typeCode(),
                    t.categoryCode(), storeBalance(t.amount()));
            categoryBalances.write(created);
            return created;
        }
        TransactionCategoryBalance updated = current.withBalance(storeBalance(current.balance().add(t.amount())));
        categoryBalances.rewrite(updated);
        return updated;
    }

    /**
     * CBTRN02C 2800-UPDATE-ACCOUNT-REC (lines 532-559): the amount is added to the current balance
     * and to exactly one cycle bucket. A negative amount is <em>added</em> to CURR-CYC-DEBIT (so that
     * bucket accumulates a negative number); the code does not negate it. Preserved as written and
     * recorded in open-questions.md.
     */
    public Account applyToAccount(Account account, Transaction t) {
        BigDecimal amount = t.amount();
        BigDecimal balance = storeMoney(account.currentBalance().add(amount));
        BigDecimal cycleCredit = account.currentCycleCredit();
        BigDecimal cycleDebit = account.currentCycleDebit();
        if (amount.signum() >= 0) {
            cycleCredit = storeMoney(cycleCredit.add(amount));
        } else {
            cycleDebit = storeMoney(cycleDebit.add(amount));
        }
        Account updated = account.withBalances(balance, cycleCredit, cycleDebit);
        if (accounts.contains(updated.accountId())) {
            accounts.rewrite(updated);
        } else {
            // REWRITE ... INVALID KEY sets reason 109 which nothing reads; the transaction still posts.
            LOG.warn("account {} vanished before rewrite (COBOL reason 109, ignored by CBTRN02C)", updated.accountId());
        }
        return updated;
    }

    public KeyedRecordStore<Account> accounts() {
        return accounts;
    }

    public KeyedRecordStore<TransactionCategoryBalance> categoryBalances() {
        return categoryBalances;
    }

    private static BigDecimal storeMoney(BigDecimal v) {
        return com.carddemo.recordio.codec.CobolNumeric.truncate(v, Account.MONEY_INT_DIGITS, Account.MONEY_SCALE);
    }

    private static BigDecimal storeBalance(BigDecimal v) {
        return com.carddemo.recordio.codec.CobolNumeric.truncate(v,
                TransactionCategoryBalance.BALANCE_INT_DIGITS, TransactionCategoryBalance.BALANCE_SCALE);
    }
}
