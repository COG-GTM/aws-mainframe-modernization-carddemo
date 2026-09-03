package com.carddemo.posting.service;

import com.carddemo.posting.domain.DailyTransaction;
import com.carddemo.posting.domain.PostedTransaction;
import com.carddemo.posting.domain.RejectedTransaction;
import com.carddemo.posting.domain.TransactionCategoryBalance;
import com.carddemo.posting.domain.TransactionCategoryKey;
import com.carddemo.posting.repository.AccountRepository;
import com.carddemo.posting.repository.TransactionCategoryBalanceRepository;
import com.carddemo.posting.rules.PostedTransactionFactory;
import com.carddemo.posting.rules.TransactionValidator;
import com.carddemo.posting.rules.ValidationOutcome;

import java.util.ArrayList;
import java.util.List;

/**
 * Posts a day's transactions: the business core of {@code CBTRN02C}
 * ({@code app/cbl/CBTRN02C.cbl:202-219}).
 *
 * <p>The service knows nothing about EBCDIC, record layouts or datasets — it consumes decoded
 * transactions and talks to repositories — and nothing about ordering beyond the one thing that
 * matters: transactions are applied in the order the daily file presents them, because each one
 * sees the balances left by its predecessors.
 */
public final class DailyTransactionPostingService {

    private final TransactionValidator validator;
    private final PostedTransactionFactory postedTransactions;
    private final AccountRepository accounts;
    private final TransactionCategoryBalanceRepository categoryBalances;

    public DailyTransactionPostingService(TransactionValidator validator,
                                          PostedTransactionFactory postedTransactions,
                                          AccountRepository accounts,
                                          TransactionCategoryBalanceRepository categoryBalances) {
        this.validator = validator;
        this.postedTransactions = postedTransactions;
        this.accounts = accounts;
        this.categoryBalances = categoryBalances;
    }

    /** Validates and posts every transaction, in file order. */
    public PostingResult post(List<DailyTransaction> dailyTransactions) {
        List<PostedTransaction> posted = new ArrayList<>();
        List<RejectedTransaction> rejected = new ArrayList<>();
        for (DailyTransaction transaction : dailyTransactions) {
            switch (validator.validate(transaction)) {
                case ValidationOutcome.Accepted accepted -> {
                    accumulateCategoryBalance(transaction, accepted);
                    accounts.save(accepted.account().withTransactionPosted(transaction.amount()));
                    posted.add(postedTransactions.post(transaction));
                }
                case ValidationOutcome.Rejected(var reason) ->
                        rejected.add(new RejectedTransaction(transaction, reason));
            }
        }
        return new PostingResult(dailyTransactions.size(), posted, rejected);
    }

    /**
     * Adds the amount to the account's bucket for this transaction type and category, opening the
     * bucket if the account has never had activity of that kind.
     *
     * <p>Business rules BR-8 and BR-9, COBOL paragraphs {@code 2700-UPDATE-TCATBAL} and its
     * create/update branches ({@code app/cbl/CBTRN02C.cbl:467-542}). Note the key is built from
     * the cross-referenced account, not from anything on the transaction itself
     * ({@code app/cbl/CBTRN02C.cbl:469}).
     */
    private void accumulateCategoryBalance(DailyTransaction transaction,
                                           ValidationOutcome.Accepted accepted) {
        TransactionCategoryKey key = new TransactionCategoryKey(
                accepted.xref().accountId(), transaction.typeCode(), transaction.categoryCode());
        TransactionCategoryBalance balance = categoryBalances.findByKey(key)
                .orElseGet(() -> TransactionCategoryBalance.opened(key));
        categoryBalances.save(balance.withAmountAdded(transaction.amount()));
    }
}
