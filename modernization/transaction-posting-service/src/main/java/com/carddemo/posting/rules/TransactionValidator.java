package com.carddemo.posting.rules;

import com.carddemo.posting.domain.Account;
import com.carddemo.posting.domain.CardXref;
import com.carddemo.posting.domain.DailyTransaction;
import com.carddemo.posting.domain.RejectReason;
import com.carddemo.posting.repository.AccountRepository;
import com.carddemo.posting.repository.CardXrefRepository;

import java.util.Optional;

/**
 * Decides whether a daily transaction can be posted.
 *
 * <p>Business rules BR-2 to BR-5, COBOL paragraphs {@code 1500-VALIDATE-TRAN},
 * {@code 1500-A-LOOKUP-XREF} and {@code 1500-B-LOOKUP-ACCT}
 * ({@code app/cbl/CBTRN02C.cbl:370-422}).
 */
public final class TransactionValidator {

    private final CardXrefRepository cardXrefs;
    private final AccountRepository accounts;

    public TransactionValidator(CardXrefRepository cardXrefs, AccountRepository accounts) {
        this.cardXrefs = cardXrefs;
        this.accounts = accounts;
    }

    /**
     * Runs the validation chain.
     *
     * <p>The two lookups short-circuit: an unknown card never reaches the account lookup
     * ({@code app/cbl/CBTRN02C.cbl:371-376}). The two account-level checks do not — the COBOL runs
     * the expiry check even after the credit-limit check has already failed, and the later
     * {@code MOVE} overwrites the earlier reason, so an over-limit transaction on an expired
     * account is reported as expired (103), not over limit (102). That precedence is preserved
     * here; see BR-6a.
     */
    public ValidationOutcome validate(DailyTransaction transaction) {
        Optional<CardXref> xref = cardXrefs.findByCardNumber(transaction.cardNumber());
        if (xref.isEmpty()) {
            return new ValidationOutcome.Rejected(RejectReason.INVALID_CARD_NUMBER);
        }
        Optional<Account> account = accounts.findById(xref.get().accountId());
        if (account.isEmpty()) {
            return new ValidationOutcome.Rejected(RejectReason.ACCOUNT_NOT_FOUND);
        }
        RejectReason reason = null;
        if (!CreditLimitRule.permits(account.get(), transaction.amount())) {
            reason = RejectReason.OVER_LIMIT;
        }
        if (AccountExpiryRule.expiredFor(account.get(), transaction)) {
            reason = RejectReason.ACCOUNT_EXPIRED;
        }
        return reason == null
                ? new ValidationOutcome.Accepted(xref.get(), account.get())
                : new ValidationOutcome.Rejected(reason);
    }
}
