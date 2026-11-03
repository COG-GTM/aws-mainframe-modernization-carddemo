package com.carddemo.account.service;

import com.carddemo.account.domain.Account;
import java.math.BigDecimal;

/** Outcome of {@link AccountService#post}, mirroring the CBTRN02C validation reason codes. */
public record PostingOutcome(
        boolean posted,
        Integer reasonCode,
        String reasonDescription,
        BigDecimal currentBalance,
        BigDecimal currentCycleCredit,
        BigDecimal currentCycleDebit) {

    public static PostingOutcome posted(Account account) {
        return new PostingOutcome(true, null, null, account.getCurrBal(),
                account.getCurrCycCredit(), account.getCurrCycDebit());
    }

    public static PostingOutcome rejected(Account account, int reasonCode, String description) {
        return new PostingOutcome(false, reasonCode, description, account.getCurrBal(),
                account.getCurrCycCredit(), account.getCurrCycDebit());
    }
}
