package com.carddemo.interest.service;

import com.carddemo.interest.domain.TransactionCategoryBalance;

import java.util.ArrayList;
import java.util.List;

/** Splits the key-ordered category-balance stream into per-account groups. */
public final class AccountBalanceGrouping {

    private AccountBalanceGrouping() {
    }

    /**
     * Business rule BR-1 — account-break detection over the {@code TCATBALF} sequence.
     *
     * <p>COBOL main loop ({@code app/cbl/CBACT04C.cbl:188-222}): records are read in ascending
     * key order and a change of {@code TRANCAT-ACCT-ID} versus {@code WS-LAST-ACCT-NUM} closes the
     * previous account and opens the next one.
     */
    public static List<AccountBalanceGroup> groupConsecutively(List<TransactionCategoryBalance> balances) {
        List<AccountBalanceGroup> groups = new ArrayList<>();
        List<TransactionCategoryBalance> current = new ArrayList<>();
        for (TransactionCategoryBalance balance : balances) {
            if (!current.isEmpty() && !current.get(0).accountId().equals(balance.accountId())) {
                groups.add(new AccountBalanceGroup(current.get(0).accountId(), List.copyOf(current)));
                current.clear();
            }
            current.add(balance);
        }
        if (!current.isEmpty()) {
            groups.add(new AccountBalanceGroup(current.get(0).accountId(), List.copyOf(current)));
        }
        return groups;
    }
}
