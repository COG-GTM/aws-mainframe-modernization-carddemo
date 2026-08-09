package com.carddemo.interest.repository;

import com.carddemo.interest.domain.Account;
import com.carddemo.interest.domain.AccountId;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

/** Keyed in-memory view of an {@code ACCTFILE} dataset image. */
public final class InMemoryAccountRepository implements AccountRepository {

    private final Map<AccountId, Account> accountsById;

    public InMemoryAccountRepository(Collection<Account> accounts) {
        this.accountsById = accounts.stream().collect(Collectors.toMap(
                Account::id, Function.identity(), (first, second) -> first, LinkedHashMap::new));
    }

    @Override
    public Optional<Account> findById(AccountId accountId) {
        return Optional.ofNullable(accountsById.get(accountId));
    }

    /** All accounts in dataset order, used when rewriting the account master. */
    public Collection<Account> all() {
        return accountsById.values();
    }
}
