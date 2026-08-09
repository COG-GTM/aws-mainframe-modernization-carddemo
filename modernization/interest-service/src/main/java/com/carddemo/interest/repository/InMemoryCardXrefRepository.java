package com.carddemo.interest.repository;

import com.carddemo.interest.domain.AccountId;
import com.carddemo.interest.domain.CardXref;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;

/**
 * In-memory equivalent of the {@code CARDXREF} alternate index: the first cross-reference row in
 * card-number order wins for a given account, matching a VSAM alternate-key read.
 */
public final class InMemoryCardXrefRepository implements CardXrefRepository {

    private final Map<AccountId, CardXref> firstXrefByAccount = new LinkedHashMap<>();

    public InMemoryCardXrefRepository(Collection<CardXref> xrefs) {
        xrefs.stream()
                .sorted((left, right) -> left.cardNumber().compareTo(right.cardNumber()))
                .forEach(xref -> firstXrefByAccount.putIfAbsent(xref.accountId(), xref));
    }

    @Override
    public Optional<CardXref> findByAccountId(AccountId accountId) {
        return Optional.ofNullable(firstXrefByAccount.get(accountId));
    }
}
