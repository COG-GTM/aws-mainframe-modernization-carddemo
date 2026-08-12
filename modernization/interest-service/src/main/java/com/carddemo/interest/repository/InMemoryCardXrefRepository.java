package com.carddemo.interest.repository;

import com.carddemo.interest.domain.AccountId;
import com.carddemo.interest.domain.CardXref;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;

/**
 * In-memory equivalent of the {@code CARDXREF} alternate index: the first cross-reference row in
 * card-number order wins for a given account.
 *
 * <p>Card-number order is an <em>assumption</em>, not observed behaviour. The {@code XREFFIL1}
 * path ({@code app/jcl/INTCALC.jcl:31-32}) is a {@code NONUNIQUEKEY} alternate index, and VSAM
 * returns duplicates in the order they were added to the index rather than in primary-key order;
 * the static export does not record that order. The shipped dataset holds one card per account, so
 * the choice is currently unobservable — see open question 5 of
 * {@code modernization/CBACT04C-logic-map.md}.
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
