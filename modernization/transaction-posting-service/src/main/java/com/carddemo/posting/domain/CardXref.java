package com.carddemo.posting.domain;

/**
 * A card cross-reference record ({@code CVACT03Y}, {@code app/cpy/CVACT03Y.cpy:4-8}): the mapping
 * from a card number to the customer and account it belongs to.
 *
 * <p>This is the relationship the posting run resolves first, because nothing about a daily
 * transaction other than the card number identifies the account
 * ({@code app/cbl/CBTRN02C.cbl:380-392}).
 */
public record CardXref(CardNumber cardNumber, String customerId, AccountId accountId) {
}
