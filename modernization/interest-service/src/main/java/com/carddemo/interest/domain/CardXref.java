package com.carddemo.interest.domain;

/**
 * Cross-reference row linking a card number to its account and customer.
 *
 * <p>Source copybook {@code CVACT03Y} ({@code app/cpy/CVACT03Y.cpy:4-8}). The interest batch needs
 * it only to stamp a card number onto the generated interest transaction
 * ({@code app/cbl/CBACT04C.cbl:495}); it is read through the account-id alternate index
 * ({@code app/cbl/CBACT04C.cbl:38}, {@code app/cbl/CBACT04C.cbl:393-398}).
 */
public record CardXref(String cardNumber, String customerId, AccountId accountId) {
}
