package com.carddemo.recordio.layout;

/** {@code app/cpy/CVACT03Y.cpy CARD-XREF-RECORD}, RECLN 50: card number to customer/account. */
public record CardXref(String cardNumber, long customerId, String accountId) {

    public static final int LENGTH = 50;
}
