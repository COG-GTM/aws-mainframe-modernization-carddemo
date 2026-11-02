package com.carddemo.transaction.client;

import java.util.Optional;

/**
 * Card domain seen from the transaction domain. On the mainframe these were VSAM reads of
 * CARDXREF from the same program; across services they are REST calls.
 */
public interface CardGateway {

    Optional<CardXrefView> xrefByCard(String cardNumber);

    Optional<CardXrefView> xrefByAccount(long accountId);
}
