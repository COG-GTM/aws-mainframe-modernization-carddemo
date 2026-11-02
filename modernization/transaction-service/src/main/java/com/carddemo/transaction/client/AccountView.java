package com.carddemo.transaction.client;

import java.math.BigDecimal;

/** Subset of the account-service response that the transaction domain needs. */
public record AccountView(Long accountId, String activeStatus, BigDecimal currentBalance, String groupId) {
}
