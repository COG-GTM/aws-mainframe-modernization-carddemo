package com.carddemo.interest.service;

import com.carddemo.interest.domain.AccountId;
import com.carddemo.interest.domain.TransactionCategoryBalance;

import java.util.List;

/**
 * All consecutive category balances belonging to one account — the unit the interest cycle
 * accrues over.
 *
 * <p>CBACT04C detects account boundaries by comparing the current record's account id with
 * {@code WS-LAST-ACCT-NUM} ({@code app/cbl/CBACT04C.cbl:194}) while reading {@code TCATBALF}
 * sequentially in key order, so a group is a run of adjacent records, not a hash grouping.
 */
public record AccountBalanceGroup(AccountId accountId, List<TransactionCategoryBalance> balances) {
}
