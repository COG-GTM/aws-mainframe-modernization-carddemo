package com.carddemo.interest.service;

import com.carddemo.interest.domain.Account;
import com.carddemo.interest.domain.InterestTransaction;

import java.util.List;

/**
 * Outcome of one interest cycle: the transactions to append to the {@code TRANSACT} file and the
 * account images to rewrite into {@code ACCTFILE}, both in the order the COBOL job produces them.
 */
public record InterestAccrualResult(List<InterestTransaction> transactions,
                                    List<Account> updatedAccounts,
                                    int categoryBalancesProcessed) {
}
