package com.carddemo.interest.model;

import com.carddemo.interest.CobolDecimal;
import java.math.BigDecimal;

/**
 * Copybook CVACT01Y — account entity (RECLN 300). Only the fields CBACT04C reads or rewrites are
 * modelled; the remaining fields are carried through unchanged by the repository implementation.
 *
 * <pre>
 * 05 ACCT-ID               PIC 9(11)
 * 05 ACCT-CURR-BAL         PIC S9(10)V99
 * 05 ACCT-CURR-CYC-CREDIT  PIC S9(10)V99
 * 05 ACCT-CURR-CYC-DEBIT   PIC S9(10)V99
 * 05 ACCT-GROUP-ID         PIC X(10)
 * </pre>
 */
public record AccountRecord(
        String acctId,
        BigDecimal currentBalance,
        BigDecimal currentCycleCredit,
        BigDecimal currentCycleDebit,
        String groupId) {

    public AccountRecord {
        acctId = CobolDecimal.zoned(acctId, 11);
        currentBalance = CobolDecimal.toBalance(currentBalance);
        currentCycleCredit = CobolDecimal.toBalance(currentCycleCredit);
        currentCycleDebit = CobolDecimal.toBalance(currentCycleDebit);
        groupId = CobolDecimal.alphanumeric(groupId, 10);
    }
}
