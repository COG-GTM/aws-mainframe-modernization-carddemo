package com.carddemo.interestcalc.program;

import com.carddemo.interestcalc.copybook.TransactionRecord;
import java.util.List;

/**
 * The outputs of one CBACT04C job step.
 *
 * @param transactions the TRANSACT records written by {@code 1300-B-WRITE-TX}, in write order
 * @param accountMasterAfter the ACCTFILE master after the run, in ascending key order, as
 *                           350/300 byte fixed-width images
 * @param recordsRead {@code WS-RECORD-COUNT}
 * @param console everything the program {@code DISPLAY}ed, in order
 */
public record InterestCalculationResult(List<TransactionRecord> transactions, List<String> accountMasterAfter,
                                        long recordsRead, List<String> console) {

    /** The TRANSACT file image: one 350-byte record per entry. */
    public List<String> transactionRecordImages() {
        return transactions.stream().map(TransactionRecord::format).toList();
    }
}
