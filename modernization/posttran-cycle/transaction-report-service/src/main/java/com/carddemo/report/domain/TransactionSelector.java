package com.carddemo.report.domain;

import com.carddemo.recordio.layout.Transaction;

import java.util.Comparator;
import java.util.List;

/**
 * TRANREPT.jcl STEP05R (SORT): {@code INCLUDE COND=(TRAN-PROC-DT(305,10) between start and end)},
 * {@code SORT FIELDS=(TRAN-CARD-NUM(263,16),A)}. Java's sort is stable, so ties keep their input
 * (TRAN-ID) order; DFSORT without {@code EQUALS} does not guarantee that (open question).
 */
public final class TransactionSelector {

    private TransactionSelector() {
    }

    public static List<Transaction> selectAndSort(List<Transaction> master, ReportDateRange sortRange) {
        return master.stream()
                .filter(t -> sortRange.includes(t.processingDate()))
                .sorted(Comparator.comparing(Transaction::cardNumber))
                .toList();
    }
}
