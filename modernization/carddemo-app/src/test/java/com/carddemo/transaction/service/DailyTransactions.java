package com.carddemo.transaction.service;

import com.carddemo.common.cobol.CobolCharsets;
import com.carddemo.common.record.Copybooks;
import com.carddemo.common.record.FixedRecord;
import com.carddemo.common.record.RecordFiles;
import com.carddemo.transaction.domain.DailyTransaction;

import java.math.BigDecimal;
import java.nio.file.Path;

/** Test DALYTRAN records: record 1 of the oracle baseline input with selected fields replaced. */
public final class DailyTransactions {

    /** Card of account 00000000001 in {@code app/data/ASCII/cardxref.txt} (line 49). */
    public static final String CARD_OF_ACCOUNT_1 = "9680294154603697";

    private static final String BASE = RecordFiles.readFixed(
            Path.of("src/test/resources/golden/posttran/baseline/input/DALYTRAN"), 350, CobolCharsets.ASCII).get(0);

    private DailyTransactions() {
    }

    public static DailyTransaction of(String id, String cardNum, String amount, String origTs) {
        FixedRecord r = Copybooks.CVTRA06Y.parse(BASE)
                .set("DALYTRAN-ID", id)
                .set("DALYTRAN-CARD-NUM", cardNum)
                .set("DALYTRAN-AMT", new BigDecimal(amount))
                .set("DALYTRAN-ORIG-TS", origTs);
        return new DailyTransaction(r.toString());
    }

    public static DailyTransaction of(String id, String cardNum, String amount) {
        return of(id, cardNum, amount, "2022-06-10 19:27:53.000000");
    }
}
