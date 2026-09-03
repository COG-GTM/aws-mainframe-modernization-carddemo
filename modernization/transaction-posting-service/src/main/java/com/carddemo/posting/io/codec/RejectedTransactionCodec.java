package com.carddemo.posting.io.codec;

import com.carddemo.mainframe.io.RecordLayout;
import com.carddemo.mainframe.io.layout.CardDemoLayouts;
import com.carddemo.posting.domain.RejectedTransaction;

import java.math.BigDecimal;

/**
 * Maps {@link RejectedTransaction} instances onto the 430-byte {@code DALYREJS} record
 * ({@code app/cbl/CBTRN02C.cbl:176-182}, {@code app/jcl/POSTTRAN.jcl:35-39}).
 *
 * <p>The rejected record is the daily transaction copied byte for byte, so the reject file can be
 * re-driven through the job once the underlying data problem is fixed, followed by the validation
 * trailer that says why it failed.
 */
public final class RejectedTransactionCodec {

    private static final RecordLayout LAYOUT = CardDemoLayouts.REJECTED_TRANSACTION;

    private RejectedTransactionCodec() {
    }

    public static int recordLength() {
        return LAYOUT.recordLength();
    }

    public static byte[] encode(RejectedTransaction rejected) {
        byte[] record = LAYOUT.blankRecord();
        LAYOUT.putRaw(record, "dailyTransaction",
                DailyTransactionCodec.encode(rejected.transaction()));
        LAYOUT.putDecimal(record, "reasonCode", BigDecimal.valueOf(rejected.reason().code()));
        LAYOUT.putText(record, "reasonDescription", rejected.reason().description());
        return record;
    }
}
