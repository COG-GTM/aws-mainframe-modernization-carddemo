package com.carddemo.common.record.layout;

import com.carddemo.common.record.AlphanumericField;
import com.carddemo.common.record.NumericField;
import com.carddemo.common.record.RecordLayout;

/** {@code CVACT03Y} CARD-XREF-RECORD: CARDXREF (VSAM KSDS, key XREF-CARD-NUM, AIX on XREF-ACCT-ID, RECLN 50). */
public final class CardXrefRecordLayout {

    private static final RecordLayout.Builder B = RecordLayout.builder("CVACT03Y", "CARD-XREF-RECORD");

    public static final AlphanumericField XREF_CARD_NUM = B.alpha("XREF-CARD-NUM", "X(16)");
    public static final NumericField XREF_CUST_ID = B.numeric("XREF-CUST-ID", "9(09)");
    public static final NumericField XREF_ACCT_ID = B.numeric("XREF-ACCT-ID", "9(11)");
    public static final AlphanumericField FILLER = B.alpha("FILLER", "X(14)");
    public static final RecordLayout LAYOUT = B.build(50);

    private CardXrefRecordLayout() {
    }
}
