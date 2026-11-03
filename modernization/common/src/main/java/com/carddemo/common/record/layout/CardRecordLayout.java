package com.carddemo.common.record.layout;

import com.carddemo.common.record.AlphanumericField;
import com.carddemo.common.record.NumericField;
import com.carddemo.common.record.RecordLayout;

/** {@code CVACT02Y} CARD-RECORD: CARDDATA (VSAM KSDS, key CARD-NUM, AIX on CARD-ACCT-ID, RECLN 150). */
public final class CardRecordLayout {

    private static final RecordLayout.Builder B = RecordLayout.builder("CVACT02Y", "CARD-RECORD");

    public static final AlphanumericField CARD_NUM = B.alpha("CARD-NUM", "X(16)");
    public static final NumericField CARD_ACCT_ID = B.numeric("CARD-ACCT-ID", "9(11)");
    public static final NumericField CARD_CVV_CD = B.numeric("CARD-CVV-CD", "9(03)");
    public static final AlphanumericField CARD_EMBOSSED_NAME = B.alpha("CARD-EMBOSSED-NAME", "X(50)");
    public static final AlphanumericField CARD_EXPIRAION_DATE = B.alpha("CARD-EXPIRAION-DATE", "X(10)");
    public static final AlphanumericField CARD_ACTIVE_STATUS = B.alpha("CARD-ACTIVE-STATUS", "X(01)");
    public static final AlphanumericField FILLER = B.alpha("FILLER", "X(59)");
    public static final RecordLayout LAYOUT = B.build(150);

    private CardRecordLayout() {
    }
}
