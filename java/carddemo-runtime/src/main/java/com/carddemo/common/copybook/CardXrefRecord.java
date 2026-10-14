package com.carddemo.common.copybook;

import com.carddemo.runtime.cobol.AlphanumericField;
import com.carddemo.runtime.cobol.CobolRecord;
import com.carddemo.runtime.cobol.NumericField;
import com.carddemo.runtime.cobol.RecordLayout;

/**
 * CVACT03Y {@code CARD-XREF-RECORD} (50 bytes): one CARDXREF KSDS record (CICS file CCXREF), primary key =
 * XREF-CARD-NUM (offset 0, length 16); alternate index CXACAIX = XREF-ACCT-ID (offset 25, length 11,
 * NONUNIQUEKEY, XREFFILE.jcl {@code KEYS(11,25)}).
 */
public final class CardXrefRecord extends CobolRecord {

    private static final RecordLayout.Builder B = RecordLayout.builder("CARD-XREF-RECORD");

    /** XREF-CARD-NUM PIC X(16). */
    public static final AlphanumericField CARD_NUM = B.x("XREF-CARD-NUM", 16);
    /** XREF-CUST-ID PIC 9(09). */
    public static final NumericField CUST_ID = B.zoned("XREF-CUST-ID", 9, 0, false);
    /** XREF-ACCT-ID PIC 9(11). */
    public static final NumericField ACCT_ID = B.zoned("XREF-ACCT-ID", 11, 0, false);

    static {
        B.filler(14);
    }

    public static final RecordLayout LAYOUT = B.build();
    public static final int LENGTH = LAYOUT.length();
    public static final int KEY_OFFSET = 0;
    public static final int KEY_LENGTH = 16;
    public static final int ACCT_AIX_OFFSET = 25;
    public static final int ACCT_AIX_LENGTH = 11;

    /** New record in uninitialised WORKING-STORAGE state (LOW-VALUES). */
    public CardXrefRecord() {
        super(LENGTH);
    }

    public static CardXrefRecord of(byte[] bytes) {
        CardXrefRecord r = new CardXrefRecord();
        r.setBytes(bytes);
        return r;
    }

    public static CardXrefRecord of(String record) {
        CardXrefRecord r = new CardXrefRecord();
        r.move(record);
        return r;
    }
}
