package com.carddemo.online.cocrdlic;

import com.carddemo.runtime.cobol.AlphanumericField;
import com.carddemo.runtime.cobol.CobolRecord;
import com.carddemo.runtime.cobol.NumericField;
import com.carddemo.runtime.cobol.RecordLayout;

/** CVCRD01Y {@code CC-WORK-AREAS} / {@code CC-WORK-AREA} (213 bytes), program-private copy for COCRDLIC. */
final class CardWorkArea extends CobolRecord {

    private static final RecordLayout.Builder B = RecordLayout.builder("CC-WORK-AREA");

    /** {@code CCARD-AID PIC X(5)} + 88 CCARD-AID-ENTER .. CCARD-AID-PFK12. */
    static final AlphanumericField CCARD_AID = B.x("CCARD-AID", 5);
    static final AlphanumericField CCARD_NEXT_PROG = B.x("CCARD-NEXT-PROG", 8);
    static final AlphanumericField CCARD_NEXT_MAPSET = B.x("CCARD-NEXT-MAPSET", 7);
    static final AlphanumericField CCARD_NEXT_MAP = B.x("CCARD-NEXT-MAP", 7);
    static final AlphanumericField CCARD_ERROR_MSG = B.x("CCARD-ERROR-MSG", 75);
    static final AlphanumericField CCARD_RETURN_MSG = B.x("CCARD-RETURN-MSG", 75);
    static final AlphanumericField CC_ACCT_ID = B.x("CC-ACCT-ID", 11);

    static {
        B.redefines(CC_ACCT_ID);
    }

    static final NumericField CC_ACCT_ID_N = B.zoned("CC-ACCT-ID-N", 11, 0, false);

    static {
        B.endRedefines(CC_ACCT_ID);
    }

    static final AlphanumericField CC_CARD_NUM = B.x("CC-CARD-NUM", 16);

    static {
        B.redefines(CC_CARD_NUM);
    }

    static final NumericField CC_CARD_NUM_N = B.zoned("CC-CARD-NUM-N", 16, 0, false);

    static {
        B.endRedefines(CC_CARD_NUM);
    }

    static final AlphanumericField CC_CUST_ID = B.x("CC-CUST-ID", 9);

    static {
        B.redefines(CC_CUST_ID);
    }

    static final NumericField CC_CUST_ID_N = B.zoned("CC-CUST-ID-N", 9, 0, false);

    static {
        B.endRedefines(CC_CUST_ID);
    }

    static final RecordLayout LAYOUT = B.build();

    /** State after {@code INITIALIZE CC-WORK-AREA}: every (non-REDEFINES) item is PIC X, so all SPACES. */
    CardWorkArea() {
        super(LAYOUT.length());
        initializeSpaces();
    }

    String aid() {
        return CCARD_AID.get(this);
    }

    void setAid(String v) {
        CCARD_AID.set(this, v);
    }

    boolean aidIs(String v) {
        return CCARD_AID.get(this).equals(v);
    }

    String acctId() {
        return CC_ACCT_ID.get(this);
    }

    String cardNum() {
        return CC_CARD_NUM.get(this);
    }
}
