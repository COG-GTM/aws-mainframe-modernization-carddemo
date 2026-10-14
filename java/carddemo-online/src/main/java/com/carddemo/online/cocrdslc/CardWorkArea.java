package com.carddemo.online.cocrdslc;

import com.carddemo.runtime.cobol.AlphanumericField;
import com.carddemo.runtime.cobol.CobolRecord;
import com.carddemo.runtime.cobol.NumericField;
import com.carddemo.runtime.cobol.RecordLayout;

/** CVCRD01Y {@code CC-WORK-AREAS} / {@code CC-WORK-AREA} (204 bytes): card-screen work fields. */
final class CardWorkArea extends CobolRecord {

    private static final RecordLayout.Builder B = RecordLayout.builder("CC-WORK-AREA");

    /** CCARD-AID PIC X(5). */
    static final AlphanumericField CCARD_AID = B.x("CCARD-AID", 5);
    /** CCARD-NEXT-PROG PIC X(8). */
    static final AlphanumericField CCARD_NEXT_PROG = B.x("CCARD-NEXT-PROG", 8);
    /** CCARD-NEXT-MAPSET PIC X(7). */
    static final AlphanumericField CCARD_NEXT_MAPSET = B.x("CCARD-NEXT-MAPSET", 7);
    /** CCARD-NEXT-MAP PIC X(7). */
    static final AlphanumericField CCARD_NEXT_MAP = B.x("CCARD-NEXT-MAP", 7);
    /** CCARD-ERROR-MSG PIC X(75). */
    static final AlphanumericField CCARD_ERROR_MSG = B.x("CCARD-ERROR-MSG", 75);
    /** CCARD-RETURN-MSG PIC X(75). */
    static final AlphanumericField CCARD_RETURN_MSG = B.x("CCARD-RETURN-MSG", 75);
    /** CC-ACCT-ID PIC X(11). */
    static final AlphanumericField CC_ACCT_ID = B.x("CC-ACCT-ID", 11);
    /** CC-ACCT-ID-N REDEFINES CC-ACCT-ID PIC 9(11). */
    static final NumericField CC_ACCT_ID_N;
    /** CC-CARD-NUM PIC X(16). */
    static final AlphanumericField CC_CARD_NUM;
    /** CC-CARD-NUM-N REDEFINES CC-CARD-NUM PIC 9(16). */
    static final NumericField CC_CARD_NUM_N;
    /** CC-CUST-ID PIC X(09). */
    static final AlphanumericField CC_CUST_ID;
    /** CC-CUST-ID-N REDEFINES CC-CUST-ID PIC 9(9). */
    static final NumericField CC_CUST_ID_N;

    static {
        B.redefines(CC_ACCT_ID);
        CC_ACCT_ID_N = B.zoned("CC-ACCT-ID-N", 11, 0, false);
        B.endRedefines(CC_ACCT_ID);
        CC_CARD_NUM = B.x("CC-CARD-NUM", 16);
        B.redefines(CC_CARD_NUM);
        CC_CARD_NUM_N = B.zoned("CC-CARD-NUM-N", 16, 0, false);
        B.endRedefines(CC_CARD_NUM);
        CC_CUST_ID = B.x("CC-CUST-ID", 9);
        B.redefines(CC_CUST_ID);
        CC_CUST_ID_N = B.zoned("CC-CUST-ID-N", 9, 0, false);
        B.endRedefines(CC_CUST_ID);
    }

    static final RecordLayout LAYOUT = B.build();
    static final int LENGTH = LAYOUT.length();

    CardWorkArea() {
        super(LENGTH);
        initializeSpaces();
    }

    String ccardAid() {
        return CCARD_AID.get(this);
    }

    boolean isCcardAidEnter() {
        return "ENTER".equals(ccardAid());
    }

    boolean isCcardAidPfk03() {
        return "PFK03".equals(ccardAid());
    }

    String ccAcctId() {
        return CC_ACCT_ID.get(this);
    }

    String ccCardNum() {
        return CC_CARD_NUM.get(this);
    }
}
