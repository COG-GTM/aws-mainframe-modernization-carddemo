package com.carddemo.programs.cotrtupc.copybook;

import com.carddemo.programs.cotrtupc.support.CobolText;

/**
 * DCLTRCAT - DCLTRANSACTION-TYPE-CATEGORY host variables for CARDDEMO.TRANSACTION_TYPE_CATEGORY
 * (TRC_TYPE_CODE CHAR(2), TRC_TYPE_CATEGORY CHAR(4), TRC_CAT_DATA VARCHAR(50); FK to TRANSACTION_TYPE
 * ON DELETE RESTRICT). Included by COTRTUPC but only the FK matters (SQLCODE -532 on delete).
 */
public final class DclTransactionTypeCategory {

    /** DCL-TRC-TYPE-CODE PIC X(2). */
    public String trcTypeCode = CobolText.lowValues(2);
    /** DCL-TRC-TYPE-CATEGORY PIC X(4). */
    public String trcTypeCategory = CobolText.lowValues(4);
    /** DCL-TRC-CAT-DATA-LEN PIC S9(4) COMP. */
    public short trcCatDataLen;
    /** DCL-TRC-CAT-DATA-TEXT PIC X(50). */
    public String trcCatDataText = CobolText.lowValues(50);

    public static DclTransactionTypeCategory of(String typeCode, String category, String data) {
        DclTransactionTypeCategory c = new DclTransactionTypeCategory();
        c.trcTypeCode = CobolText.pic(typeCode, 2);
        c.trcTypeCategory = CobolText.pic(category, 4);
        String d = data.length() > 50 ? data.substring(0, 50) : data;
        c.trcCatDataLen = (short) d.length();
        c.trcCatDataText = CobolText.pic(d, 50);
        return c;
    }
}
