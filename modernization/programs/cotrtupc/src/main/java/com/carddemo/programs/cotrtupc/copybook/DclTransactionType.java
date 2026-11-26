package com.carddemo.programs.cotrtupc.copybook;

import com.carddemo.programs.cotrtupc.support.CobolText;

/**
 * DCLTRTYP - DCLTRANSACTION-TYPE host variables for CARDDEMO.TRANSACTION_TYPE
 * (TR_TYPE CHAR(2) NOT NULL, TR_DESCRIPTION VARCHAR(50) NOT NULL).
 */
public final class DclTransactionType {

    public static final int TYPE_LENGTH = 2;
    public static final int DESCRIPTION_MAX = 50;

    /** DCL-TR-TYPE PIC X(2). */
    public String trType = CobolText.lowValues(TYPE_LENGTH);
    /** DCL-TR-DESCRIPTION-LEN PIC S9(4) COMP. */
    public short trDescriptionLen;
    /** DCL-TR-DESCRIPTION-TEXT PIC X(50). */
    public String trDescriptionText = CobolText.lowValues(DESCRIPTION_MAX);

    public void trType(String v) { trType = CobolText.pic(v, TYPE_LENGTH); }
    public void trDescriptionText(String v) { trDescriptionText = CobolText.pic(v, DESCRIPTION_MAX); }

    /** The VARCHAR value as DB2 sees it: TEXT(1:LEN). */
    public String varcharValue() {
        int len = Math.max(0, Math.min(trDescriptionLen, DESCRIPTION_MAX));
        return trDescriptionText.substring(0, len);
    }
}
