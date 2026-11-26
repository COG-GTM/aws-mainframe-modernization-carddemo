package com.carddemo.programs.cobtupdt.copybook;

import java.util.Objects;

/**
 * DCLGEN host structure for CARDDEMO.TRANSACTION_TYPE
 * (app/app-transaction-type-db2/dcl/DCLTRTYP.dcl, {@code EXEC SQL INCLUDE DCLTRTYP}).
 *
 * <pre>
 * 01  DCLTRANSACTION-TYPE.
 *     10 DCL-TR-TYPE                 PIC X(2).          TR_TYPE        CHAR(2)     NOT NULL
 *     10 DCL-TR-DESCRIPTION.                            TR_DESCRIPTION VARCHAR(50) NOT NULL
 *        49 DCL-TR-DESCRIPTION-LEN   PIC S9(4) COMP.
 *        49 DCL-TR-DESCRIPTION-TEXT  PIC X(50).
 * </pre>
 *
 * COBTUPDT includes this structure but binds its own INPUT-REC-* fields as host variables.
 */
public record DclTransactionType(String trType, short trDescriptionLen, String trDescriptionText) {

    public static final String TABLE_NAME = "CARDDEMO.TRANSACTION_TYPE";
    public static final String COL_TR_TYPE = "TR_TYPE";
    public static final String COL_TR_DESCRIPTION = "TR_DESCRIPTION";

    public static final int TR_TYPE_LENGTH = 2;
    public static final int TR_DESCRIPTION_MAX_LENGTH = 50;

    public DclTransactionType {
        Objects.requireNonNull(trType, "trType");
        Objects.requireNonNull(trDescriptionText, "trDescriptionText");
        trType = Pic.alphanumeric(trType, TR_TYPE_LENGTH);
        trDescriptionText = Pic.alphanumeric(trDescriptionText, TR_DESCRIPTION_MAX_LENGTH);
        if (trDescriptionLen < 0 || trDescriptionLen > TR_DESCRIPTION_MAX_LENGTH) {
            throw new IllegalArgumentException("DCL-TR-DESCRIPTION-LEN out of range 0..50: " + trDescriptionLen);
        }
    }

    /** Builds the host structure from a column value the way DB2 fills a VARCHAR host variable. */
    public static DclTransactionType of(String trType, String trDescription) {
        String text = trDescription.length() > TR_DESCRIPTION_MAX_LENGTH
                ? trDescription.substring(0, TR_DESCRIPTION_MAX_LENGTH)
                : trDescription;
        return new DclTransactionType(trType, (short) text.length(), text);
    }

    /** The VARCHAR value: the first DCL-TR-DESCRIPTION-LEN bytes of DCL-TR-DESCRIPTION-TEXT. */
    public String trDescription() {
        return trDescriptionText.substring(0, trDescriptionLen);
    }
}
