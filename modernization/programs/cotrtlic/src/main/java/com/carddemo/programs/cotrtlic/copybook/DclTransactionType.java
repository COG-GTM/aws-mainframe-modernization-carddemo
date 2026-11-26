package com.carddemo.programs.cotrtlic.copybook;

import com.carddemo.programs.cotrtlic.support.Cob;

/**
 * DCLGEN DCLTRTYP: table CARDDEMO.TRANSACTION_TYPE (TR_TYPE CHAR(2) NOT NULL, TR_DESCRIPTION
 * VARCHAR(50) NOT NULL) and host structure 01 DCLTRANSACTION-TYPE.
 *
 * @param trType          DCL-TR-TYPE PIC X(2)
 * @param descriptionLen  DCL-TR-DESCRIPTION-LEN PIC S9(4) COMP
 * @param descriptionText DCL-TR-DESCRIPTION-TEXT PIC X(50)
 */
public record DclTransactionType(String trType, int descriptionLen, String descriptionText) {

    public static final int TR_TYPE_LEN = 2;
    public static final int TR_DESCRIPTION_MAX = 50;

    /** INITIALIZE DCLTRANSACTION-TYPE. */
    public static DclTransactionType initialized() {
        return new DclTransactionType(Cob.spaces(2), 0, Cob.spaces(50));
    }

    /** Host structure after a FETCH of a VARCHAR value into a previously INITIALIZEd area. */
    public static DclTransactionType fetched(String trType, String description) {
        String d = description.length() > 50 ? description.substring(0, 50) : description;
        return new DclTransactionType(Cob.move(trType, 2), d.length(), Cob.move(d, 50));
    }

    /** The VARCHAR value as DB2 sees it: the first DCL-TR-DESCRIPTION-LEN bytes of the text. */
    public String varcharValue() {
        int len = Math.max(0, Math.min(descriptionLen, descriptionText.length()));
        return descriptionText.substring(0, len);
    }
}
