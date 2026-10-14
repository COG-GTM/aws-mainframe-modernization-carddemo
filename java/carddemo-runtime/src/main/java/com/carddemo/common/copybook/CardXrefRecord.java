package com.carddemo.common.copybook;

import com.carddemo.runtime.cobol.AlphanumericField;
import com.carddemo.runtime.cobol.CobolRecord;
import com.carddemo.runtime.cobol.NumericField;
import com.carddemo.runtime.cobol.RecordLayout;
import java.util.List;

/**
 * CVACT03Y {@code CARD-XREF-RECORD} (RECLN 50): one CCXREF KSDS record, key = XREF-CARD-NUM (offset 0, length 16);
 * alternate index (path CXACAIX) on XREF-ACCT-ID (offset 25, length 11, NONUNIQUEKEY per XREFFILE.jcl).
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
    /** CSD: DEFINE FILE(CCXREF) DSNAME(AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS). */
    public static final String FILE_NAME = "CCXREF";
    public static final String DSNAME = "AWS.M2.CARDDEMO.CARDXREF.VSAM.KSDS";
    /** CSD: DEFINE FILE(CXACAIX) DSNAME(AWS.M2.CARDDEMO.CARDXREF.VSAM.AIX.PATH). */
    public static final String ACCT_PATH_NAME = "CXACAIX";
    public static final String SEED_RESOURCE = "carddemo/data/ASCII/cardxref.txt";

    /** New record in uninitialised WORKING-STORAGE state (LOW-VALUES). */
    public CardXrefRecord() {
        super(LENGTH);
    }

    public static CardXrefRecord of(byte[] bytes) {
        CardXrefRecord r = new CardXrefRecord();
        r.setBytes(bytes);
        return r;
    }

    /** The 50 cross references of app/data/ASCII/cardxref.txt. */
    public static List<String> seed() {
        return AsciiSeed.records(SEED_RESOURCE, LENGTH);
    }
}
