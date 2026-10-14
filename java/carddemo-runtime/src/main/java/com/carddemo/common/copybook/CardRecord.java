package com.carddemo.common.copybook;

import com.carddemo.runtime.cobol.AlphanumericField;
import com.carddemo.runtime.cobol.CobolRecord;
import com.carddemo.runtime.cobol.NumericField;
import com.carddemo.runtime.cobol.RecordLayout;

/**
 * CVACT02Y {@code CARD-RECORD} (150 bytes): one CARDDAT KSDS record, key = CARD-NUM (offset 0, length 16).
 * The CARDAIX path (alternate index on CARD-ACCT-ID, non-unique) is at offset 16, length 11.
 */
public final class CardRecord extends CobolRecord {

    private static final RecordLayout.Builder B = RecordLayout.builder("CARD-RECORD");

    /** {@code CARD-NUM} PIC X(16). */
    public static final AlphanumericField NUM = B.x("CARD-NUM", 16);
    /** {@code CARD-ACCT-ID} PIC 9(11). */
    public static final NumericField ACCT_ID = B.zoned("CARD-ACCT-ID", 11, 0, false);
    /** {@code CARD-CVV-CD} PIC 9(03). */
    public static final NumericField CVV_CD = B.zoned("CARD-CVV-CD", 3, 0, false);
    /** {@code CARD-EMBOSSED-NAME} PIC X(50). */
    public static final AlphanumericField EMBOSSED_NAME = B.x("CARD-EMBOSSED-NAME", 50);
    /** {@code CARD-EXPIRAION-DATE} PIC X(10) (sic). */
    public static final AlphanumericField EXPIRAION_DATE = B.x("CARD-EXPIRAION-DATE", 10);
    /** {@code CARD-ACTIVE-STATUS} PIC X(01). */
    public static final AlphanumericField ACTIVE_STATUS = B.x("CARD-ACTIVE-STATUS", 1);

    static {
        B.filler(59);
    }

    public static final RecordLayout LAYOUT = B.build();
    public static final int LENGTH = LAYOUT.length();
    public static final int KEY_OFFSET = 0;
    public static final int KEY_LENGTH = 16;
    /** CARDAIX alternate key = CARD-ACCT-ID. */
    public static final int ACCT_ID_OFFSET = 16;
    public static final int ACCT_ID_LENGTH = 11;

    /** New record in uninitialised WORKING-STORAGE state (LOW-VALUES). */
    public CardRecord() {
        super(LENGTH);
    }

    public static CardRecord of(byte[] bytes) {
        CardRecord r = new CardRecord();
        r.setBytes(bytes);
        return r;
    }

    public static CardRecord of(String record) {
        CardRecord r = new CardRecord();
        r.move(record);
        return r;
    }

    public String cardNum() {
        return NUM.get(this);
    }

    /** Raw 11 characters of CARD-ACCT-ID. */
    public String acctIdRaw() {
        return ACCT_ID.raw(this);
    }

    public String embossedName() {
        return EMBOSSED_NAME.get(this);
    }

    public String expirationDate() {
        return EXPIRAION_DATE.get(this);
    }

    public String activeStatus() {
        return ACTIVE_STATUS.get(this);
    }
}
