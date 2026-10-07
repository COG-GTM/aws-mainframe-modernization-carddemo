package com.carddemo.batch.record;

import com.carddemo.batch.codec.Field;
import com.carddemo.batch.codec.FixedWidth;
import com.carddemo.batch.codec.Layout;

/**
 * Copybook {@code CVACT02Y} - {@code CARD-RECORD} (RECLN 150), the CARDFILE KSDS record (key CARD-NUM).
 * <pre>
 * 05  CARD-NUM                    PIC X(16).
 * 05  CARD-ACCT-ID                PIC 9(11).
 * 05  CARD-CVV-CD                 PIC 9(03).
 * 05  CARD-EMBOSSED-NAME          PIC X(50).
 * 05  CARD-EXPIRAION-DATE         PIC X(10).
 * 05  CARD-ACTIVE-STATUS          PIC X(01).
 * 05  FILLER                      PIC X(59).
 * </pre>
 */
public final class CardRecord extends FixedWidthRecord {

    private static final Layout.Builder B = Layout.builder("CARD-RECORD");
    public static final Field CARD_NUM = B.text("CARD-NUM", 16);
    public static final Field CARD_ACCT_ID = B.unsigned("CARD-ACCT-ID", 11);
    public static final Field CARD_CVV_CD = B.unsigned("CARD-CVV-CD", 3);
    public static final Field CARD_EMBOSSED_NAME = B.text("CARD-EMBOSSED-NAME", 50);
    public static final Field CARD_EXPIRAION_DATE = B.text("CARD-EXPIRAION-DATE", 10);
    public static final Field CARD_ACTIVE_STATUS = B.text("CARD-ACTIVE-STATUS", 1);
    public static final Field FILLER = B.filler(59);
    public static final Layout LAYOUT = B.build();
    public static final int LENGTH = LAYOUT.length();

    public CardRecord() {
        super(LAYOUT);
    }

    private CardRecord(byte[] raw) {
        super(LAYOUT, raw);
    }

    public static CardRecord decode(byte[] raw) {
        return new CardRecord(raw);
    }

    @Override
    public Layout layout() {
        return LAYOUT;
    }

    /** COBOL {@code READ ... INTO CARD-RECORD}: replace the whole buffer. */
    public void moveFrom(byte[] raw) {
        System.arraycopy(FixedWidth.decode(LAYOUT, raw), 0, data, 0, data.length);
    }

    public String cardNum() {
        return FixedWidth.text(data, CARD_NUM);
    }

    public void setCardNum(String v) {
        FixedWidth.setText(data, CARD_NUM, v);
    }

    public long cardAcctId() {
        return FixedWidth.unsigned(data, CARD_ACCT_ID);
    }

    public void setCardAcctId(long v) {
        FixedWidth.setUnsigned(data, CARD_ACCT_ID, v);
    }

    public long cardCvvCd() {
        return FixedWidth.unsigned(data, CARD_CVV_CD);
    }

    public void setCardCvvCd(long v) {
        FixedWidth.setUnsigned(data, CARD_CVV_CD, v);
    }

    public String cardEmbossedName() {
        return FixedWidth.text(data, CARD_EMBOSSED_NAME);
    }

    public void setCardEmbossedName(String v) {
        FixedWidth.setText(data, CARD_EMBOSSED_NAME, v);
    }

    public String cardExpiraionDate() {
        return FixedWidth.text(data, CARD_EXPIRAION_DATE);
    }

    public void setCardExpiraionDate(String v) {
        FixedWidth.setText(data, CARD_EXPIRAION_DATE, v);
    }

    public String cardActiveStatus() {
        return FixedWidth.text(data, CARD_ACTIVE_STATUS);
    }

    public void setCardActiveStatus(String v) {
        FixedWidth.setText(data, CARD_ACTIVE_STATUS, v);
    }

    /** What {@code DISPLAY field} prints for one field (signed numerics get GnuCOBOL's trailing sign). */
    public String display(Field f) {
        return FixedWidth.display(data, f);
    }
}
