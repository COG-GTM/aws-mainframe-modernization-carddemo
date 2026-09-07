package com.carddemo.poc.copybook;

import java.nio.charset.StandardCharsets;

/**
 * Java model of copybook {@code CVACT03Y} (card cross-reference, RECLN 50).
 *
 * <pre>
 *  01 CARD-XREF-RECORD.
 *      05  XREF-CARD-NUM   PIC X(16).   offset  0, len 16
 *      05  XREF-CUST-ID    PIC 9(09).   offset 16, len  9
 *      05  XREF-ACCT-ID    PIC 9(11).   offset 25, len 11
 *      05  FILLER          PIC X(14).   offset 36, len 14
 * </pre>
 *
 * The raw 50-character image is retained so that {@code DISPLAY CARD-XREF-RECORD}
 * can be reproduced byte-for-byte.
 */
public final class CardXrefRecord {

    public static final int RECORD_LENGTH = 50;

    private static final int CARD_NUM_OFF = 0;
    private static final int CARD_NUM_LEN = 16;
    private static final int CUST_ID_OFF = 16;
    private static final int CUST_ID_LEN = 9;
    private static final int ACCT_ID_OFF = 25;
    private static final int ACCT_ID_LEN = 11;

    private final String image;

    private CardXrefRecord(String image) {
        this.image = image;
    }

    /** Equivalent of {@code READ ... INTO CARD-XREF-RECORD}: the record image is ASCII text. */
    public static CardXrefRecord fromImage(String image) {
        if (image.length() != RECORD_LENGTH) {
            throw new IllegalArgumentException(
                    "CARD-XREF-RECORD must be " + RECORD_LENGTH + " chars, got " + image.length());
        }
        return new CardXrefRecord(image);
    }

    /** Record bytes already translated to ISO-8859-1 (see {@link com.carddemo.poc.io.FixedLengthRecordReader}). */
    public static CardXrefRecord fromBytes(byte[] record) {
        return fromImage(new String(record, StandardCharsets.ISO_8859_1));
    }

    public String getCardNum() {
        return image.substring(CARD_NUM_OFF, CARD_NUM_OFF + CARD_NUM_LEN);
    }

    public long getCustId() {
        return ZonedDecimal.parseUnsignedLong(image.substring(CUST_ID_OFF, CUST_ID_OFF + CUST_ID_LEN));
    }

    public long getAcctId() {
        return ZonedDecimal.parseUnsignedLong(image.substring(ACCT_ID_OFF, ACCT_ID_OFF + ACCT_ID_LEN));
    }

    /** The full 50-character group item, i.e. what {@code DISPLAY CARD-XREF-RECORD} prints. */
    public String toDisplayString() {
        return image;
    }

    @Override
    public String toString() {
        return "CardXrefRecord{cardNum=" + getCardNum() + ", custId=" + getCustId() + ", acctId=" + getAcctId() + '}';
    }
}
