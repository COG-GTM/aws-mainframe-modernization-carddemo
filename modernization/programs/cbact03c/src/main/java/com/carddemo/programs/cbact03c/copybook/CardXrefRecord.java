package com.carddemo.programs.cbact03c.copybook;

import java.util.Objects;

/**
 * CVACT03Y - CARD-XREF-RECORD (RECLN 50).
 *
 * <pre>
 * 05 XREF-CARD-NUM  PIC X(16).   offset  0
 * 05 XREF-CUST-ID   PIC 9(09).   offset 16
 * 05 XREF-ACCT-ID   PIC 9(11).   offset 25
 * 05 FILLER         PIC X(14).   offset 36
 * </pre>
 *
 * FILLER is kept so the record round-trips byte-for-byte to the 50-byte legacy image.
 */
public record CardXrefRecord(String cardNum, long custId, long acctId, String filler) {

    public static final int RECORD_LENGTH = 50;
    public static final int CARD_NUM_LENGTH = 16;
    public static final int CUST_ID_DIGITS = 9;
    public static final int ACCT_ID_DIGITS = 11;
    public static final int FILLER_LENGTH = 14;

    static final int CARD_NUM_OFFSET = 0;
    static final int CUST_ID_OFFSET = 16;
    static final int ACCT_ID_OFFSET = 25;
    static final int FILLER_OFFSET = 36;

    public CardXrefRecord {
        cardNum = Pic.alphanumeric(Objects.requireNonNull(cardNum, "cardNum"), CARD_NUM_LENGTH);
        custId = Pic.unsignedTruncate(custId, CUST_ID_DIGITS);
        acctId = Pic.unsignedTruncate(acctId, ACCT_ID_DIGITS);
        filler = Pic.alphanumeric(filler == null ? "" : filler, FILLER_LENGTH);
    }

    public CardXrefRecord(String cardNum, long custId, long acctId) {
        this(cardNum, custId, acctId, "");
    }

    /** Parses a 50-byte record image; shorter images are space-padded like a MOVE to a longer group. */
    public static CardXrefRecord fromRecord(String image) {
        String rec = Pic.alphanumeric(Objects.requireNonNull(image, "image"), RECORD_LENGTH);
        return new CardXrefRecord(
                rec.substring(CARD_NUM_OFFSET, CUST_ID_OFFSET),
                Pic.parseUnsigned(rec.substring(CUST_ID_OFFSET, ACCT_ID_OFFSET), "XREF-CUST-ID"),
                Pic.parseUnsigned(rec.substring(ACCT_ID_OFFSET, FILLER_OFFSET), "XREF-ACCT-ID"),
                rec.substring(FILLER_OFFSET, RECORD_LENGTH));
    }

    /** Fixed-width 50-byte record image (what DISPLAY CARD-XREF-RECORD prints). */
    public String toRecord() {
        return cardNum
                + Pic.zeroPad(custId, CUST_ID_DIGITS)
                + Pic.zeroPad(acctId, ACCT_ID_DIGITS)
                + filler;
    }

    /** The RECORD KEY of the XREFFILE KSDS (FD-XREF-CARD-NUM). */
    public String key() {
        return cardNum;
    }
}
