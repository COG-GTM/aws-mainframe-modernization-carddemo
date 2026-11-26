package com.carddemo.programs.cocrdlic.copybook;

/**
 * CVACT02Y - CARD-RECORD (RECLN 150), the CARDDAT VSAM KSDS layout keyed by CARD-NUM.
 *
 * <pre>
 * 05 CARD-NUM             PIC X(16).   offset   0
 * 05 CARD-ACCT-ID         PIC 9(11).   offset  16
 * 05 CARD-CVV-CD          PIC 9(03).   offset  27
 * 05 CARD-EMBOSSED-NAME   PIC X(50).   offset  30
 * 05 CARD-EXPIRAION-DATE  PIC X(10).   offset  80
 * 05 CARD-ACTIVE-STATUS   PIC X(01).   offset  90
 * 05 FILLER               PIC X(59).   offset  91
 * </pre>
 */
public record CardRecord(
        String cardNum,
        long cardAcctId,
        int cardCvvCd,
        String cardEmbossedName,
        String cardExpirationDate,
        char cardActiveStatus,
        String filler) {

    public static final int RECORD_LENGTH = 150;
    public static final int CARD_NUM_LEN = 16;
    public static final int CARD_ACCT_ID_DIGITS = 11;
    public static final int CARD_CVV_CD_DIGITS = 3;
    public static final int CARD_EMBOSSED_NAME_LEN = 50;
    public static final int CARD_EXPIRATION_DATE_LEN = 10;
    public static final int FILLER_LEN = 59;

    public CardRecord {
        cardNum = Pic.x(cardNum, CARD_NUM_LEN);
        cardAcctId = Pic.truncUnsigned(cardAcctId, CARD_ACCT_ID_DIGITS);
        cardCvvCd = (int) Pic.truncUnsigned(cardCvvCd, CARD_CVV_CD_DIGITS);
        cardEmbossedName = Pic.x(cardEmbossedName, CARD_EMBOSSED_NAME_LEN);
        cardExpirationDate = Pic.x(cardExpirationDate, CARD_EXPIRATION_DATE_LEN);
        filler = Pic.x(filler, FILLER_LEN);
    }

    public static CardRecord of(String cardNum, long acctId, char activeStatus) {
        return new CardRecord(cardNum, acctId, 0, "", "", activeStatus, "");
    }

    /** Parse one fixed-width 150-byte record (ASCII extract of the VSAM file). */
    public static CardRecord parse(String line) {
        String r = Pic.x(line, RECORD_LENGTH);
        return new CardRecord(
                r.substring(0, 16),
                Pic.toUnsigned(r.substring(16, 27), 11),
                (int) Pic.toUnsigned(r.substring(27, 30), 3),
                r.substring(30, 80),
                r.substring(80, 90),
                r.charAt(90),
                r.substring(91, 150));
    }

    public String toFixedWidth() {
        return cardNum + Pic.nine(cardAcctId, 11) + Pic.nine(cardCvvCd, 3) + cardEmbossedName
                + cardExpirationDate + cardActiveStatus + filler;
    }

    /** CARD-ACCT-ID as it appears in DISPLAY form (used for alphanumeric comparisons / moves). */
    public String cardAcctIdDisplay() {
        return Pic.nine(cardAcctId, 11);
    }
}
