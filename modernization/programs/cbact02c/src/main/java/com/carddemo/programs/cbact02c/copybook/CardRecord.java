package com.carddemo.programs.cbact02c.copybook;

/**
 * Copybook CVACT02Y - data-structure for card entity (RECLN 150).
 *
 * <pre>
 * 01  CARD-RECORD.
 *     05  CARD-NUM                 PIC X(16).
 *     05  CARD-ACCT-ID             PIC 9(11).
 *     05  CARD-CVV-CD              PIC 9(03).
 *     05  CARD-EMBOSSED-NAME       PIC X(50).
 *     05  CARD-EXPIRAION-DATE      PIC X(10).
 *     05  CARD-ACTIVE-STATUS       PIC X(01).
 *     05  FILLER                   PIC X(59).
 * </pre>
 *
 * The copybook declares no 88-levels. FILLER is kept so the 150-byte image round-trips exactly.
 * Alphanumeric components hold the exact field width (space padded); numeric components are the
 * unsigned values of the zoned PIC 9 fields.
 */
public record CardRecord(
        String cardNum,
        long cardAcctId,
        int cardCvvCd,
        String cardEmbossedName,
        String cardExpiraionDate,
        String cardActiveStatus,
        String filler) {

    public static final int RECORD_LENGTH = 150;

    public static final int CARD_NUM_LEN = 16;
    public static final int CARD_ACCT_ID_LEN = 11;
    public static final int CARD_CVV_CD_LEN = 3;
    public static final int CARD_EMBOSSED_NAME_LEN = 50;
    public static final int CARD_EXPIRAION_DATE_LEN = 10;
    public static final int CARD_ACTIVE_STATUS_LEN = 1;
    public static final int FILLER_LEN = 59;

    public static final int CARD_NUM_OFFSET = 0;
    public static final int CARD_ACCT_ID_OFFSET = CARD_NUM_OFFSET + CARD_NUM_LEN;
    public static final int CARD_CVV_CD_OFFSET = CARD_ACCT_ID_OFFSET + CARD_ACCT_ID_LEN;
    public static final int CARD_EMBOSSED_NAME_OFFSET = CARD_CVV_CD_OFFSET + CARD_CVV_CD_LEN;
    public static final int CARD_EXPIRAION_DATE_OFFSET = CARD_EMBOSSED_NAME_OFFSET + CARD_EMBOSSED_NAME_LEN;
    public static final int CARD_ACTIVE_STATUS_OFFSET = CARD_EXPIRAION_DATE_OFFSET + CARD_EXPIRAION_DATE_LEN;
    public static final int FILLER_OFFSET = CARD_ACTIVE_STATUS_OFFSET + CARD_ACTIVE_STATUS_LEN;

    public static final long CARD_ACCT_ID_MAX = 99_999_999_999L;
    public static final int CARD_CVV_CD_MAX = 999;

    public CardRecord {
        cardNum = Pic.alpha(cardNum, CARD_NUM_LEN);
        if (cardAcctId < 0 || cardAcctId > CARD_ACCT_ID_MAX) {
            throw new IllegalArgumentException("CARD-ACCT-ID out of range for PIC 9(11): " + cardAcctId);
        }
        if (cardCvvCd < 0 || cardCvvCd > CARD_CVV_CD_MAX) {
            throw new IllegalArgumentException("CARD-CVV-CD out of range for PIC 9(03): " + cardCvvCd);
        }
        cardEmbossedName = Pic.alpha(cardEmbossedName, CARD_EMBOSSED_NAME_LEN);
        cardExpiraionDate = Pic.alpha(cardExpiraionDate, CARD_EXPIRAION_DATE_LEN);
        cardActiveStatus = Pic.alpha(cardActiveStatus, CARD_ACTIVE_STATUS_LEN);
        filler = Pic.alpha(filler, FILLER_LEN);
    }

    /**
     * Parses a 150-byte CARD-RECORD image. Shorter images are space padded and longer ones
     * truncated (group MOVE semantics). Numeric fields must contain digits only.
     */
    public static CardRecord fromFixedWidth(String image) {
        String rec = Pic.alpha(image, RECORD_LENGTH);
        return new CardRecord(
                field(rec, CARD_NUM_OFFSET, CARD_NUM_LEN),
                Long.parseLong(numeric(rec, "CARD-ACCT-ID", CARD_ACCT_ID_OFFSET, CARD_ACCT_ID_LEN)),
                Integer.parseInt(numeric(rec, "CARD-CVV-CD", CARD_CVV_CD_OFFSET, CARD_CVV_CD_LEN)),
                field(rec, CARD_EMBOSSED_NAME_OFFSET, CARD_EMBOSSED_NAME_LEN),
                field(rec, CARD_EXPIRAION_DATE_OFFSET, CARD_EXPIRAION_DATE_LEN),
                field(rec, CARD_ACTIVE_STATUS_OFFSET, CARD_ACTIVE_STATUS_LEN),
                field(rec, FILLER_OFFSET, FILLER_LEN));
    }

    /** Serialises back to the exact 150-byte CARD-RECORD image. */
    public String toFixedWidth() {
        return cardNum
                + Pic.unsignedNumeric(cardAcctId, CARD_ACCT_ID_LEN)
                + Pic.unsignedNumeric(cardCvvCd, CARD_CVV_CD_LEN)
                + cardEmbossedName
                + cardExpiraionDate
                + cardActiveStatus
                + filler;
    }

    private static String field(String rec, int offset, int length) {
        return rec.substring(offset, offset + length);
    }

    private static String numeric(String rec, String name, int offset, int length) {
        String raw = field(rec, offset, length);
        if (!Pic.isNumeric(raw)) {
            throw new IllegalArgumentException(name + " is not numeric: '" + raw + "'");
        }
        return raw;
    }
}
