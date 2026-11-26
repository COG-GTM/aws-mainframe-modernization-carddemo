package com.carddemo.programs.cocrdslc.copybook;

import com.carddemo.programs.cocrdslc.CobolText;
import com.carddemo.programs.cocrdslc.FixedWidthReader;

/**
 * CVACT02Y - CARD-RECORD (RECLN 150), VSAM KSDS CARDDAT keyed by CARD-NUM,
 * alternate index CARDAIX on CARD-ACCT-ID.
 *
 * <pre>
 * 05 CARD-NUM             PIC X(16).
 * 05 CARD-ACCT-ID         PIC 9(11).
 * 05 CARD-CVV-CD          PIC 9(03).
 * 05 CARD-EMBOSSED-NAME   PIC X(50).
 * 05 CARD-EXPIRAION-DATE  PIC X(10).
 * 05 CARD-ACTIVE-STATUS   PIC X(01).
 * 05 FILLER               PIC X(59).
 * </pre>
 */
public record CardRecord(
        String cardNum,
        long cardAcctId,
        int cardCvvCd,
        String cardEmbossedName,
        String cardExpiraionDate,
        String cardActiveStatus) {

    public static final int RECORD_LENGTH = 150;
    public static final int CARD_NUM_LEN = 16;
    public static final int CARD_ACCT_ID_LEN = 11;
    public static final int CARD_CVV_CD_LEN = 3;
    public static final int CARD_EMBOSSED_NAME_LEN = 50;
    public static final int CARD_EXPIRAION_DATE_LEN = 10;
    public static final int CARD_ACTIVE_STATUS_LEN = 1;
    public static final int FILLER_LEN = 59;

    public CardRecord {
        cardNum = CobolText.pic(cardNum, CARD_NUM_LEN);
        cardEmbossedName = CobolText.pic(cardEmbossedName, CARD_EMBOSSED_NAME_LEN);
        cardExpiraionDate = CobolText.pic(cardExpiraionDate, CARD_EXPIRAION_DATE_LEN);
        cardActiveStatus = CobolText.pic(cardActiveStatus, CARD_ACTIVE_STATUS_LEN);
    }

    public static CardRecord parse(String line) {
        FixedWidthReader r = new FixedWidthReader(line, RECORD_LENGTH);
        return new CardRecord(r.x(CARD_NUM_LEN), r.n(CARD_ACCT_ID_LEN), (int) r.n(CARD_CVV_CD_LEN),
                r.x(CARD_EMBOSSED_NAME_LEN), r.x(CARD_EXPIRAION_DATE_LEN), r.x(CARD_ACTIVE_STATUS_LEN));
    }

    public String toFixedWidth() {
        return cardNum
                + CobolText.zoned(cardAcctId, CARD_ACCT_ID_LEN)
                + CobolText.zoned(cardCvvCd, CARD_CVV_CD_LEN)
                + cardEmbossedName
                + cardExpiraionDate
                + cardActiveStatus
                + CobolText.spaces(FILLER_LEN);
    }
}
