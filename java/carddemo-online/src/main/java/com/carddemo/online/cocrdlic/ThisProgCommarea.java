package com.carddemo.online.cocrdlic;

import com.carddemo.runtime.cobol.AlphanumericField;
import com.carddemo.runtime.cobol.CobolRecord;
import com.carddemo.runtime.cobol.Condition88;
import com.carddemo.runtime.cobol.NumericField;
import com.carddemo.runtime.cobol.PicX;
import com.carddemo.runtime.cobol.RecordLayout;

/**
 * COCRDLIC {@code WS-THIS-PROGCOMMAREA} (lines 229-260, 254 bytes): paging keys/flags plus {@code WS-SCREEN-DATA}
 * (7 rows of 28 bytes). {@code 05 WS-SCREEN-DATA} follows the {@code 10}-level items inside the same 01, so the
 * displayed rows travel in the commarea right after CARDDEMO-COMMAREA (bytes 161-414).
 */
final class ThisProgCommarea extends CobolRecord {

    static final int ROWS = 7;

    private static final RecordLayout.Builder B = RecordLayout.builder("WS-THIS-PROGCOMMAREA");

    static {
        B.group("WS-CA-LAST-CARDKEY");
    }

    static final AlphanumericField LAST_CARD_NUM = B.x("WS-CA-LAST-CARD-NUM", 16);
    static final NumericField LAST_CARD_ACCT_ID = B.zoned("WS-CA-LAST-CARD-ACCT-ID", 11, 0, false);
    static final AlphanumericField LAST_CARDKEY = B.endGroup();

    static {
        B.group("WS-CA-FIRST-CARDKEY");
    }

    static final AlphanumericField FIRST_CARD_NUM = B.x("WS-CA-FIRST-CARD-NUM", 16);
    static final NumericField FIRST_CARD_ACCT_ID = B.zoned("WS-CA-FIRST-CARD-ACCT-ID", 11, 0, false);
    static final AlphanumericField FIRST_CARDKEY = B.endGroup();

    static final NumericField SCREEN_NUM = B.zoned("WS-CA-SCREEN-NUM", 1, 0, false);
    static final NumericField LAST_PAGE_DISPLAYED = B.zoned("WS-CA-LAST-PAGE-DISPLAYED", 1, 0, false);
    static final AlphanumericField NEXT_PAGE_IND = B.x("WS-CA-NEXT-PAGE-IND", 1);
    static final AlphanumericField RETURN_FLAG = B.x("WS-RETURN-FLAG", 1);

    static final AlphanumericField[] ROW_ACCTNO = new AlphanumericField[ROWS];
    static final AlphanumericField[] ROW_CARD_NUM = new AlphanumericField[ROWS];
    static final AlphanumericField[] ROW_CARD_STATUS = new AlphanumericField[ROWS];
    /** {@code WS-EACH-CARD(n)} group views. */
    static final AlphanumericField[] EACH_CARD = new AlphanumericField[ROWS];
    static final AlphanumericField ALL_ROWS;
    static final AlphanumericField SCREEN_DATA;

    static {
        B.group("WS-SCREEN-DATA");
        B.group("WS-ALL-ROWS");
        for (int n = 1; n <= ROWS; n++) {
            B.group("WS-EACH-CARD(" + n + ")");
            ROW_ACCTNO[n - 1] = B.x("WS-ROW-ACCTNO(" + n + ")", 11);
            ROW_CARD_NUM[n - 1] = B.x("WS-ROW-CARD-NUM(" + n + ")", 16);
            ROW_CARD_STATUS[n - 1] = B.x("WS-ROW-CARD-STATUS(" + n + ")", 1);
            EACH_CARD[n - 1] = B.endGroup();
        }
        ALL_ROWS = B.endGroup();
        SCREEN_DATA = B.endGroup();
    }

    static final RecordLayout LAYOUT = B.build();
    static final int LENGTH = LAYOUT.length();

    static final Condition88 CA_FIRST_PAGE = Condition88.of("CA-FIRST-PAGE", SCREEN_NUM, 1);
    static final Condition88 CA_LAST_PAGE_SHOWN = Condition88.of("CA-LAST-PAGE-SHOWN", LAST_PAGE_DISPLAYED, 0);
    static final Condition88 CA_LAST_PAGE_NOT_SHOWN = Condition88.of("CA-LAST-PAGE-NOT-SHOWN", LAST_PAGE_DISPLAYED, 9);
    static final Condition88 CA_NEXT_PAGE_NOT_EXISTS = Condition88.of("CA-NEXT-PAGE-NOT-EXISTS", NEXT_PAGE_IND,
            String.valueOf(PicX.LOW_VALUE));
    static final Condition88 CA_NEXT_PAGE_EXISTS = Condition88.of("CA-NEXT-PAGE-EXISTS", NEXT_PAGE_IND, "Y");
    static final Condition88 WS_RETURN_FLAG_OFF = Condition88.of("WS-RETURN-FLAG-OFF", RETURN_FLAG,
            String.valueOf(PicX.LOW_VALUE));
    static final Condition88 WS_RETURN_FLAG_ON = Condition88.of("WS-RETURN-FLAG-ON", RETURN_FLAG, "1");

    /** Uninitialised WORKING-STORAGE (LOW-VALUES). */
    ThisProgCommarea() {
        super(LENGTH);
    }

    static ThisProgCommarea of(byte[] bytes) {
        ThisProgCommarea c = new ThisProgCommarea();
        c.setBytes(bytes);
        return c;
    }

    /** {@code INITIALIZE WS-THIS-PROGCOMMAREA}: PIC X items to SPACES, PIC 9 items to ZERO. */
    void initialize() {
        initializeSpaces();
        LAST_CARD_ACCT_ID.set(this, 0);
        FIRST_CARD_ACCT_ID.set(this, 0);
        SCREEN_NUM.set(this, 0);
        LAST_PAGE_DISPLAYED.set(this, 0);
    }

    String row(int n) {
        return EACH_CARD[n - 1].get(this);
    }

    String rowAcctNo(int n) {
        return ROW_ACCTNO[n - 1].get(this);
    }

    String rowCardNum(int n) {
        return ROW_CARD_NUM[n - 1].get(this);
    }

    String rowCardStatus(int n) {
        return ROW_CARD_STATUS[n - 1].get(this);
    }

    String firstCardNum() {
        return FIRST_CARD_NUM.get(this);
    }

    String lastCardNum() {
        return LAST_CARD_NUM.get(this);
    }

    /** Value of {@code WS-CA-SCREEN-NUM}, or -1 when it does not hold a valid digit. */
    int screenNum() {
        return SCREEN_NUM.isNumeric(this) ? SCREEN_NUM.get(this).intValue() : -1;
    }
}
