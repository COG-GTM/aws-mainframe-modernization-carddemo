package com.carddemo.online.cocrdlic;

import com.carddemo.common.copybook.CardDemoCommarea;
import com.carddemo.runtime.cobol.AlphanumericField;
import com.carddemo.runtime.cobol.CobolField;
import com.carddemo.runtime.cobol.CobolRecord;
import com.carddemo.runtime.cobol.NumericField;
import com.carddemo.runtime.cobol.PicX;
import com.carddemo.runtime.cobol.RecordLayout;

/**
 * COCRDLIC {@code WS-THIS-PROGCOMMAREA} (lines 229-260, 254 bytes). {@code 05 WS-SCREEN-DATA} follows the
 * {@code 10}-level items but still belongs to this 01 record, so the 7 screen rows travel in the commarea.
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

    /** WS-CA-SCREEN-NUM 9(1); 88 CA-FIRST-PAGE VALUE 1. */
    static final NumericField SCREEN_NUM = B.zoned("WS-CA-SCREEN-NUM", 1, 0, false);
    /** WS-CA-LAST-PAGE-DISPLAYED 9(1); 88 CA-LAST-PAGE-SHOWN 0, CA-LAST-PAGE-NOT-SHOWN 9. */
    static final NumericField LAST_PAGE_DISPLAYED = B.zoned("WS-CA-LAST-PAGE-DISPLAYED", 1, 0, false);
    /** WS-CA-NEXT-PAGE-IND X(1); 88 CA-NEXT-PAGE-NOT-EXISTS LOW-VALUES, CA-NEXT-PAGE-EXISTS 'Y'. */
    static final AlphanumericField NEXT_PAGE_IND = B.x("WS-CA-NEXT-PAGE-IND", 1);
    /** WS-RETURN-FLAG X(1); 88 WS-RETURN-FLAG-OFF LOW-VALUES, WS-RETURN-FLAG-ON '1' (never referenced). */
    static final AlphanumericField RETURN_FLAG = B.x("WS-RETURN-FLAG", 1);

    static {
        B.group("WS-SCREEN-DATA");
    }

    /** WS-ALL-ROWS X(196). */
    static final AlphanumericField ALL_ROWS = B.x("WS-ALL-ROWS", 196);
    /** WS-EACH-CARD(n) = WS-ROW-ACCTNO X(11), WS-ROW-CARD-NUM X(16), WS-ROW-CARD-STATUS X(1) (REDEFINES). */
    static final AlphanumericField[] EACH_CARD = new AlphanumericField[ROWS];
    static final AlphanumericField[] ROW_ACCTNO = new AlphanumericField[ROWS];
    static final AlphanumericField[] ROW_CARD_NUM = new AlphanumericField[ROWS];
    static final AlphanumericField[] ROW_CARD_STATUS = new AlphanumericField[ROWS];

    static {
        B.redefines(ALL_ROWS);
        for (int i = 0; i < ROWS; i++) {
            B.group("WS-EACH-CARD(" + (i + 1) + ")");
            ROW_ACCTNO[i] = B.x("WS-ROW-ACCTNO(" + (i + 1) + ")", 11);
            ROW_CARD_NUM[i] = B.x("WS-ROW-CARD-NUM(" + (i + 1) + ")", 16);
            ROW_CARD_STATUS[i] = B.x("WS-ROW-CARD-STATUS(" + (i + 1) + ")", 1);
            EACH_CARD[i] = B.endGroup();
        }
        B.endRedefines(ALL_ROWS);
    }

    static final AlphanumericField SCREEN_DATA = B.endGroup();

    static final RecordLayout LAYOUT = B.build();
    static final int LENGTH = LAYOUT.length();

    /** WS-COMMAREA PIC X(2000) (line 262): what RETURN TRANSID passes on. */
    static final int WS_COMMAREA_LENGTH = 2000;

    /**
     * Layout of the 2000-byte WS-COMMAREA built in COMMON-RETURN: CARDDEMO-COMMAREA (1:160),
     * WS-THIS-PROGCOMMAREA (161:254), spaces.
     */
    static final RecordLayout WS_COMMAREA_LAYOUT = wsCommareaLayout();

    ThisProgCommarea() {
        super(LENGTH);
    }

    /** INITIALIZE WS-THIS-PROGCOMMAREA: alphanumerics to SPACES, numerics to ZERO, REDEFINES skipped. */
    void initialize() {
        for (CobolField f : new CobolField[] {LAST_CARD_NUM, LAST_CARD_ACCT_ID, FIRST_CARD_NUM, FIRST_CARD_ACCT_ID,
                SCREEN_NUM, LAST_PAGE_DISPLAYED, NEXT_PAGE_IND, RETURN_FLAG, ALL_ROWS}) {
            if (f instanceof NumericField n) {
                n.set(this, 0);
            } else {
                f.fill(this, PicX.SPACE);
            }
        }
    }

    String screenNumRaw() {
        return SCREEN_NUM.raw(this);
    }

    private static RecordLayout wsCommareaLayout() {
        RecordLayout.Builder b = RecordLayout.builder("WS-COMMAREA");
        for (RecordLayout part : new RecordLayout[] {CardDemoCommarea.LAYOUT, LAYOUT}) {
            for (CobolField f : part.fields()) {
                if (f instanceof NumericField n) {
                    b.zoned(n.name(), n.integerDigits(), n.scale(), n.signed());
                } else if (f != ALL_ROWS) {
                    b.x(f.name(), f.length());
                }
            }
        }
        b.filler(WS_COMMAREA_LENGTH - CardDemoCommarea.LENGTH - LENGTH);
        return b.build();
    }
}
