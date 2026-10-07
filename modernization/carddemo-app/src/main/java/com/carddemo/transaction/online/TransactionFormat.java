package com.carddemo.transaction.online;

import com.carddemo.common.codec.NumericEdited;
import java.math.BigDecimal;

/** Screen formats shared by the transaction maps (COTRN0A, COTRN1A, COTRN2A). */
public final class TransactionFormat {

    /** {@code WS-TRAN-AMT PIC +99999999.99} (COTRN00C, COTRN01C, COTRN02C). */
    public static final String AMOUNT_PICTURE = "+99999999.99";

    /** {@code TDESCnn} on the list: 26 bytes. */
    public static final int LIST_DESCRIPTION_LENGTH = 26;

    private TransactionFormat() {
    }

    public static String amount(BigDecimal amount) {
        return NumericEdited.format(amount, AMOUNT_PICTURE);
    }

    /** COTRN00C {@code POPULATE-TRAN-DATA} R-17: {@code MM/DD/YY} from {@code TRAN-ORIG-TS} (6:2), (9:2) and (3:2). */
    public static String listDate(String origTs) {
        String ts = pad(origTs, 10);
        return ts.substring(5, 7) + "/" + ts.substring(8, 10) + "/" + ts.substring(2, 4);
    }

    /** COTRN00C R-17: {@code TRAN-DESC} moved into the 26-byte {@code TDESCnn}. */
    public static String listDescription(String description) {
        String text = description == null ? "" : description;
        return text.length() <= LIST_DESCRIPTION_LENGTH ? text : text.substring(0, LIST_DESCRIPTION_LENGTH);
    }

    /** The first 10 bytes of a 26-byte timestamp, as {@code MOVE TRAN-ORIG-TS TO TORIGDTI} (X(10)) keeps them. */
    public static String date(String timestamp) {
        String ts = timestamp == null ? "" : timestamp;
        return ts.length() <= 10 ? ts : ts.substring(0, 10);
    }

    static String pad(String value, int length) {
        String text = value == null ? "" : value;
        return text.length() >= length ? text : text + " ".repeat(length - text.length());
    }
}
