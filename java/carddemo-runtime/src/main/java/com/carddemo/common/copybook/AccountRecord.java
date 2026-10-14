package com.carddemo.common.copybook;

import com.carddemo.runtime.cobol.AlphanumericField;
import com.carddemo.runtime.cobol.CobolRecord;
import com.carddemo.runtime.cobol.NumericField;
import com.carddemo.runtime.cobol.RecordLayout;

/**
 * CVACT01Y {@code ACCOUNT-RECORD} (300 bytes): one ACCTDAT KSDS record, key = ACCT-ID (offset 0, length 11).
 * Field names are the COBOL names; all numerics are DISPLAY (zoned, trailing overpunch sign when signed).
 */
public final class AccountRecord extends CobolRecord {

    private static final RecordLayout.Builder B = RecordLayout.builder("ACCOUNT-RECORD");

    /** ACCT-ID PIC 9(11). */
    public static final NumericField ACCT_ID = B.zoned("ACCT-ID", 11, 0, false);
    /** ACCT-ACTIVE-STATUS PIC X(01). */
    public static final AlphanumericField ACTIVE_STATUS = B.x("ACCT-ACTIVE-STATUS", 1);
    /** ACCT-CURR-BAL PIC S9(10)V99. */
    public static final NumericField CURR_BAL = B.zoned("ACCT-CURR-BAL", 10, 2, true);
    /** ACCT-CREDIT-LIMIT PIC S9(10)V99. */
    public static final NumericField CREDIT_LIMIT = B.zoned("ACCT-CREDIT-LIMIT", 10, 2, true);
    /** ACCT-CASH-CREDIT-LIMIT PIC S9(10)V99. */
    public static final NumericField CASH_CREDIT_LIMIT = B.zoned("ACCT-CASH-CREDIT-LIMIT", 10, 2, true);
    /** ACCT-OPEN-DATE PIC X(10). */
    public static final AlphanumericField OPEN_DATE = B.x("ACCT-OPEN-DATE", 10);
    /** ACCT-EXPIRAION-DATE PIC X(10) (sic). */
    public static final AlphanumericField EXPIRAION_DATE = B.x("ACCT-EXPIRAION-DATE", 10);
    /** ACCT-REISSUE-DATE PIC X(10). */
    public static final AlphanumericField REISSUE_DATE = B.x("ACCT-REISSUE-DATE", 10);
    /** ACCT-CURR-CYC-CREDIT PIC S9(10)V99. */
    public static final NumericField CURR_CYC_CREDIT = B.zoned("ACCT-CURR-CYC-CREDIT", 10, 2, true);
    /** ACCT-CURR-CYC-DEBIT PIC S9(10)V99. */
    public static final NumericField CURR_CYC_DEBIT = B.zoned("ACCT-CURR-CYC-DEBIT", 10, 2, true);
    /** ACCT-ADDR-ZIP PIC X(10). */
    public static final AlphanumericField ADDR_ZIP = B.x("ACCT-ADDR-ZIP", 10);
    /** ACCT-GROUP-ID PIC X(10). */
    public static final AlphanumericField GROUP_ID = B.x("ACCT-GROUP-ID", 10);

    static {
        B.filler(178);
    }

    public static final RecordLayout LAYOUT = B.build();
    public static final int LENGTH = LAYOUT.length();
    public static final int KEY_OFFSET = 0;
    public static final int KEY_LENGTH = 11;

    /** New record in uninitialised WORKING-STORAGE state (LOW-VALUES). */
    public AccountRecord() {
        super(LENGTH);
    }

    public static AccountRecord of(byte[] bytes) {
        AccountRecord r = new AccountRecord();
        r.setBytes(bytes);
        return r;
    }

    public static AccountRecord of(String record) {
        AccountRecord r = new AccountRecord();
        r.move(record);
        return r;
    }
}
