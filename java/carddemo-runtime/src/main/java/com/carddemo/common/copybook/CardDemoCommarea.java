package com.carddemo.common.copybook;

import com.carddemo.runtime.cobol.AlphanumericField;
import com.carddemo.runtime.cobol.CobolRecord;
import com.carddemo.runtime.cobol.Condition88;
import com.carddemo.runtime.cobol.NumericField;
import com.carddemo.runtime.cobol.RecordLayout;

/** COCOM01Y {@code CARDDEMO-COMMAREA} (160 bytes): the commarea passed between all online programs. */
public final class CardDemoCommarea extends CobolRecord {

    private static final RecordLayout.Builder B = RecordLayout.builder("CARDDEMO-COMMAREA");

    static {
        B.group("CDEMO-GENERAL-INFO");
    }

    public static final AlphanumericField FROM_TRANID = B.x("CDEMO-FROM-TRANID", 4);
    public static final AlphanumericField FROM_PROGRAM = B.x("CDEMO-FROM-PROGRAM", 8);
    public static final AlphanumericField TO_TRANID = B.x("CDEMO-TO-TRANID", 4);
    public static final AlphanumericField TO_PROGRAM = B.x("CDEMO-TO-PROGRAM", 8);
    public static final AlphanumericField USER_ID = B.x("CDEMO-USER-ID", 8);
    public static final AlphanumericField USER_TYPE = B.x("CDEMO-USER-TYPE", 1);
    public static final NumericField PGM_CONTEXT = B.zoned("CDEMO-PGM-CONTEXT", 1, 0, false);
    public static final AlphanumericField GENERAL_INFO = B.endGroup();

    static {
        B.group("CDEMO-CUSTOMER-INFO");
    }

    public static final NumericField CUST_ID = B.zoned("CDEMO-CUST-ID", 9, 0, false);
    public static final AlphanumericField CUST_FNAME = B.x("CDEMO-CUST-FNAME", 25);
    public static final AlphanumericField CUST_MNAME = B.x("CDEMO-CUST-MNAME", 25);
    public static final AlphanumericField CUST_LNAME = B.x("CDEMO-CUST-LNAME", 25);
    public static final AlphanumericField CUSTOMER_INFO = B.endGroup();

    static {
        B.group("CDEMO-ACCOUNT-INFO");
    }

    public static final NumericField ACCT_ID = B.zoned("CDEMO-ACCT-ID", 11, 0, false);
    public static final AlphanumericField ACCT_STATUS = B.x("CDEMO-ACCT-STATUS", 1);
    public static final AlphanumericField ACCOUNT_INFO = B.endGroup();

    static {
        B.group("CDEMO-CARD-INFO");
    }

    public static final NumericField CARD_NUM = B.zoned("CDEMO-CARD-NUM", 16, 0, false);
    public static final AlphanumericField CARD_INFO = B.endGroup();

    static {
        B.group("CDEMO-MORE-INFO");
    }

    public static final AlphanumericField LAST_MAP = B.x("CDEMO-LAST-MAP", 7);
    public static final AlphanumericField LAST_MAPSET = B.x("CDEMO-LAST-MAPSET", 7);
    public static final AlphanumericField MORE_INFO = B.endGroup();

    public static final RecordLayout LAYOUT = B.build();
    public static final int LENGTH = LAYOUT.length();

    public static final Condition88 USRTYP_ADMIN = Condition88.of("CDEMO-USRTYP-ADMIN", USER_TYPE, "A");
    public static final Condition88 USRTYP_USER = Condition88.of("CDEMO-USRTYP-USER", USER_TYPE, "U");
    public static final Condition88 PGM_ENTER = Condition88.of("CDEMO-PGM-ENTER", PGM_CONTEXT, 0);
    public static final Condition88 PGM_REENTER = Condition88.of("CDEMO-PGM-REENTER", PGM_CONTEXT, 1);

    /** New commarea in uninitialised WORKING-STORAGE state (LOW-VALUES). */
    public CardDemoCommarea() {
        super(LENGTH);
    }

    /** Copies (DFHCOMMAREA(1:EIBCALEN) TO CARDDEMO-COMMAREA): pads with spaces / truncates to 160 bytes. */
    public static CardDemoCommarea of(byte[] bytes) {
        CardDemoCommarea c = new CardDemoCommarea();
        c.setBytes(bytes);
        return c;
    }

    public String fromTranid() {
        return FROM_TRANID.get(this);
    }

    public void setFromTranid(String v) {
        FROM_TRANID.set(this, v);
    }

    public String fromProgram() {
        return FROM_PROGRAM.get(this);
    }

    public void setFromProgram(String v) {
        FROM_PROGRAM.set(this, v);
    }

    public String toTranid() {
        return TO_TRANID.get(this);
    }

    public void setToTranid(String v) {
        TO_TRANID.set(this, v);
    }

    public String toProgram() {
        return TO_PROGRAM.get(this);
    }

    public void setToProgram(String v) {
        TO_PROGRAM.set(this, v);
    }

    public String userId() {
        return USER_ID.get(this);
    }

    public void setUserId(String v) {
        USER_ID.set(this, v);
    }

    public String userType() {
        return USER_TYPE.get(this);
    }

    public void setUserType(String v) {
        USER_TYPE.set(this, v);
    }

    public boolean isAdmin() {
        return USRTYP_ADMIN.isTrue(this);
    }

    public boolean isUser() {
        return USRTYP_USER.isTrue(this);
    }

    /** Raw CDEMO-PGM-CONTEXT byte (it may be LOW-VALUE when never set). */
    public String pgmContextRaw() {
        return PGM_CONTEXT.raw(this);
    }

    public void setPgmContext(int v) {
        PGM_CONTEXT.set(this, v);
    }

    public boolean isPgmEnter() {
        return PGM_ENTER.isTrue(this);
    }

    public boolean isPgmReenter() {
        return PGM_REENTER.isTrue(this);
    }

    public String lastMap() {
        return LAST_MAP.get(this);
    }

    public void setLastMap(String v) {
        LAST_MAP.set(this, v);
    }

    public String lastMapset() {
        return LAST_MAPSET.get(this);
    }

    public void setLastMapset(String v) {
        LAST_MAPSET.set(this, v);
    }
}
