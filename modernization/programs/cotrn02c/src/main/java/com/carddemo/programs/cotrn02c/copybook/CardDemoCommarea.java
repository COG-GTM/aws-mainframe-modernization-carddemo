package com.carddemo.programs.cotrn02c.copybook;

/**
 * COCOM01Y {@code CARDDEMO-COMMAREA} plus the COTRN02C-local {@code CDEMO-CT02-INFO} group that the
 * program appends to the same 01 level. Record length 160 + 58 = 218 bytes.
 */
public final class CardDemoCommarea {

    public static final int GENERAL_INFO_LENGTH = 34;
    public static final int BASE_LENGTH = 160;
    public static final int CT02_INFO_LENGTH = 58;
    public static final int LENGTH = BASE_LENGTH + CT02_INFO_LENGTH;

    /** 88-levels on CDEMO-USER-TYPE. */
    public enum UserType {
        ADMIN('A'), USER('U');

        public final char code;

        UserType(char code) {
            this.code = code;
        }
    }

    // 05 CDEMO-GENERAL-INFO
    private String fromTranId;   // X(04)
    private String fromProgram;  // X(08)
    private String toTranId;     // X(04)
    private String toProgram;    // X(08)
    private String userId;       // X(08)
    private char userType;       // X(01)  88 ADMIN 'A' / USER 'U'
    private char pgmContext;     // 9(01)  88 ENTER 0 / REENTER 1
    // 05 CDEMO-CUSTOMER-INFO
    private String custId;       // 9(09)
    private String custFname;    // X(25)
    private String custMname;    // X(25)
    private String custLname;    // X(25)
    // 05 CDEMO-ACCOUNT-INFO
    private String acctId;       // 9(11)
    private char acctStatus;     // X(01)
    // 05 CDEMO-CARD-INFO
    private String cardNum;      // 9(16)
    // 05 CDEMO-MORE-INFO
    private String lastMap;      // X(7)
    private String lastMapset;   // X(7)
    // 05 CDEMO-CT02-INFO (COTRN02C working storage)
    private String ct02TrnIdFirst;   // X(16)
    private String ct02TrnIdLast;    // X(16)
    private String ct02PageNum;      // 9(08)
    private char ct02NextPageFlg;    // X(01) VALUE 'N'  88 NEXT-PAGE-YES 'Y' / NEXT-PAGE-NO 'N'
    private char ct02TrnSelFlg;      // X(01)
    private String ct02TrnSelected;  // X(16)

    /**
     * Working-storage initial state: no VALUE clause except CDEMO-CT02-NEXT-PAGE-FLG ('N'); all other
     * bytes are assumed to be LOW-VALUES (CICS-acquired storage).
     */
    public static CardDemoCommarea initial() {
        CardDemoCommarea c = fromRecord(Pic.lowValues(LENGTH));
        c.ct02NextPageFlg = 'N';
        return c;
    }

    /**
     * {@code MOVE DFHCOMMAREA(1:EIBCALEN) TO CARDDEMO-COMMAREA}: group (alphanumeric) move, so a short
     * COMMAREA is padded with spaces and a long one is truncated.
     */
    public static CardDemoCommarea fromRecord(String raw) {
        String r = Pic.x(raw, LENGTH);
        CardDemoCommarea c = new CardDemoCommarea();
        int[] p = {0};
        c.fromTranId = take(r, p, 4);
        c.fromProgram = take(r, p, 8);
        c.toTranId = take(r, p, 4);
        c.toProgram = take(r, p, 8);
        c.userId = take(r, p, 8);
        c.userType = take(r, p, 1).charAt(0);
        c.pgmContext = take(r, p, 1).charAt(0);
        c.custId = take(r, p, 9);
        c.custFname = take(r, p, 25);
        c.custMname = take(r, p, 25);
        c.custLname = take(r, p, 25);
        c.acctId = take(r, p, 11);
        c.acctStatus = take(r, p, 1).charAt(0);
        c.cardNum = take(r, p, 16);
        c.lastMap = take(r, p, 7);
        c.lastMapset = take(r, p, 7);
        c.ct02TrnIdFirst = take(r, p, 16);
        c.ct02TrnIdLast = take(r, p, 16);
        c.ct02PageNum = take(r, p, 8);
        c.ct02NextPageFlg = take(r, p, 1).charAt(0);
        c.ct02TrnSelFlg = take(r, p, 1).charAt(0);
        c.ct02TrnSelected = take(r, p, 16);
        return c;
    }

    private static String take(String r, int[] p, int len) {
        String s = r.substring(p[0], p[0] + len);
        p[0] += len;
        return s;
    }

    public String toRecord() {
        return fromTranId + fromProgram + toTranId + toProgram + userId + userType + pgmContext
                + custId + custFname + custMname + custLname
                + acctId + acctStatus
                + cardNum
                + lastMap + lastMapset
                + ct02TrnIdFirst + ct02TrnIdLast + ct02PageNum + ct02NextPageFlg + ct02TrnSelFlg
                + ct02TrnSelected;
    }

    public CardDemoCommarea copy() {
        return fromRecord(toRecord());
    }

    // 88-level predicates / setters
    public boolean isUserTypeAdmin() {
        return userType == UserType.ADMIN.code;
    }

    public boolean isUserTypeUser() {
        return userType == UserType.USER.code;
    }

    public boolean isPgmEnter() {
        return pgmContext == '0';
    }

    public boolean isPgmReenter() {
        return pgmContext == '1';
    }

    public void setPgmReenter() {
        pgmContext = '1';
    }

    public void setPgmEnter() {
        pgmContext = '0';
    }

    public boolean isNextPageYes() {
        return ct02NextPageFlg == 'Y';
    }

    public boolean isNextPageNo() {
        return ct02NextPageFlg == 'N';
    }

    public String getFromTranId() {
        return fromTranId;
    }

    public void setFromTranId(String v) {
        fromTranId = Pic.x(v, 4);
    }

    public String getFromProgram() {
        return fromProgram;
    }

    public void setFromProgram(String v) {
        fromProgram = Pic.x(v, 8);
    }

    public String getToTranId() {
        return toTranId;
    }

    public void setToTranId(String v) {
        toTranId = Pic.x(v, 4);
    }

    public String getToProgram() {
        return toProgram;
    }

    public void setToProgram(String v) {
        toProgram = Pic.x(v, 8);
    }

    public String getUserId() {
        return userId;
    }

    public void setUserId(String v) {
        userId = Pic.x(v, 8);
    }

    public char getUserType() {
        return userType;
    }

    public void setUserType(char v) {
        userType = v;
    }

    public char getPgmContext() {
        return pgmContext;
    }

    /** {@code MOVE ZEROS TO CDEMO-PGM-CONTEXT} / any PIC 9(01) value. */
    public void setPgmContext(int v) {
        pgmContext = Pic.nine(v, 1).charAt(0);
    }

    public String getCustId() {
        return custId;
    }

    public void setCustId(long v) {
        custId = Pic.nine(v, 9);
    }

    public String getCustFname() {
        return custFname;
    }

    public void setCustFname(String v) {
        custFname = Pic.x(v, 25);
    }

    public String getCustMname() {
        return custMname;
    }

    public void setCustMname(String v) {
        custMname = Pic.x(v, 25);
    }

    public String getCustLname() {
        return custLname;
    }

    public void setCustLname(String v) {
        custLname = Pic.x(v, 25);
    }

    public String getAcctId() {
        return acctId;
    }

    public void setAcctId(long v) {
        acctId = Pic.nine(v, 11);
    }

    public char getAcctStatus() {
        return acctStatus;
    }

    public void setAcctStatus(char v) {
        acctStatus = v;
    }

    public String getCardNum() {
        return cardNum;
    }

    public void setCardNum(String digits) {
        cardNum = Pic.nine(Pic.alphaToUnsigned(digits), 16);
    }

    public String getLastMap() {
        return lastMap;
    }

    public void setLastMap(String v) {
        lastMap = Pic.x(v, 7);
    }

    public String getLastMapset() {
        return lastMapset;
    }

    public void setLastMapset(String v) {
        lastMapset = Pic.x(v, 7);
    }

    public String getCt02TrnIdFirst() {
        return ct02TrnIdFirst;
    }

    public void setCt02TrnIdFirst(String v) {
        ct02TrnIdFirst = Pic.x(v, 16);
    }

    public String getCt02TrnIdLast() {
        return ct02TrnIdLast;
    }

    public void setCt02TrnIdLast(String v) {
        ct02TrnIdLast = Pic.x(v, 16);
    }

    public String getCt02PageNum() {
        return ct02PageNum;
    }

    public void setCt02PageNum(long v) {
        ct02PageNum = Pic.nine(v, 8);
    }

    public char getCt02NextPageFlg() {
        return ct02NextPageFlg;
    }

    public void setCt02NextPageFlg(char v) {
        ct02NextPageFlg = v;
    }

    public char getCt02TrnSelFlg() {
        return ct02TrnSelFlg;
    }

    public void setCt02TrnSelFlg(char v) {
        ct02TrnSelFlg = v;
    }

    public String getCt02TrnSelected() {
        return ct02TrnSelected;
    }

    public void setCt02TrnSelected(String v) {
        ct02TrnSelected = Pic.x(v, 16);
    }
}
