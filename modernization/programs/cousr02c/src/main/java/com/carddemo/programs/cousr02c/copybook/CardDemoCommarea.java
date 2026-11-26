package com.carddemo.programs.cousr02c.copybook;

import java.util.OptionalLong;

/**
 * COCOM01Y - {@code 01 CARDDEMO-COMMAREA} (160 bytes), extended by COUSR02C with {@code 05 CDEMO-CU02-INFO}
 * (34 bytes) for a total COMMAREA of {@value #LENGTH} bytes.
 *
 * <p>Fields are held as raw display strings of their exact PIC width so a COMMAREA received from another program
 * round-trips byte for byte (including non-numeric content in PIC 9 fields); typed accessors are provided for
 * numerics and 88-levels.
 */
public final class CardDemoCommarea {

    public static final int GENERAL_INFO_LENGTH = 34;
    public static final int BASE_LENGTH = 160;
    public static final int CU02_INFO_LENGTH = 34;
    public static final int LENGTH = BASE_LENGTH + CU02_INFO_LENGTH;

    /** 88 CDEMO-USRTYP-ADMIN / CDEMO-USRTYP-USER on CDEMO-USER-TYPE. */
    public enum UserType {
        ADMIN('A'), USER('U');

        private final char value;

        UserType(char value) {
            this.value = value;
        }

        public char value() {
            return value;
        }
    }

    /** 88 CDEMO-PGM-ENTER / CDEMO-PGM-REENTER on CDEMO-PGM-CONTEXT PIC 9(01). */
    public enum PgmContext {
        ENTER('0'), REENTER('1');

        private final char value;

        PgmContext(char value) {
            this.value = value;
        }

        public char value() {
            return value;
        }
    }

    /** 88 NEXT-PAGE-YES / NEXT-PAGE-NO on CDEMO-CU02-NEXT-PAGE-FLG. */
    public enum NextPage {
        YES('Y'), NO('N');

        private final char value;

        NextPage(char value) {
            this.value = value;
        }

        public char value() {
            return value;
        }
    }

    // 05 CDEMO-GENERAL-INFO
    private String cdemoFromTranid;    // X(04)
    private String cdemoFromProgram;   // X(08)
    private String cdemoToTranid;      // X(04)
    private String cdemoToProgram;     // X(08)
    private String cdemoUserId;        // X(08)
    private String cdemoUserType;      // X(01)
    private String cdemoPgmContext;    // 9(01)
    // 05 CDEMO-CUSTOMER-INFO
    private String cdemoCustId;        // 9(09)
    private String cdemoCustFname;     // X(25)
    private String cdemoCustMname;     // X(25)
    private String cdemoCustLname;     // X(25)
    // 05 CDEMO-ACCOUNT-INFO
    private String cdemoAcctId;        // 9(11)
    private String cdemoAcctStatus;    // X(01)
    // 05 CDEMO-CARD-INFO
    private String cdemoCardNum;       // 9(16)
    // 05 CDEMO-MORE-INFO
    private String cdemoLastMap;       // X(7)
    private String cdemoLastMapset;    // X(7)
    // 05 CDEMO-CU02-INFO (COUSR02C working storage)
    private String cdemoCu02UsridFirst;   // X(08)
    private String cdemoCu02UsridLast;    // X(08)
    private String cdemoCu02PageNum;      // 9(08)
    private String cdemoCu02NextPageFlg;  // X(01) VALUE 'N'
    private String cdemoCu02UsrSelFlg;    // X(01)
    private String cdemoCu02UsrSelected;  // X(08)

    private CardDemoCommarea(String raw) {
        int[] p = {0};
        cdemoFromTranid = take(raw, p, 4);
        cdemoFromProgram = take(raw, p, 8);
        cdemoToTranid = take(raw, p, 4);
        cdemoToProgram = take(raw, p, 8);
        cdemoUserId = take(raw, p, 8);
        cdemoUserType = take(raw, p, 1);
        cdemoPgmContext = take(raw, p, 1);
        cdemoCustId = take(raw, p, 9);
        cdemoCustFname = take(raw, p, 25);
        cdemoCustMname = take(raw, p, 25);
        cdemoCustLname = take(raw, p, 25);
        cdemoAcctId = take(raw, p, 11);
        cdemoAcctStatus = take(raw, p, 1);
        cdemoCardNum = take(raw, p, 16);
        cdemoLastMap = take(raw, p, 7);
        cdemoLastMapset = take(raw, p, 7);
        cdemoCu02UsridFirst = take(raw, p, 8);
        cdemoCu02UsridLast = take(raw, p, 8);
        cdemoCu02PageNum = take(raw, p, 8);
        cdemoCu02NextPageFlg = take(raw, p, 1);
        cdemoCu02UsrSelFlg = take(raw, p, 1);
        cdemoCu02UsrSelected = take(raw, p, 8);
    }

    private static String take(String raw, int[] pos, int len) {
        String s = raw.substring(pos[0], pos[0] + len);
        pos[0] += len;
        return s;
    }

    /**
     * {@code MOVE DFHCOMMAREA(1:EIBCALEN) TO CARDDEMO-COMMAREA}: alphanumeric group move, so a shorter COMMAREA is
     * space-padded and a longer one truncated.
     */
    public static CardDemoCommarea fromBytes(String dfhcommarea) {
        return new CardDemoCommarea(Pic.x(dfhcommarea, LENGTH));
    }

    /**
     * Initial working-storage image: no VALUE clauses (modelled as LOW-VALUES) except
     * {@code CDEMO-CU02-NEXT-PAGE-FLG VALUE 'N'}.
     */
    public static CardDemoCommarea initialWorkingStorage() {
        CardDemoCommarea c = new CardDemoCommarea(Pic.lowValues(LENGTH));
        c.cdemoCu02NextPageFlg = "N";
        return c;
    }

    /** A space-filled COMMAREA, convenient for building caller COMMAREAs. */
    public static CardDemoCommarea blank() {
        return fromBytes("");
    }

    public String toBytes() {
        return cdemoFromTranid + cdemoFromProgram + cdemoToTranid + cdemoToProgram + cdemoUserId + cdemoUserType
                + cdemoPgmContext + cdemoCustId + cdemoCustFname + cdemoCustMname + cdemoCustLname + cdemoAcctId
                + cdemoAcctStatus + cdemoCardNum + cdemoLastMap + cdemoLastMapset + cdemoCu02UsridFirst
                + cdemoCu02UsridLast + cdemoCu02PageNum + cdemoCu02NextPageFlg + cdemoCu02UsrSelFlg
                + cdemoCu02UsrSelected;
    }

    public CardDemoCommarea copy() {
        return new CardDemoCommarea(toBytes());
    }

    private static OptionalLong numeric(String v) {
        return v.chars().allMatch(Character::isDigit) ? OptionalLong.of(Long.parseLong(v)) : OptionalLong.empty();
    }

    private static String pic9(long v, int width) {
        if (v < 0) {
            throw new IllegalArgumentException("unsigned PIC 9 cannot hold " + v);
        }
        String s = Long.toString(v);
        return s.length() >= width ? s.substring(s.length() - width) : "0".repeat(width - s.length()) + s;
    }

    // --- 88-level predicates / SET ... TO TRUE ---

    public boolean isCdemoUsrtypAdmin() {
        return cdemoUserType.charAt(0) == UserType.ADMIN.value();
    }

    public boolean isCdemoUsrtypUser() {
        return cdemoUserType.charAt(0) == UserType.USER.value();
    }

    public boolean isCdemoPgmEnter() {
        return cdemoPgmContext.charAt(0) == PgmContext.ENTER.value();
    }

    public boolean isCdemoPgmReenter() {
        return cdemoPgmContext.charAt(0) == PgmContext.REENTER.value();
    }

    public void setPgmContext(PgmContext ctx) {
        cdemoPgmContext = String.valueOf(ctx.value());
    }

    public boolean isNextPageYes() {
        return cdemoCu02NextPageFlg.charAt(0) == NextPage.YES.value();
    }

    public boolean isNextPageNo() {
        return cdemoCu02NextPageFlg.charAt(0) == NextPage.NO.value();
    }

    // --- accessors (setters apply MOVE truncation/padding) ---

    public String getCdemoFromTranid() {
        return cdemoFromTranid;
    }

    public void setCdemoFromTranid(String v) {
        cdemoFromTranid = Pic.x(v, 4);
    }

    public String getCdemoFromProgram() {
        return cdemoFromProgram;
    }

    public void setCdemoFromProgram(String v) {
        cdemoFromProgram = Pic.x(v, 8);
    }

    public String getCdemoToTranid() {
        return cdemoToTranid;
    }

    public void setCdemoToTranid(String v) {
        cdemoToTranid = Pic.x(v, 4);
    }

    public String getCdemoToProgram() {
        return cdemoToProgram;
    }

    public void setCdemoToProgram(String v) {
        cdemoToProgram = Pic.x(v, 8);
    }

    public String getCdemoUserId() {
        return cdemoUserId;
    }

    public void setCdemoUserId(String v) {
        cdemoUserId = Pic.x(v, 8);
    }

    public String getCdemoUserType() {
        return cdemoUserType;
    }

    public void setCdemoUserType(String v) {
        cdemoUserType = Pic.x(v, 1);
    }

    public String getCdemoPgmContext() {
        return cdemoPgmContext;
    }

    public void setCdemoPgmContext(int v) {
        cdemoPgmContext = pic9(v, 1);
    }

    public String getCdemoCustId() {
        return cdemoCustId;
    }

    public OptionalLong cdemoCustIdValue() {
        return numeric(cdemoCustId);
    }

    public void setCdemoCustId(long v) {
        cdemoCustId = pic9(v, 9);
    }

    public String getCdemoCustFname() {
        return cdemoCustFname;
    }

    public void setCdemoCustFname(String v) {
        cdemoCustFname = Pic.x(v, 25);
    }

    public String getCdemoCustMname() {
        return cdemoCustMname;
    }

    public void setCdemoCustMname(String v) {
        cdemoCustMname = Pic.x(v, 25);
    }

    public String getCdemoCustLname() {
        return cdemoCustLname;
    }

    public void setCdemoCustLname(String v) {
        cdemoCustLname = Pic.x(v, 25);
    }

    public String getCdemoAcctId() {
        return cdemoAcctId;
    }

    public OptionalLong cdemoAcctIdValue() {
        return numeric(cdemoAcctId);
    }

    public void setCdemoAcctId(long v) {
        cdemoAcctId = pic9(v, 11);
    }

    public String getCdemoAcctStatus() {
        return cdemoAcctStatus;
    }

    public void setCdemoAcctStatus(String v) {
        cdemoAcctStatus = Pic.x(v, 1);
    }

    public String getCdemoCardNum() {
        return cdemoCardNum;
    }

    public OptionalLong cdemoCardNumValue() {
        return numeric(cdemoCardNum);
    }

    public void setCdemoCardNum(long v) {
        cdemoCardNum = pic9(v, 16);
    }

    public String getCdemoLastMap() {
        return cdemoLastMap;
    }

    public void setCdemoLastMap(String v) {
        cdemoLastMap = Pic.x(v, 7);
    }

    public String getCdemoLastMapset() {
        return cdemoLastMapset;
    }

    public void setCdemoLastMapset(String v) {
        cdemoLastMapset = Pic.x(v, 7);
    }

    public String getCdemoCu02UsridFirst() {
        return cdemoCu02UsridFirst;
    }

    public void setCdemoCu02UsridFirst(String v) {
        cdemoCu02UsridFirst = Pic.x(v, 8);
    }

    public String getCdemoCu02UsridLast() {
        return cdemoCu02UsridLast;
    }

    public void setCdemoCu02UsridLast(String v) {
        cdemoCu02UsridLast = Pic.x(v, 8);
    }

    public String getCdemoCu02PageNum() {
        return cdemoCu02PageNum;
    }

    public OptionalLong cdemoCu02PageNumValue() {
        return numeric(cdemoCu02PageNum);
    }

    public void setCdemoCu02PageNum(long v) {
        cdemoCu02PageNum = pic9(v, 8);
    }

    public String getCdemoCu02NextPageFlg() {
        return cdemoCu02NextPageFlg;
    }

    public void setCdemoCu02NextPageFlg(String v) {
        cdemoCu02NextPageFlg = Pic.x(v, 1);
    }

    public String getCdemoCu02UsrSelFlg() {
        return cdemoCu02UsrSelFlg;
    }

    public void setCdemoCu02UsrSelFlg(String v) {
        cdemoCu02UsrSelFlg = Pic.x(v, 1);
    }

    public String getCdemoCu02UsrSelected() {
        return cdemoCu02UsrSelected;
    }

    public void setCdemoCu02UsrSelected(String v) {
        cdemoCu02UsrSelected = Pic.x(v, 8);
    }
}
