package com.carddemo.programs.cobil00c.copybook;

/**
 * COCOM01Y {@code CARDDEMO-COMMAREA} (160 bytes) extended in COBIL00C working storage with
 * {@code CDEMO-CB00-INFO} (58 bytes), 218 bytes in total. Fields are held as their raw fixed-width
 * display bytes because the COMMAREA is a byte buffer passed between programs.
 */
public final class CardDemoCommarea {

    public static final int BASE_LENGTH = 160;
    public static final int CB00_INFO_LENGTH = 58;
    public static final int LENGTH = BASE_LENGTH + CB00_INFO_LENGTH;

    /** 88-levels on CDEMO-USER-TYPE. */
    public enum UserType {
        ADMIN('A'), USER('U');

        private final char code;

        UserType(char code) {
            this.code = code;
        }

        public char code() {
            return code;
        }
    }

    // 05 CDEMO-GENERAL-INFO
    private String fromTranId = Cobol.spaces(4);   // PIC X(04)
    private String fromProgram = Cobol.spaces(8);  // PIC X(08)
    private String toTranId = Cobol.spaces(4);     // PIC X(04)
    private String toProgram = Cobol.spaces(8);    // PIC X(08)
    private String userId = Cobol.spaces(8);       // PIC X(08)
    private String userType = Cobol.spaces(1);     // PIC X(01)
    private String pgmContext = Cobol.spaces(1);   // PIC 9(01)
    // 05 CDEMO-CUSTOMER-INFO
    private String custId = Cobol.spaces(9);       // PIC 9(09)
    private String custFname = Cobol.spaces(25);   // PIC X(25)
    private String custMname = Cobol.spaces(25);   // PIC X(25)
    private String custLname = Cobol.spaces(25);   // PIC X(25)
    // 05 CDEMO-ACCOUNT-INFO
    private String acctId = Cobol.spaces(11);      // PIC 9(11)
    private String acctStatus = Cobol.spaces(1);   // PIC X(01)
    // 05 CDEMO-CARD-INFO
    private String cardNum = Cobol.spaces(16);     // PIC 9(16)
    // 05 CDEMO-MORE-INFO
    private String lastMap = Cobol.spaces(7);      // PIC X(7)
    private String lastMapset = Cobol.spaces(7);   // PIC X(7)
    // 05 CDEMO-CB00-INFO (COBIL00C extension)
    private String cb00TrnidFirst = Cobol.spaces(16); // PIC X(16)
    private String cb00TrnidLast = Cobol.spaces(16);  // PIC X(16)
    private String cb00PageNum = Cobol.spaces(8);     // PIC 9(08)
    private String cb00NextPageFlg = "N";             // PIC X(01) VALUE 'N'
    private String cb00TrnSelFlg = Cobol.spaces(1);   // PIC X(01)
    private String cb00TrnSelected = Cobol.spaces(16); // PIC X(16)

    private static final int[] WIDTHS = {4, 8, 4, 8, 8, 1, 1, 9, 25, 25, 25, 11, 1, 16, 7, 7, 16, 16, 8, 1, 1, 16};

    public CardDemoCommarea() {
    }

    /**
     * {@code MOVE DFHCOMMAREA(1:EIBCALEN) TO CARDDEMO-COMMAREA}: a group (alphanumeric) move, so a
     * shorter COMMAREA is padded with spaces and a longer one is truncated.
     */
    public static CardDemoCommarea fromBytes(String dfhcommarea, int eibcalen) {
        String src = dfhcommarea.substring(0, Math.min(eibcalen, dfhcommarea.length()));
        String buf = Cobol.alnum(src, LENGTH);
        String[] f = new String[WIDTHS.length];
        int pos = 0;
        for (int i = 0; i < WIDTHS.length; i++) {
            f[i] = buf.substring(pos, pos + WIDTHS[i]);
            pos += WIDTHS[i];
        }
        CardDemoCommarea c = new CardDemoCommarea();
        c.fromTranId = f[0];
        c.fromProgram = f[1];
        c.toTranId = f[2];
        c.toProgram = f[3];
        c.userId = f[4];
        c.userType = f[5];
        c.pgmContext = f[6];
        c.custId = f[7];
        c.custFname = f[8];
        c.custMname = f[9];
        c.custLname = f[10];
        c.acctId = f[11];
        c.acctStatus = f[12];
        c.cardNum = f[13];
        c.lastMap = f[14];
        c.lastMapset = f[15];
        c.cb00TrnidFirst = f[16];
        c.cb00TrnidLast = f[17];
        c.cb00PageNum = f[18];
        c.cb00NextPageFlg = f[19];
        c.cb00TrnSelFlg = f[20];
        c.cb00TrnSelected = f[21];
        return c;
    }

    public String toBytes() {
        return fromTranId + fromProgram + toTranId + toProgram + userId + userType + pgmContext
                + custId + custFname + custMname + custLname + acctId + acctStatus + cardNum
                + lastMap + lastMapset + cb00TrnidFirst + cb00TrnidLast + cb00PageNum
                + cb00NextPageFlg + cb00TrnSelFlg + cb00TrnSelected;
    }

    // 88 CDEMO-USRTYP-ADMIN / CDEMO-USRTYP-USER
    public boolean isUserTypeAdmin() {
        return userType.charAt(0) == UserType.ADMIN.code();
    }

    public boolean isUserTypeUser() {
        return userType.charAt(0) == UserType.USER.code();
    }

    // 88 CDEMO-PGM-ENTER VALUE 0 / CDEMO-PGM-REENTER VALUE 1
    public boolean isPgmEnter() {
        return "0".equals(pgmContext);
    }

    public boolean isPgmReenter() {
        return "1".equals(pgmContext);
    }

    public void setPgmReenter() {
        pgmContext = "1";
    }

    public void setPgmEnter() {
        pgmContext = "0";
    }

    // 88 NEXT-PAGE-YES / NEXT-PAGE-NO
    public boolean isNextPageYes() {
        return "Y".equals(cb00NextPageFlg);
    }

    public boolean isNextPageNo() {
        return "N".equals(cb00NextPageFlg);
    }

    public String fromTranId() { return fromTranId; }
    public void fromTranId(String v) { fromTranId = Cobol.alnum(v, 4); }
    public String fromProgram() { return fromProgram; }
    public void fromProgram(String v) { fromProgram = Cobol.alnum(v, 8); }
    public String toTranId() { return toTranId; }
    public void toTranId(String v) { toTranId = Cobol.alnum(v, 4); }
    public String toProgram() { return toProgram; }
    public void toProgram(String v) { toProgram = Cobol.alnum(v, 8); }
    public String userId() { return userId; }
    public void userId(String v) { userId = Cobol.alnum(v, 8); }
    public String userType() { return userType; }
    public void userType(String v) { userType = Cobol.alnum(v, 1); }
    public String pgmContext() { return pgmContext; }
    public void pgmContext(String v) { pgmContext = Cobol.alnum(v, 1); }
    public String custId() { return custId; }
    public void custId(String v) { custId = Cobol.alnum(v, 9); }
    public String custFname() { return custFname; }
    public void custFname(String v) { custFname = Cobol.alnum(v, 25); }
    public String custMname() { return custMname; }
    public void custMname(String v) { custMname = Cobol.alnum(v, 25); }
    public String custLname() { return custLname; }
    public void custLname(String v) { custLname = Cobol.alnum(v, 25); }
    public String acctId() { return acctId; }
    public void acctId(String v) { acctId = Cobol.alnum(v, 11); }
    public String acctStatus() { return acctStatus; }
    public void acctStatus(String v) { acctStatus = Cobol.alnum(v, 1); }
    public String cardNum() { return cardNum; }
    public void cardNum(String v) { cardNum = Cobol.alnum(v, 16); }
    public String lastMap() { return lastMap; }
    public void lastMap(String v) { lastMap = Cobol.alnum(v, 7); }
    public String lastMapset() { return lastMapset; }
    public void lastMapset(String v) { lastMapset = Cobol.alnum(v, 7); }
    public String cb00TrnidFirst() { return cb00TrnidFirst; }
    public void cb00TrnidFirst(String v) { cb00TrnidFirst = Cobol.alnum(v, 16); }
    public String cb00TrnidLast() { return cb00TrnidLast; }
    public void cb00TrnidLast(String v) { cb00TrnidLast = Cobol.alnum(v, 16); }
    public String cb00PageNum() { return cb00PageNum; }
    public void cb00PageNum(String v) { cb00PageNum = Cobol.alnum(v, 8); }
    public String cb00NextPageFlg() { return cb00NextPageFlg; }
    public void cb00NextPageFlg(String v) { cb00NextPageFlg = Cobol.alnum(v, 1); }
    public String cb00TrnSelFlg() { return cb00TrnSelFlg; }
    public void cb00TrnSelFlg(String v) { cb00TrnSelFlg = Cobol.alnum(v, 1); }
    public String cb00TrnSelected() { return cb00TrnSelected; }
    public void cb00TrnSelected(String v) { cb00TrnSelected = Cobol.alnum(v, 16); }
}
