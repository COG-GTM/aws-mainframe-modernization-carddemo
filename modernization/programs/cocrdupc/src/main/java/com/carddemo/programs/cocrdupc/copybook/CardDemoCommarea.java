package com.carddemo.programs.cocrdupc.copybook;

import static com.carddemo.programs.cocrdupc.Cobol.alnum;
import static com.carddemo.programs.cocrdupc.Cobol.displayValue;
import static com.carddemo.programs.cocrdupc.Cobol.lowValues;
import static com.carddemo.programs.cocrdupc.Cobol.numeric;
import static com.carddemo.programs.cocrdupc.Cobol.spaces;

/** COCOM01Y: 01 CARDDEMO-COMMAREA (160 bytes). */
public final class CardDemoCommarea {

    public static final int LENGTH = 160;

    // 05 CDEMO-GENERAL-INFO
    private String fromTranid;   // X(04)
    private String fromProgram;  // X(08)
    private String toTranid;     // X(04)
    private String toProgram;    // X(08)
    private String userId;       // X(08)
    private String userType;     // X(01)  88 ADMIN 'A' / USER 'U'
    private int pgmContext;      // 9(01)  88 ENTER 0 / REENTER 1
    // 05 CDEMO-CUSTOMER-INFO
    private long custId;         // 9(09)
    private String custFname;    // X(25)
    private String custMname;    // X(25)
    private String custLname;    // X(25)
    // 05 CDEMO-ACCOUNT-INFO
    private long acctId;         // 9(11)
    private String acctStatus;   // X(01)
    // 05 CDEMO-CARD-INFO
    private long cardNum;        // 9(16)
    // 05 CDEMO-MORE-INFO
    private String lastMap;      // X(7)
    private String lastMapset;   // X(7)

    private CardDemoCommarea() {
    }

    /** INITIALIZE CARDDEMO-COMMAREA: alphanumerics to SPACES, numerics to ZERO. */
    public static CardDemoCommarea initialized() {
        CardDemoCommarea c = new CardDemoCommarea();
        c.initialize();
        return c;
    }

    /** Working storage before any MOVE (no VALUE clauses): LOW-VALUES. */
    public static CardDemoCommarea uninitialized() {
        CardDemoCommarea c = new CardDemoCommarea();
        c.fromTranid = lowValues(4);
        c.fromProgram = lowValues(8);
        c.toTranid = lowValues(4);
        c.toProgram = lowValues(8);
        c.userId = lowValues(8);
        c.userType = lowValues(1);
        c.custFname = lowValues(25);
        c.custMname = lowValues(25);
        c.custLname = lowValues(25);
        c.acctStatus = lowValues(1);
        c.lastMap = lowValues(7);
        c.lastMapset = lowValues(7);
        return c;
    }

    public void initialize() {
        fromTranid = spaces(4);
        fromProgram = spaces(8);
        toTranid = spaces(4);
        toProgram = spaces(8);
        userId = spaces(8);
        userType = spaces(1);
        pgmContext = 0;
        custId = 0;
        custFname = spaces(25);
        custMname = spaces(25);
        custLname = spaces(25);
        acctId = 0;
        acctStatus = spaces(1);
        cardNum = 0;
        lastMap = spaces(7);
        lastMapset = spaces(7);
    }

    public static CardDemoCommarea decode(String raw) {
        String r = alnum(raw, LENGTH);
        CardDemoCommarea c = new CardDemoCommarea();
        c.fromTranid = r.substring(0, 4);
        c.fromProgram = r.substring(4, 12);
        c.toTranid = r.substring(12, 16);
        c.toProgram = r.substring(16, 24);
        c.userId = r.substring(24, 32);
        c.userType = r.substring(32, 33);
        c.pgmContext = (int) displayValue(r.substring(33, 34));
        c.custId = displayValue(r.substring(34, 43));
        c.custFname = r.substring(43, 68);
        c.custMname = r.substring(68, 93);
        c.custLname = r.substring(93, 118);
        c.acctId = displayValue(r.substring(118, 129));
        c.acctStatus = r.substring(129, 130);
        c.cardNum = displayValue(r.substring(130, 146));
        c.lastMap = r.substring(146, 153);
        c.lastMapset = r.substring(153, 160);
        return c;
    }

    public String encode() {
        return fromTranid + fromProgram + toTranid + toProgram + userId + userType
                + numeric(pgmContext, 1) + numeric(custId, 9) + custFname + custMname + custLname
                + numeric(acctId, 11) + acctStatus + numeric(cardNum, 16) + lastMap + lastMapset;
    }

    public CardDemoCommarea copy() {
        return decode(encode());
    }

    public String fromTranid() { return fromTranid; }
    public void setFromTranid(String v) { fromTranid = alnum(v, 4); }
    public String fromProgram() { return fromProgram; }
    public void setFromProgram(String v) { fromProgram = alnum(v, 8); }
    public String toTranid() { return toTranid; }
    public void setToTranid(String v) { toTranid = alnum(v, 4); }
    public String toProgram() { return toProgram; }
    public void setToProgram(String v) { toProgram = alnum(v, 8); }
    public String userId() { return userId; }
    public void setUserId(String v) { userId = alnum(v, 8); }
    public String userType() { return userType; }
    public void setUserType(String v) { userType = alnum(v, 1); }
    public boolean isUsrtypAdmin() { return "A".equals(userType); }
    public boolean isUsrtypUser() { return "U".equals(userType); }
    public void setUsrtypUser() { userType = "U"; }
    public void setUsrtypAdmin() { userType = "A"; }
    public int pgmContext() { return pgmContext; }
    public void setPgmContext(int v) { pgmContext = (int) (Math.abs(v) % 10); }
    public boolean isPgmEnter() { return pgmContext == 0; }
    public boolean isPgmReenter() { return pgmContext == 1; }
    public void setPgmEnter() { pgmContext = 0; }
    public void setPgmReenter() { pgmContext = 1; }
    public long custId() { return custId; }
    public void setCustId(long v) { custId = displayValue(numeric(v, 9)); }
    public String custFname() { return custFname; }
    public void setCustFname(String v) { custFname = alnum(v, 25); }
    public String custMname() { return custMname; }
    public void setCustMname(String v) { custMname = alnum(v, 25); }
    public String custLname() { return custLname; }
    public void setCustLname(String v) { custLname = alnum(v, 25); }
    public long acctId() { return acctId; }
    public void setAcctId(long v) { acctId = displayValue(numeric(v, 11)); }
    public String acctStatus() { return acctStatus; }
    public void setAcctStatus(String v) { acctStatus = alnum(v, 1); }
    public long cardNum() { return cardNum; }
    public void setCardNum(long v) { cardNum = displayValue(numeric(v, 16)); }
    public String lastMap() { return lastMap; }
    public void setLastMap(String v) { lastMap = alnum(v, 7); }
    public String lastMapset() { return lastMapset; }
    public void setLastMapset(String v) { lastMapset = alnum(v, 7); }
}
