package com.carddemo.programs.corpt00c.copybook;

import com.carddemo.programs.corpt00c.CobolText;

/**
 * COCOM01Y - 01 CARDDEMO-COMMAREA (160 bytes). Fields hold their raw display text so that a commarea
 * round-trips byte for byte; numeric PIC 9 fields also have typed accessors.
 */
public final class CarddemoCommarea {

    public static final int LENGTH = 160;

    // 05 CDEMO-GENERAL-INFO
    private String fromTranid = CobolText.spaces(4);      // X(04)
    private String fromProgram = CobolText.spaces(8);     // X(08)
    private String toTranid = CobolText.spaces(4);        // X(04)
    private String toProgram = CobolText.spaces(8);       // X(08)
    private String userId = CobolText.spaces(8);          // X(08)
    private String userType = CobolText.spaces(1);        // X(01)
    private String pgmContext = "0";                      // 9(01)
    // 05 CDEMO-CUSTOMER-INFO
    private String custId = "0".repeat(9);                // 9(09)
    private String custFname = CobolText.spaces(25);      // X(25)
    private String custMname = CobolText.spaces(25);      // X(25)
    private String custLname = CobolText.spaces(25);      // X(25)
    // 05 CDEMO-ACCOUNT-INFO
    private String acctId = "0".repeat(11);               // 9(11)
    private String acctStatus = CobolText.spaces(1);      // X(01)
    // 05 CDEMO-CARD-INFO
    private String cardNum = "0".repeat(16);              // 9(16)
    // 05 CDEMO-MORE-INFO
    private String lastMap = CobolText.spaces(7);         // X(7)
    private String lastMapset = CobolText.spaces(7);      // X(7)

    /** 88-level values of CDEMO-USER-TYPE. */
    public enum UserType {
        ADMIN("A"), USER("U");

        public final String value;

        UserType(String value) {
            this.value = value;
        }
    }

    /** 88-level values of CDEMO-PGM-CONTEXT. */
    public enum PgmContext {
        ENTER("0"), REENTER("1");

        public final String value;

        PgmContext(String value) {
            this.value = value;
        }
    }

    public static CarddemoCommarea fromFixedWidth(String raw) {
        String s = CobolText.move(raw, LENGTH);
        CarddemoCommarea c = new CarddemoCommarea();
        int[] p = {0};
        c.fromTranid = take(s, p, 4);
        c.fromProgram = take(s, p, 8);
        c.toTranid = take(s, p, 4);
        c.toProgram = take(s, p, 8);
        c.userId = take(s, p, 8);
        c.userType = take(s, p, 1);
        c.pgmContext = take(s, p, 1);
        c.custId = take(s, p, 9);
        c.custFname = take(s, p, 25);
        c.custMname = take(s, p, 25);
        c.custLname = take(s, p, 25);
        c.acctId = take(s, p, 11);
        c.acctStatus = take(s, p, 1);
        c.cardNum = take(s, p, 16);
        c.lastMap = take(s, p, 7);
        c.lastMapset = take(s, p, 7);
        return c;
    }

    private static String take(String s, int[] pos, int len) {
        String v = s.substring(pos[0], pos[0] + len);
        pos[0] += len;
        return v;
    }

    public String toFixedWidth() {
        return fromTranid + fromProgram + toTranid + toProgram + userId + userType + pgmContext
                + custId + custFname + custMname + custLname
                + acctId + acctStatus + cardNum + lastMap + lastMapset;
    }

    public CarddemoCommarea copy() {
        return fromFixedWidth(toFixedWidth());
    }

    private static String num(long v, int digits) {
        return CobolText.toUnsignedDisplay(java.math.BigDecimal.valueOf(v), digits);
    }

    public String fromTranid() { return fromTranid; }
    public void setFromTranid(String v) { fromTranid = CobolText.move(v, 4); }
    public String fromProgram() { return fromProgram; }
    public void setFromProgram(String v) { fromProgram = CobolText.move(v, 8); }
    public String toTranid() { return toTranid; }
    public void setToTranid(String v) { toTranid = CobolText.move(v, 4); }
    public String toProgram() { return toProgram; }
    public void setToProgram(String v) { toProgram = CobolText.move(v, 8); }
    public String userId() { return userId; }
    public void setUserId(String v) { userId = CobolText.move(v, 8); }
    public String userType() { return userType; }
    public void setUserType(String v) { userType = CobolText.move(v, 1); }
    public String pgmContext() { return pgmContext; }
    /** MOVE n TO CDEMO-PGM-CONTEXT (PIC 9(01)). */
    public void setPgmContext(int v) { pgmContext = num(v, 1); }
    public long custId() { return Long.parseLong(custId); }
    public String custIdText() { return custId; }
    public void setCustId(long v) { custId = num(v, 9); }
    public String custFname() { return custFname; }
    public void setCustFname(String v) { custFname = CobolText.move(v, 25); }
    public String custMname() { return custMname; }
    public void setCustMname(String v) { custMname = CobolText.move(v, 25); }
    public String custLname() { return custLname; }
    public void setCustLname(String v) { custLname = CobolText.move(v, 25); }
    public long acctId() { return Long.parseLong(acctId); }
    public String acctIdText() { return acctId; }
    public void setAcctId(long v) { acctId = num(v, 11); }
    public String acctStatus() { return acctStatus; }
    public void setAcctStatus(String v) { acctStatus = CobolText.move(v, 1); }
    public long cardNum() { return Long.parseLong(cardNum); }
    public String cardNumText() { return cardNum; }
    public void setCardNum(long v) { cardNum = num(v, 16); }
    public String lastMap() { return lastMap; }
    public void setLastMap(String v) { lastMap = CobolText.move(v, 7); }
    public String lastMapset() { return lastMapset; }
    public void setLastMapset(String v) { lastMapset = CobolText.move(v, 7); }

    // 88-levels
    public boolean isUsrtypAdmin() { return UserType.ADMIN.value.equals(userType); }
    public boolean isUsrtypUser() { return UserType.USER.value.equals(userType); }
    public boolean isPgmEnter() { return PgmContext.ENTER.value.equals(pgmContext); }
    public boolean isPgmReenter() { return PgmContext.REENTER.value.equals(pgmContext); }
    /** SET CDEMO-PGM-REENTER TO TRUE. */
    public void setPgmReenter() { pgmContext = PgmContext.REENTER.value; }
    /** SET CDEMO-PGM-ENTER TO TRUE. */
    public void setPgmEnter() { pgmContext = PgmContext.ENTER.value; }
}
