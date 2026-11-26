package com.carddemo.programs.cocrdslc.copybook;

import com.carddemo.programs.cocrdslc.CobolText;
import com.carddemo.programs.cocrdslc.FixedWidthReader;

/**
 * COCOM01Y - CARDDEMO-COMMAREA (160 bytes). Mutable like the COBOL working-storage copy.
 * PIC 9 items are held as their raw DISPLAY bytes so non-numeric content (and the
 * WHEN OTHER branch it drives) survives round-tripping.
 */
public final class CarddemoCommarea {
    public static final int LENGTH = 160;

    // CDEMO-GENERAL-INFO
    public String cdemoFromTranid;   // X(04)
    public String cdemoFromProgram;  // X(08)
    public String cdemoToTranid;     // X(04)
    public String cdemoToProgram;    // X(08)
    public String cdemoUserId;       // X(08)
    public String cdemoUserType;     // X(01)
    public String cdemoPgmContext;   // 9(01)
    // CDEMO-CUSTOMER-INFO
    public String cdemoCustId;       // 9(09)
    public String cdemoCustFname;    // X(25)
    public String cdemoCustMname;    // X(25)
    public String cdemoCustLname;    // X(25)
    // CDEMO-ACCOUNT-INFO
    public String cdemoAcctId;       // 9(11)
    public String cdemoAcctStatus;   // X(01)
    // CDEMO-CARD-INFO
    public String cdemoCardNum;      // 9(16)
    // CDEMO-MORE-INFO
    public String cdemoLastMap;      // X(07)
    public String cdemoLastMapset;   // X(07)

    private CarddemoCommarea() {
    }

    /** Working storage with no VALUE clause, before any MOVE: LOW-VALUES throughout. */
    public static CarddemoCommarea lowValues() {
        return parse(CobolText.lowValues(LENGTH));
    }

    /** INITIALIZE CARDDEMO-COMMAREA: alphanumerics to SPACES, numerics to ZERO. */
    public static CarddemoCommarea initialized() {
        CarddemoCommarea c = new CarddemoCommarea();
        c.initialize();
        return c;
    }

    public void initialize() {
        cdemoFromTranid = CobolText.spaces(4);
        cdemoFromProgram = CobolText.spaces(8);
        cdemoToTranid = CobolText.spaces(4);
        cdemoToProgram = CobolText.spaces(8);
        cdemoUserId = CobolText.spaces(8);
        cdemoUserType = CobolText.spaces(1);
        cdemoPgmContext = CobolText.zoned(0, 1);
        cdemoCustId = CobolText.zoned(0, 9);
        cdemoCustFname = CobolText.spaces(25);
        cdemoCustMname = CobolText.spaces(25);
        cdemoCustLname = CobolText.spaces(25);
        cdemoAcctId = CobolText.zoned(0, 11);
        cdemoAcctStatus = CobolText.spaces(1);
        cdemoCardNum = CobolText.zoned(0, 16);
        cdemoLastMap = CobolText.spaces(7);
        cdemoLastMapset = CobolText.spaces(7);
    }

    public static CarddemoCommarea parse(String raw) {
        FixedWidthReader r = new FixedWidthReader(raw, LENGTH);
        CarddemoCommarea c = new CarddemoCommarea();
        c.cdemoFromTranid = r.x(4);
        c.cdemoFromProgram = r.x(8);
        c.cdemoToTranid = r.x(4);
        c.cdemoToProgram = r.x(8);
        c.cdemoUserId = r.x(8);
        c.cdemoUserType = r.x(1);
        c.cdemoPgmContext = r.x(1);
        c.cdemoCustId = r.x(9);
        c.cdemoCustFname = r.x(25);
        c.cdemoCustMname = r.x(25);
        c.cdemoCustLname = r.x(25);
        c.cdemoAcctId = r.x(11);
        c.cdemoAcctStatus = r.x(1);
        c.cdemoCardNum = r.x(16);
        c.cdemoLastMap = r.x(7);
        c.cdemoLastMapset = r.x(7);
        return c;
    }

    public String toFixedWidth() {
        return pic(cdemoFromTranid, 4) + pic(cdemoFromProgram, 8) + pic(cdemoToTranid, 4)
                + pic(cdemoToProgram, 8) + pic(cdemoUserId, 8) + pic(cdemoUserType, 1)
                + pic(cdemoPgmContext, 1) + pic(cdemoCustId, 9) + pic(cdemoCustFname, 25)
                + pic(cdemoCustMname, 25) + pic(cdemoCustLname, 25) + pic(cdemoAcctId, 11)
                + pic(cdemoAcctStatus, 1) + pic(cdemoCardNum, 16) + pic(cdemoLastMap, 7)
                + pic(cdemoLastMapset, 7);
    }

    private static String pic(String v, int len) {
        return CobolText.pic(v, len);
    }

    public CarddemoCommarea copy() {
        return parse(toFixedWidth());
    }

    // 88-levels
    public boolean isUsrtypAdmin() {
        return "A".equals(cdemoUserType);
    }

    public boolean isUsrtypUser() {
        return "U".equals(cdemoUserType);
    }

    public void setUsrtypUser() {
        cdemoUserType = "U";
    }

    public void setUsrtypAdmin() {
        cdemoUserType = "A";
    }

    /** 88 CDEMO-PGM-ENTER VALUE 0 (numeric comparison on a PIC 9(01) DISPLAY item). */
    public boolean isPgmEnter() {
        return CobolText.zonedValue(cdemoPgmContext) == 0;
    }

    /** 88 CDEMO-PGM-REENTER VALUE 1. */
    public boolean isPgmReenter() {
        return CobolText.zonedValue(cdemoPgmContext) == 1;
    }

    public void setPgmEnter() {
        cdemoPgmContext = "0";
    }

    public void setPgmReenter() {
        cdemoPgmContext = "1";
    }

    // numeric views
    public long acctId() {
        return CobolText.zonedValue(cdemoAcctId);
    }

    public void acctId(long v) {
        cdemoAcctId = CobolText.zoned(v, 11);
    }

    public long cardNum() {
        return CobolText.zonedValue(cdemoCardNum);
    }

    public void cardNum(long v) {
        cdemoCardNum = CobolText.zoned(v, 16);
    }

    public long custId() {
        return CobolText.zonedValue(cdemoCustId);
    }

    public void custId(long v) {
        cdemoCustId = CobolText.zoned(v, 9);
    }
}
