package com.carddemo.programs.copaus0c.copybook;

import com.carddemo.programs.copaus0c.cobol.Cobol;

import java.util.Arrays;

/**
 * COCOM01Y CARDDEMO-COMMAREA plus the COPAUS0C-local extension CDEMO-CPVS-INFO.
 * Mutable, fixed-width fields exactly like the COBOL storage; PIC 9 display fields are kept as digit
 * strings so {@code IS NUMERIC} tests behave as in COBOL.
 */
public final class CardDemoCommarea {
    // CDEMO-GENERAL-INFO
    public String cdemoFromTranid = Cobol.lowValues(4);   // PIC X(04)
    public String cdemoFromProgram = Cobol.lowValues(8);  // PIC X(08)
    public String cdemoToTranid = Cobol.lowValues(4);     // PIC X(04)
    public String cdemoToProgram = Cobol.lowValues(8);    // PIC X(08)
    public String cdemoUserId = Cobol.lowValues(8);       // PIC X(08)
    public String cdemoUserType = Cobol.lowValues(1);     // PIC X(01) 88 ADMIN 'A' / USER 'U'
    public int cdemoPgmContext;                           // PIC 9(01) 88 ENTER 0 / REENTER 1
    // CDEMO-CUSTOMER-INFO
    public String cdemoCustId = "000000000";              // PIC 9(09)
    public String cdemoCustFname = Cobol.lowValues(25);   // PIC X(25)
    public String cdemoCustMname = Cobol.lowValues(25);   // PIC X(25)
    public String cdemoCustLname = Cobol.lowValues(25);   // PIC X(25)
    // CDEMO-ACCOUNT-INFO
    public String cdemoAcctId = "00000000000";            // PIC 9(11)
    public String cdemoAcctStatus = Cobol.lowValues(1);   // PIC X(01)
    // CDEMO-CARD-INFO
    public String cdemoCardNum = "0000000000000000";      // PIC 9(16)
    // CDEMO-MORE-INFO
    public String cdemoLastMap = Cobol.lowValues(7);      // PIC X(7)
    public String cdemoLastMapset = Cobol.lowValues(7);   // PIC X(7)
    // CDEMO-CPVS-INFO (COPAUS0C extension)
    public String cdemoCpvsPauSelFlg = Cobol.lowValues(1);    // PIC X(01)
    public String cdemoCpvsPauSelected = Cobol.lowValues(8);  // PIC X(08)
    public final String[] cdemoCpvsPaukeyPrevPg = filled(20); // PIC X(08) OCCURS 20 TIMES
    public String cdemoCpvsPaukeyLast = Cobol.lowValues(8);   // PIC X(08)
    public int cdemoCpvsPageNum;                              // PIC S9(04) COMP
    public String cdemoCpvsNextPageFlg = "N";                 // PIC X(01) VALUE 'N'
    public final String[] cdemoCpvsAuthKeys = filled(5);      // PIC X(08) OCCURS 5 TIMES

    public enum UserType {
        CDEMO_USRTYP_ADMIN("A"), CDEMO_USRTYP_USER("U");

        public final String value;

        UserType(String value) {
            this.value = value;
        }
    }

    public static final int CDEMO_PGM_ENTER = 0;
    public static final int CDEMO_PGM_REENTER = 1;

    private static String[] filled(int n) {
        String[] a = new String[n];
        Arrays.fill(a, Cobol.lowValues(8));
        return a;
    }

    public boolean isCdemoPgmReenter() {
        return cdemoPgmContext == CDEMO_PGM_REENTER;
    }

    public boolean isNextPageYes() {
        return "Y".equals(cdemoCpvsNextPageFlg);
    }

    public boolean is(UserType type) {
        return type.value.equals(cdemoUserType);
    }

    /** 1-based OCCURS access, as in COBOL. */
    public String paukeyPrevPg(int index) {
        return cdemoCpvsPaukeyPrevPg[index - 1];
    }

    public void paukeyPrevPg(int index, String value) {
        cdemoCpvsPaukeyPrevPg[index - 1] = Cobol.moveX(value, 8);
    }

    public String authKey(int index) {
        return cdemoCpvsAuthKeys[index - 1];
    }

    public void authKey(int index, String value) {
        cdemoCpvsAuthKeys[index - 1] = Cobol.moveX(value, 8);
    }

    /** INITIALIZE CARDDEMO-COMMAREA: alphanumerics to spaces, numerics to zero (VALUE clauses ignored). */
    public void initialize() {
        cdemoFromTranid = Cobol.spaces(4);
        cdemoFromProgram = Cobol.spaces(8);
        cdemoToTranid = Cobol.spaces(4);
        cdemoToProgram = Cobol.spaces(8);
        cdemoUserId = Cobol.spaces(8);
        cdemoUserType = Cobol.spaces(1);
        cdemoPgmContext = 0;
        cdemoCustId = "0".repeat(9);
        cdemoCustFname = Cobol.spaces(25);
        cdemoCustMname = Cobol.spaces(25);
        cdemoCustLname = Cobol.spaces(25);
        cdemoAcctId = "0".repeat(11);
        cdemoAcctStatus = Cobol.spaces(1);
        cdemoCardNum = "0".repeat(16);
        cdemoLastMap = Cobol.spaces(7);
        cdemoLastMapset = Cobol.spaces(7);
        cdemoCpvsPauSelFlg = Cobol.spaces(1);
        cdemoCpvsPauSelected = Cobol.spaces(8);
        Arrays.fill(cdemoCpvsPaukeyPrevPg, Cobol.spaces(8));
        cdemoCpvsPaukeyLast = Cobol.spaces(8);
        cdemoCpvsPageNum = 0;
        cdemoCpvsNextPageFlg = Cobol.spaces(1);
        Arrays.fill(cdemoCpvsAuthKeys, Cobol.spaces(8));
    }

    public CardDemoCommarea copy() {
        CardDemoCommarea c = new CardDemoCommarea();
        c.cdemoFromTranid = cdemoFromTranid;
        c.cdemoFromProgram = cdemoFromProgram;
        c.cdemoToTranid = cdemoToTranid;
        c.cdemoToProgram = cdemoToProgram;
        c.cdemoUserId = cdemoUserId;
        c.cdemoUserType = cdemoUserType;
        c.cdemoPgmContext = cdemoPgmContext;
        c.cdemoCustId = cdemoCustId;
        c.cdemoCustFname = cdemoCustFname;
        c.cdemoCustMname = cdemoCustMname;
        c.cdemoCustLname = cdemoCustLname;
        c.cdemoAcctId = cdemoAcctId;
        c.cdemoAcctStatus = cdemoAcctStatus;
        c.cdemoCardNum = cdemoCardNum;
        c.cdemoLastMap = cdemoLastMap;
        c.cdemoLastMapset = cdemoLastMapset;
        c.cdemoCpvsPauSelFlg = cdemoCpvsPauSelFlg;
        c.cdemoCpvsPauSelected = cdemoCpvsPauSelected;
        System.arraycopy(cdemoCpvsPaukeyPrevPg, 0, c.cdemoCpvsPaukeyPrevPg, 0, 20);
        c.cdemoCpvsPaukeyLast = cdemoCpvsPaukeyLast;
        c.cdemoCpvsPageNum = cdemoCpvsPageNum;
        c.cdemoCpvsNextPageFlg = cdemoCpvsNextPageFlg;
        System.arraycopy(cdemoCpvsAuthKeys, 0, c.cdemoCpvsAuthKeys, 0, 5);
        return c;
    }
}
