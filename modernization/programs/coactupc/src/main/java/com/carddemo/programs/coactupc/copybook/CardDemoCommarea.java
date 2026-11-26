package com.carddemo.programs.coactupc.copybook;

import com.carddemo.programs.coactupc.support.Cobol;
import com.carddemo.programs.coactupc.support.FixedWidth;

/** COCOM01Y - CARDDEMO-COMMAREA, the communication area shared by all CardDemo online programs. */
public final class CardDemoCommarea {

    public static final int LENGTH = 160;

    /** CDEMO-FROM-TRANID PIC X(04). */
    public String fromTranid;
    /** CDEMO-FROM-PROGRAM PIC X(08). */
    public String fromProgram;
    /** CDEMO-TO-TRANID PIC X(04). */
    public String toTranid;
    /** CDEMO-TO-PROGRAM PIC X(08). */
    public String toProgram;
    /** CDEMO-USER-ID PIC X(08). */
    public String userId;
    /** CDEMO-USER-TYPE PIC X(01): 88 CDEMO-USRTYP-ADMIN 'A', 88 CDEMO-USRTYP-USER 'U'. */
    public String userType;
    /** CDEMO-PGM-CONTEXT PIC 9(01): 88 CDEMO-PGM-ENTER 0, 88 CDEMO-PGM-REENTER 1. */
    public int pgmContext;
    /** CDEMO-CUST-ID PIC 9(09). */
    public long custId;
    /** CDEMO-CUST-FNAME PIC X(25). */
    public String custFname;
    /** CDEMO-CUST-MNAME PIC X(25). */
    public String custMname;
    /** CDEMO-CUST-LNAME PIC X(25). */
    public String custLname;
    /** CDEMO-ACCT-ID PIC 9(11). */
    public long acctId;
    /** CDEMO-ACCT-STATUS PIC X(01). */
    public String acctStatus;
    /** CDEMO-CARD-NUM PIC 9(16). */
    public long cardNum;
    /** CDEMO-LAST-MAP PIC X(7). */
    public String lastMap;
    /** CDEMO-LAST-MAPSET PIC X(7). */
    public String lastMapset;

    public enum UserType {
        ADMIN("A"), USER("U");

        public final String code;

        UserType(String code) {
            this.code = code;
        }
    }

    /** Storage with no VALUE clause in a fresh CICS task (all LOW-VALUES). */
    public static CardDemoCommarea lowValues() {
        return parse(Cobol.lowValues(LENGTH));
    }

    /** INITIALIZE CARDDEMO-COMMAREA: alphanumerics to SPACES, numerics to ZERO. */
    public static CardDemoCommarea initialized() {
        CardDemoCommarea c = new CardDemoCommarea();
        c.initialize();
        return c;
    }

    public void initialize() {
        fromTranid = Cobol.spaces(4);
        fromProgram = Cobol.spaces(8);
        toTranid = Cobol.spaces(4);
        toProgram = Cobol.spaces(8);
        userId = Cobol.spaces(8);
        userType = Cobol.spaces(1);
        pgmContext = 0;
        custId = 0;
        custFname = Cobol.spaces(25);
        custMname = Cobol.spaces(25);
        custLname = Cobol.spaces(25);
        acctId = 0;
        acctStatus = Cobol.spaces(1);
        cardNum = 0;
        lastMap = Cobol.spaces(7);
        lastMapset = Cobol.spaces(7);
    }

    public boolean isUsrtypAdmin() {
        return "A".equals(userType);
    }

    public boolean isUsrtypUser() {
        return "U".equals(userType);
    }

    public void setUsrtypUser() {
        userType = UserType.USER.code;
    }

    public boolean isPgmEnter() {
        return pgmContext == 0;
    }

    public boolean isPgmReenter() {
        return pgmContext == 1;
    }

    public void setPgmEnter() {
        pgmContext = 0;
    }

    public void setPgmReenter() {
        pgmContext = 1;
    }

    public static CardDemoCommarea parse(String image) {
        FixedWidth f = new FixedWidth(image, LENGTH);
        CardDemoCommarea c = new CardDemoCommarea();
        c.fromTranid = f.next(4);
        c.fromProgram = f.next(8);
        c.toTranid = f.next(4);
        c.toProgram = f.next(8);
        c.userId = f.next(8);
        c.userType = f.next(1);
        c.pgmContext = (int) Cobol.unsignedValue(f.next(1));
        c.custId = Cobol.unsignedValue(f.next(9));
        c.custFname = f.next(25);
        c.custMname = f.next(25);
        c.custLname = f.next(25);
        c.acctId = Cobol.unsignedValue(f.next(11));
        c.acctStatus = f.next(1);
        c.cardNum = Cobol.unsignedValue(f.next(16));
        c.lastMap = f.next(7);
        c.lastMapset = f.next(7);
        return c;
    }

    public String toFixedWidth() {
        return Cobol.pad(fromTranid, 4) + Cobol.pad(fromProgram, 8) + Cobol.pad(toTranid, 4)
                + Cobol.pad(toProgram, 8) + Cobol.pad(userId, 8) + Cobol.pad(userType, 1)
                + Cobol.unsignedDisplay(pgmContext, 1) + Cobol.unsignedDisplay(custId, 9)
                + Cobol.pad(custFname, 25) + Cobol.pad(custMname, 25) + Cobol.pad(custLname, 25)
                + Cobol.unsignedDisplay(acctId, 11) + Cobol.pad(acctStatus, 1) + Cobol.unsignedDisplay(cardNum, 16)
                + Cobol.pad(lastMap, 7) + Cobol.pad(lastMapset, 7);
    }

    public CardDemoCommarea copy() {
        CardDemoCommarea c = parse(toFixedWidth());
        return c;
    }
}
