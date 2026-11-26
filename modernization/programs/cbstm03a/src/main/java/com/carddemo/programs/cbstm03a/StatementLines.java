package com.carddemo.programs.cbstm03a;

import com.carddemo.programs.cbstm03a.support.Pic;
import java.math.BigDecimal;

/**
 * STATEMENT-LINES (ST-LINE0 .. ST-LINE15), each exactly 80 bytes. Only the named
 * elementary items are state; FILLERs are constants (INITIALIZE never touches them).
 */
final class StatementLines {

    static final int LENGTH = 80;

    static final String ST_LINE0 = "*".repeat(31) + "START OF STATEMENT" + "*".repeat(31);
    static final String ST_LINE5 = "-".repeat(80);
    static final String ST_LINE6 = " ".repeat(33) + Pic.x("Basic Details", 14) + " ".repeat(33);
    static final String ST_LINE10 = "-".repeat(80);
    static final String ST_LINE11 = " ".repeat(30) + "TRANSACTION SUMMARY " + " ".repeat(30);
    static final String ST_LINE12 = "-".repeat(80);
    static final String ST_LINE13 = Pic.x("Tran ID         ", 16) + Pic.x("Tran Details    ", 51) + Pic.x("  Tran Amount", 13);
    static final String ST_LINE15 = "*".repeat(32) + "END OF STATEMENT" + "*".repeat(32);

    String stName;        // ST-NAME         PIC X(75)
    String stAdd1;        // ST-ADD1         PIC X(50)
    String stAdd2;        // ST-ADD2         PIC X(50)
    String stAdd3;        // ST-ADD3         PIC X(80)
    String stAcctId;      // ST-ACCT-ID      PIC X(20)
    String stCurrBal;     // ST-CURR-BAL     PIC 9(9).99-
    String stFicoScore;   // ST-FICO-SCORE   PIC X(20)
    String stTranid;      // ST-TRANID       PIC X(16)
    String stTrandt;      // ST-TRANDT       PIC X(49)
    String stTranamt;     // ST-TRANAMT      PIC Z(9).99-
    String stTotalTramt;  // ST-TOTAL-TRAMT  PIC Z(9).99-

    StatementLines() {
        initialize();
    }

    /** INITIALIZE STATEMENT-LINES: alphanumerics to SPACES, numeric-edited to ZERO. */
    void initialize() {
        stName = Pic.x("", 75);
        stAdd1 = Pic.x("", 50);
        stAdd2 = Pic.x("", 50);
        stAdd3 = Pic.x("", 80);
        stAcctId = Pic.x("", 20);
        stCurrBal = Pic.editNines(BigDecimal.ZERO, 9);
        stFicoScore = Pic.x("", 20);
        stTranid = Pic.x("", 16);
        stTrandt = Pic.x("", 49);
        stTranamt = Pic.editZeroSuppressed(BigDecimal.ZERO, 9);
        stTotalTramt = Pic.editZeroSuppressed(BigDecimal.ZERO, 9);
    }

    String stLine1() {
        return stName + " ".repeat(5);
    }

    String stLine2() {
        return stAdd1 + " ".repeat(30);
    }

    String stLine3() {
        return stAdd2 + " ".repeat(30);
    }

    String stLine4() {
        return stAdd3;
    }

    String stLine7() {
        return "Account ID         :" + stAcctId + " ".repeat(40);
    }

    String stLine8() {
        return "Current Balance    :" + stCurrBal + " ".repeat(7) + " ".repeat(40);
    }

    String stLine9() {
        return "FICO Score         :" + stFicoScore + " ".repeat(40);
    }

    String stLine14() {
        return stTranid + " " + stTrandt + "$" + stTranamt;
    }

    String stLine14a() {
        return "Total EXP:" + " ".repeat(56) + "$" + stTotalTramt;
    }
}
