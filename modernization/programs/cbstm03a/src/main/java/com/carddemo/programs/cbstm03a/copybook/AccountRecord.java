package com.carddemo.programs.cbstm03a.copybook;

import com.carddemo.programs.cbstm03a.support.Pic;
import java.math.BigDecimal;

/** CVACT01Y - ACCOUNT-RECORD (RECLN 300). */
public record AccountRecord(
        long acctId,                     // ACCT-ID                 PIC 9(11)
        String acctActiveStatus,         // ACCT-ACTIVE-STATUS      PIC X(01)
        BigDecimal acctCurrBal,          // ACCT-CURR-BAL           PIC S9(10)V99
        BigDecimal acctCreditLimit,      // ACCT-CREDIT-LIMIT       PIC S9(10)V99
        BigDecimal acctCashCreditLimit,  // ACCT-CASH-CREDIT-LIMIT  PIC S9(10)V99
        String acctOpenDate,             // ACCT-OPEN-DATE          PIC X(10)
        String acctExpiraionDate,        // ACCT-EXPIRAION-DATE     PIC X(10)
        String acctReissueDate,          // ACCT-REISSUE-DATE       PIC X(10)
        BigDecimal acctCurrCycCredit,    // ACCT-CURR-CYC-CREDIT    PIC S9(10)V99
        BigDecimal acctCurrCycDebit,     // ACCT-CURR-CYC-DEBIT     PIC S9(10)V99
        String acctAddrZip,              // ACCT-ADDR-ZIP           PIC X(10)
        String acctGroupId,              // ACCT-GROUP-ID           PIC X(10)
        String filler) {                 // FILLER                  PIC X(178)

    public static final int LENGTH = 300;
    public static final int KEY_LENGTH = 11;

    public static AccountRecord parse(String raw) {
        String r = Pic.x(raw, LENGTH);
        return new AccountRecord(
                Pic.unsigned(r.substring(0, 11)),
                r.substring(11, 12),
                Pic.zoned(r.substring(12, 24), 10, 2),
                Pic.zoned(r.substring(24, 36), 10, 2),
                Pic.zoned(r.substring(36, 48), 10, 2),
                r.substring(48, 58),
                r.substring(58, 68),
                r.substring(68, 78),
                Pic.zoned(r.substring(78, 90), 10, 2),
                Pic.zoned(r.substring(90, 102), 10, 2),
                r.substring(102, 112),
                r.substring(112, 122),
                r.substring(122, 300));
    }

    /** ACCT-ID as its 11 DISPLAY digits. */
    public String acctIdDisplay() {
        return Pic.unsigned(acctId, KEY_LENGTH);
    }

    public String toFixedWidth() {
        return acctIdDisplay()
                + Pic.x(acctActiveStatus, 1)
                + Pic.zoned(acctCurrBal, 10, 2)
                + Pic.zoned(acctCreditLimit, 10, 2)
                + Pic.zoned(acctCashCreditLimit, 10, 2)
                + Pic.x(acctOpenDate, 10)
                + Pic.x(acctExpiraionDate, 10)
                + Pic.x(acctReissueDate, 10)
                + Pic.zoned(acctCurrCycCredit, 10, 2)
                + Pic.zoned(acctCurrCycDebit, 10, 2)
                + Pic.x(acctAddrZip, 10)
                + Pic.x(acctGroupId, 10)
                + Pic.x(filler, 178);
    }
}
