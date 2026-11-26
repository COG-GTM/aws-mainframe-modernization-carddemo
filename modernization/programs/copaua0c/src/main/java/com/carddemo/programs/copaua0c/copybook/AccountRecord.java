package com.carddemo.programs.copaua0c.copybook;

import com.carddemo.programs.copaua0c.support.Cobol;
import java.math.BigDecimal;

/** CVACT01Y - 01 ACCOUNT-RECORD (RECLN 300; FILLER X(178) dropped). */
public record AccountRecord(
        long acctId,                     // ACCT-ID                PIC 9(11)
        String activeStatus,             // ACCT-ACTIVE-STATUS     PIC X(01)
        BigDecimal currBal,              // ACCT-CURR-BAL          PIC S9(10)V99
        BigDecimal creditLimit,          // ACCT-CREDIT-LIMIT      PIC S9(10)V99
        BigDecimal cashCreditLimit,      // ACCT-CASH-CREDIT-LIMIT PIC S9(10)V99
        String openDate,                 // ACCT-OPEN-DATE         PIC X(10)
        String expiraionDate,            // ACCT-EXPIRAION-DATE    PIC X(10)
        String reissueDate,              // ACCT-REISSUE-DATE      PIC X(10)
        BigDecimal currCycCredit,        // ACCT-CURR-CYC-CREDIT   PIC S9(10)V99
        BigDecimal currCycDebit,         // ACCT-CURR-CYC-DEBIT    PIC S9(10)V99
        String addrZip,                  // ACCT-ADDR-ZIP          PIC X(10)
        String groupId) {                // ACCT-GROUP-ID          PIC X(10)

    public static final int RECORD_LENGTH = 300;

    /** Parse a fixed-width DISPLAY record (signed zoned decimals with trailing overpunch). */
    public static AccountRecord fromFixed(String line) {
        String r = Cobol.alpha(line, RECORD_LENGTH);
        return new AccountRecord(
                Cobol.unsigned(r.substring(0, 11)),
                r.substring(11, 12),
                Cobol.zoned(r.substring(12, 24), 2),
                Cobol.zoned(r.substring(24, 36), 2),
                Cobol.zoned(r.substring(36, 48), 2),
                r.substring(48, 58),
                r.substring(58, 68),
                r.substring(68, 78),
                Cobol.zoned(r.substring(78, 90), 2),
                Cobol.zoned(r.substring(90, 102), 2),
                r.substring(102, 112),
                r.substring(112, 122));
    }
}
