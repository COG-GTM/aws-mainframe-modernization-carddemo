package com.carddemo.programs.copaus0c.copybook;

import com.carddemo.programs.copaus0c.cobol.ZonedDecimal;

import java.math.BigDecimal;

/** CVACT01Y - ACCOUNT-RECORD (RECLN 300). FILLER X(178) dropped. */
public record AccountRecord(
        long acctId,                    // ACCT-ID                PIC 9(11)
        String acctActiveStatus,        // ACCT-ACTIVE-STATUS     PIC X(01)
        BigDecimal acctCurrBal,         // ACCT-CURR-BAL          PIC S9(10)V99
        BigDecimal acctCreditLimit,     // ACCT-CREDIT-LIMIT      PIC S9(10)V99
        BigDecimal acctCashCreditLimit, // ACCT-CASH-CREDIT-LIMIT PIC S9(10)V99
        String acctOpenDate,            // ACCT-OPEN-DATE         PIC X(10)
        String acctExpiraionDate,       // ACCT-EXPIRAION-DATE    PIC X(10)
        String acctReissueDate,         // ACCT-REISSUE-DATE      PIC X(10)
        BigDecimal acctCurrCycCredit,   // ACCT-CURR-CYC-CREDIT   PIC S9(10)V99
        BigDecimal acctCurrCycDebit,    // ACCT-CURR-CYC-DEBIT    PIC S9(10)V99
        String acctAddrZip,             // ACCT-ADDR-ZIP          PIC X(10)
        String acctGroupId) {           // ACCT-GROUP-ID          PIC X(10)

    public static final int RECORD_LENGTH = 300;

    /** Parses a fixed-width display record (signed fields use trailing overpunch). */
    public static AccountRecord parse(String line) {
        String r = String.format("%-" + RECORD_LENGTH + "s", line);
        return new AccountRecord(
                Long.parseLong(r.substring(0, 11)),
                r.substring(11, 12),
                ZonedDecimal.parse(r.substring(12, 24), 2),
                ZonedDecimal.parse(r.substring(24, 36), 2),
                ZonedDecimal.parse(r.substring(36, 48), 2),
                r.substring(48, 58),
                r.substring(58, 68),
                r.substring(68, 78),
                ZonedDecimal.parse(r.substring(78, 90), 2),
                ZonedDecimal.parse(r.substring(90, 102), 2),
                r.substring(102, 112),
                r.substring(112, 122));
    }
}
