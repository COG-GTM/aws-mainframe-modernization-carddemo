package com.carddemo.programs.cbtrn02c.copybook;

import java.math.BigDecimal;

/** CVACT01Y: ACCOUNT-RECORD, account entity (RECLN 300). Key ACCT-ID. */
public record AccountRecord(
        long acctId,                     // ACCT-ID                PIC 9(11)
        String acctActiveStatus,         // ACCT-ACTIVE-STATUS     PIC X(01)
        BigDecimal acctCurrBal,          // ACCT-CURR-BAL          PIC S9(10)V99
        BigDecimal acctCreditLimit,      // ACCT-CREDIT-LIMIT      PIC S9(10)V99
        BigDecimal acctCashCreditLimit,  // ACCT-CASH-CREDIT-LIMIT PIC S9(10)V99
        String acctOpenDate,             // ACCT-OPEN-DATE         PIC X(10)
        String acctExpiraionDate,        // ACCT-EXPIRAION-DATE    PIC X(10) (sic)
        String acctReissueDate,          // ACCT-REISSUE-DATE      PIC X(10)
        BigDecimal acctCurrCycCredit,    // ACCT-CURR-CYC-CREDIT   PIC S9(10)V99
        BigDecimal acctCurrCycDebit,     // ACCT-CURR-CYC-DEBIT    PIC S9(10)V99
        String acctAddrZip,              // ACCT-ADDR-ZIP          PIC X(10)
        String acctGroupId,              // ACCT-GROUP-ID          PIC X(10)
        String filler) {                 // FILLER                 PIC X(178)

    public static final int RECORD_LENGTH = 300;
    public static final int KEY_OFFSET = 0;
    public static final int KEY_LENGTH = 11;
    public static final int AMT_INT_DIGITS = 10;
    public static final int AMT_SCALE = 2;

    public static AccountRecord parse(String raw) {
        Pic.Reader r = new Pic.Reader(raw, RECORD_LENGTH);
        return new AccountRecord(r.n(11), r.x(1), r.s(10, 2), r.s(10, 2), r.s(10, 2), r.x(10), r.x(10),
                r.x(10), r.s(10, 2), r.s(10, 2), r.x(10), r.x(10), r.x(178));
    }

    public String format() {
        return new Pic.Writer().n(acctId, 11).x(acctActiveStatus, 1).s(acctCurrBal, 10, 2)
                .s(acctCreditLimit, 10, 2).s(acctCashCreditLimit, 10, 2).x(acctOpenDate, 10)
                .x(acctExpiraionDate, 10).x(acctReissueDate, 10).s(acctCurrCycCredit, 10, 2)
                .s(acctCurrCycDebit, 10, 2).x(acctAddrZip, 10).x(acctGroupId, 10).x(filler, 178)
                .build(RECORD_LENGTH);
    }

    public AccountRecord withBalances(BigDecimal currBal, BigDecimal currCycCredit, BigDecimal currCycDebit) {
        return new AccountRecord(acctId, acctActiveStatus, currBal, acctCreditLimit, acctCashCreditLimit,
                acctOpenDate, acctExpiraionDate, acctReissueDate, currCycCredit, currCycDebit, acctAddrZip,
                acctGroupId, filler);
    }
}
