package com.carddemo.programs.cobil00c.copybook;

import java.math.BigDecimal;

/** CVACT01Y {@code ACCOUNT-RECORD} (RECLN 300). */
public record AccountRecord(
        long acctId,                      // PIC 9(11)
        String acctActiveStatus,          // PIC X(01)
        BigDecimal acctCurrBal,           // PIC S9(10)V99
        BigDecimal acctCreditLimit,       // PIC S9(10)V99
        BigDecimal acctCashCreditLimit,   // PIC S9(10)V99
        String acctOpenDate,              // PIC X(10)
        String acctExpiraionDate,         // PIC X(10)
        String acctReissueDate,           // PIC X(10)
        BigDecimal acctCurrCycCredit,     // PIC S9(10)V99
        BigDecimal acctCurrCycDebit,      // PIC S9(10)V99
        String acctAddrZip,               // PIC X(10)
        String acctGroupId) {             // PIC X(10); FILLER X(178) dropped

    public static final int RECORD_LENGTH = 300;
    public static final int ACCT_ID_LENGTH = 11;
    public static final int AMOUNT_INTEGER_DIGITS = 10;
    public static final int AMOUNT_SCALE = 2;

    public AccountRecord {
        acctCurrBal = pic(acctCurrBal);
        acctCreditLimit = pic(acctCreditLimit);
        acctCashCreditLimit = pic(acctCashCreditLimit);
        acctCurrCycCredit = pic(acctCurrCycCredit);
        acctCurrCycDebit = pic(acctCurrCycDebit);
        acctActiveStatus = Cobol.alnum(acctActiveStatus, 1);
        acctOpenDate = Cobol.alnum(acctOpenDate, 10);
        acctExpiraionDate = Cobol.alnum(acctExpiraionDate, 10);
        acctReissueDate = Cobol.alnum(acctReissueDate, 10);
        acctAddrZip = Cobol.alnum(acctAddrZip, 10);
        acctGroupId = Cobol.alnum(acctGroupId, 10);
    }

    private static BigDecimal pic(BigDecimal v) {
        return Cobol.toPic(v == null ? BigDecimal.ZERO : v, AMOUNT_INTEGER_DIGITS, AMOUNT_SCALE);
    }

    /** The VSAM key bytes (ACCT-ID as PIC 9(11) display). */
    public String key() {
        return Cobol.unsignedDigits(acctId, ACCT_ID_LENGTH);
    }

    public AccountRecord withAcctCurrBal(BigDecimal newBal) {
        return new AccountRecord(acctId, acctActiveStatus, newBal, acctCreditLimit, acctCashCreditLimit,
                acctOpenDate, acctExpiraionDate, acctReissueDate, acctCurrCycCredit, acctCurrCycDebit,
                acctAddrZip, acctGroupId);
    }
}
