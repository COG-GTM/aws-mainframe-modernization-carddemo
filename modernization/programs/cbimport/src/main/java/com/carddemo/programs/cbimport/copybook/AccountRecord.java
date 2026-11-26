package com.carddemo.programs.cbimport.copybook;

import java.math.BigDecimal;

/** CVACT01Y: ACCOUNT-RECORD, RECLN 300 (ACCTOUT, LRECL=300). FILLER X(178) written as spaces. */
public record AccountRecord(
        long acctId,                    // ACCT-ID 9(11)
        String activeStatus,            // ACCT-ACTIVE-STATUS X(01)
        BigDecimal currBal,             // ACCT-CURR-BAL S9(10)V99
        BigDecimal creditLimit,         // ACCT-CREDIT-LIMIT S9(10)V99
        BigDecimal cashCreditLimit,     // ACCT-CASH-CREDIT-LIMIT S9(10)V99
        String openDate,                // ACCT-OPEN-DATE X(10)
        String expirationDate,          // ACCT-EXPIRAION-DATE X(10)
        String reissueDate,             // ACCT-REISSUE-DATE X(10)
        BigDecimal currCycCredit,       // ACCT-CURR-CYC-CREDIT S9(10)V99
        BigDecimal currCycDebit,        // ACCT-CURR-CYC-DEBIT S9(10)V99
        String addrZip,                 // ACCT-ADDR-ZIP X(10)
        String groupId) {               // ACCT-GROUP-ID X(10)

    public static final int LENGTH = 300;
    public static final int FILLER_LENGTH = 178;

    public AccountRecord {
        acctId = Pic.unsigned(acctId, 11);
        activeStatus = Pic.alnum(activeStatus, 1);
        currBal = Pic.signed(currBal, 10, 2);
        creditLimit = Pic.signed(creditLimit, 10, 2);
        cashCreditLimit = Pic.signed(cashCreditLimit, 10, 2);
        openDate = Pic.alnum(openDate, 10);
        expirationDate = Pic.alnum(expirationDate, 10);
        reissueDate = Pic.alnum(reissueDate, 10);
        currCycCredit = Pic.signed(currCycCredit, 10, 2);
        currCycDebit = Pic.signed(currCycDebit, 10, 2);
        addrZip = Pic.alnum(addrZip, 10);
        groupId = Pic.alnum(groupId, 10);
    }

    /** INITIALIZE ACCOUNT-RECORD. */
    public static AccountRecord initialized() {
        return new AccountRecord(0, "", BigDecimal.ZERO, BigDecimal.ZERO, BigDecimal.ZERO, "", "", "",
                BigDecimal.ZERO, BigDecimal.ZERO, "", "");
    }

    public String toFixedWidth() {
        return Pic.displayUnsigned(acctId, 11) + activeStatus
                + Pic.displaySigned(currBal, 10, 2) + Pic.displaySigned(creditLimit, 10, 2)
                + Pic.displaySigned(cashCreditLimit, 10, 2) + openDate + expirationDate + reissueDate
                + Pic.displaySigned(currCycCredit, 10, 2) + Pic.displaySigned(currCycDebit, 10, 2)
                + addrZip + groupId + " ".repeat(FILLER_LENGTH);
    }

    public static AccountRecord parse(String line) {
        Slicer s = new Slicer(line, LENGTH);
        return new AccountRecord(s.unsigned(11), s.x(1), s.signed(10, 2), s.signed(10, 2), s.signed(10, 2),
                s.x(10), s.x(10), s.x(10), s.signed(10, 2), s.signed(10, 2), s.x(10), s.x(10));
    }
}
