package com.carddemo.programs.cotrn02c.copybook;

import java.math.BigDecimal;

/**
 * CVACT01Y {@code ACCOUNT-RECORD}, RECLN 300. COTRN02C COPYs this layout (and names the ACCTDAT file)
 * but never reads it; it is carried here for completeness of the copybook translation.
 */
public record AccountRecord(
        long acctId,                  // 9(11)
        String activeStatus,          // X(01)
        BigDecimal currBal,           // S9(10)V99
        BigDecimal creditLimit,       // S9(10)V99
        BigDecimal cashCreditLimit,   // S9(10)V99
        String openDate,              // X(10)
        String expirationDate,        // X(10) (ACCT-EXPIRAION-DATE)
        String reissueDate,           // X(10)
        BigDecimal currCycCredit,     // S9(10)V99
        BigDecimal currCycDebit,      // S9(10)V99
        String addrZip,               // X(10)
        String groupId,               // X(10)
        String filler) {              // X(178)

    public static final int LENGTH = 300;

    public AccountRecord {
        acctId = Long.parseLong(Pic.nine(acctId, 11));
        activeStatus = Pic.x(activeStatus, 1);
        currBal = Pic.fit(currBal, 10, 2);
        creditLimit = Pic.fit(creditLimit, 10, 2);
        cashCreditLimit = Pic.fit(cashCreditLimit, 10, 2);
        openDate = Pic.x(openDate, 10);
        expirationDate = Pic.x(expirationDate, 10);
        reissueDate = Pic.x(reissueDate, 10);
        currCycCredit = Pic.fit(currCycCredit, 10, 2);
        currCycDebit = Pic.fit(currCycDebit, 10, 2);
        addrZip = Pic.x(addrZip, 10);
        groupId = Pic.x(groupId, 10);
        filler = Pic.x(filler, 178);
    }

    public static AccountRecord parse(String line) {
        String r = Pic.x(line, LENGTH);
        return new AccountRecord(
                Pic.alphaToUnsigned(r.substring(0, 11)).longValueExact(),
                r.substring(11, 12),
                Pic.parseZonedSigned(r.substring(12, 24), 2),
                Pic.parseZonedSigned(r.substring(24, 36), 2),
                Pic.parseZonedSigned(r.substring(36, 48), 2),
                r.substring(48, 58), r.substring(58, 68), r.substring(68, 78),
                Pic.parseZonedSigned(r.substring(78, 90), 2),
                Pic.parseZonedSigned(r.substring(90, 102), 2),
                r.substring(102, 112), r.substring(112, 122), r.substring(122, 300));
    }

    public String toRecord() {
        return Pic.nine(acctId, 11) + activeStatus
                + Pic.zonedSigned(currBal, 10, 2) + Pic.zonedSigned(creditLimit, 10, 2)
                + Pic.zonedSigned(cashCreditLimit, 10, 2)
                + openDate + expirationDate + reissueDate
                + Pic.zonedSigned(currCycCredit, 10, 2) + Pic.zonedSigned(currCycDebit, 10, 2)
                + addrZip + groupId + filler;
    }
}
