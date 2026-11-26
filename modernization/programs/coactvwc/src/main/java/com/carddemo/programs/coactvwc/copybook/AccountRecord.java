package com.carddemo.programs.coactvwc.copybook;

import com.carddemo.programs.coactvwc.support.Cobol;
import java.math.BigDecimal;

/** CVACT01Y: {@code ACCOUNT-RECORD} (RECLN 300). Signed amounts are PIC S9(10)V99 zoned decimal. */
public record AccountRecord(
        long acctId,                  // 9(11)
        String acctActiveStatus,      // X(01)
        BigDecimal acctCurrBal,       // S9(10)V99
        BigDecimal acctCreditLimit,   // S9(10)V99
        BigDecimal acctCashCreditLimit, // S9(10)V99
        String acctOpenDate,          // X(10)
        String acctExpirationDate,    // X(10)  (COBOL name ACCT-EXPIRAION-DATE)
        String acctReissueDate,       // X(10)
        BigDecimal acctCurrCycCredit, // S9(10)V99
        BigDecimal acctCurrCycDebit,  // S9(10)V99
        String acctAddrZip,           // X(10)
        String acctGroupId) {         // X(10); FILLER X(178) dropped
    public static final int LENGTH = 300;

    public static AccountRecord parse(String line) {
        String s = Cobol.alnum(line, LENGTH);
        return new AccountRecord(
                Cobol.toLong(s.substring(0, 11)),
                s.substring(11, 12),
                Cobol.zonedDecimal(s.substring(12, 24), 2),
                Cobol.zonedDecimal(s.substring(24, 36), 2),
                Cobol.zonedDecimal(s.substring(36, 48), 2),
                s.substring(48, 58),
                s.substring(58, 68),
                s.substring(68, 78),
                Cobol.zonedDecimal(s.substring(78, 90), 2),
                Cobol.zonedDecimal(s.substring(90, 102), 2),
                s.substring(102, 112),
                s.substring(112, 122));
    }

    public String toFixedWidth() {
        return Cobol.digits(acctId, 11) + Cobol.alnum(acctActiveStatus, 1)
                + Cobol.toZonedDecimal(acctCurrBal, 10, 2) + Cobol.toZonedDecimal(acctCreditLimit, 10, 2)
                + Cobol.toZonedDecimal(acctCashCreditLimit, 10, 2)
                + Cobol.alnum(acctOpenDate, 10) + Cobol.alnum(acctExpirationDate, 10) + Cobol.alnum(acctReissueDate, 10)
                + Cobol.toZonedDecimal(acctCurrCycCredit, 10, 2) + Cobol.toZonedDecimal(acctCurrCycDebit, 10, 2)
                + Cobol.alnum(acctAddrZip, 10) + Cobol.alnum(acctGroupId, 10) + Cobol.spaces(178);
    }
}
