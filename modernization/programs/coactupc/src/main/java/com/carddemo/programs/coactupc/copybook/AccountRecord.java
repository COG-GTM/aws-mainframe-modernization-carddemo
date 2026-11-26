package com.carddemo.programs.coactupc.copybook;

import com.carddemo.programs.coactupc.support.Cobol;
import com.carddemo.programs.coactupc.support.FixedWidth;
import com.carddemo.programs.coactupc.support.ZonedDecimal;
import java.math.BigDecimal;

/**
 * CVACT01Y - ACCOUNT-RECORD (ACCTDAT VSAM KSDS, RECLN 300, key ACCT-ID).
 *
 * @param acctId               ACCT-ID PIC 9(11)
 * @param activeStatus         ACCT-ACTIVE-STATUS PIC X(01)
 * @param currBal              ACCT-CURR-BAL PIC S9(10)V99
 * @param creditLimit          ACCT-CREDIT-LIMIT PIC S9(10)V99
 * @param cashCreditLimit      ACCT-CASH-CREDIT-LIMIT PIC S9(10)V99
 * @param openDate             ACCT-OPEN-DATE PIC X(10)
 * @param expirationDate       ACCT-EXPIRAION-DATE PIC X(10)
 * @param reissueDate          ACCT-REISSUE-DATE PIC X(10)
 * @param currCycCredit        ACCT-CURR-CYC-CREDIT PIC S9(10)V99
 * @param currCycDebit         ACCT-CURR-CYC-DEBIT PIC S9(10)V99
 * @param addrZip              ACCT-ADDR-ZIP PIC X(10)
 * @param groupId              ACCT-GROUP-ID PIC X(10)
 * @param filler               FILLER PIC X(178)
 */
public record AccountRecord(
        long acctId,
        String activeStatus,
        BigDecimal currBal,
        BigDecimal creditLimit,
        BigDecimal cashCreditLimit,
        String openDate,
        String expirationDate,
        String reissueDate,
        BigDecimal currCycCredit,
        BigDecimal currCycDebit,
        String addrZip,
        String groupId,
        String filler) {

    public static final int RECORD_LENGTH = 300;
    public static final int AMOUNT_INT_DIGITS = 10;
    public static final int AMOUNT_SCALE = 2;

    public static AccountRecord parse(String image) {
        FixedWidth f = new FixedWidth(image, RECORD_LENGTH);
        return new AccountRecord(
                Cobol.unsignedValue(f.next(11)),
                f.next(1),
                amount(f.next(12)),
                amount(f.next(12)),
                amount(f.next(12)),
                f.next(10),
                f.next(10),
                f.next(10),
                amount(f.next(12)),
                amount(f.next(12)),
                f.next(10),
                f.next(10),
                f.next(178));
    }

    /** Record area that has never been read into (CICS working storage without VALUE clause). */
    public static AccountRecord lowValues() {
        return parse(Cobol.lowValues(RECORD_LENGTH));
    }

    public String toFixedWidth() {
        return Cobol.unsignedDisplay(acctId, 11)
                + Cobol.pad(activeStatus, 1)
                + zoned(currBal)
                + zoned(creditLimit)
                + zoned(cashCreditLimit)
                + Cobol.pad(openDate, 10)
                + Cobol.pad(expirationDate, 10)
                + Cobol.pad(reissueDate, 10)
                + zoned(currCycCredit)
                + zoned(currCycDebit)
                + Cobol.pad(addrZip, 10)
                + Cobol.pad(groupId, 10)
                + Cobol.pad(filler, 178);
    }

    /** Key image for RIDFLD (ACCT-ID as 11 display digits). */
    public String key() {
        return Cobol.unsignedDisplay(acctId, 11);
    }

    private static BigDecimal amount(String text) {
        return ZonedDecimal.decode(text, AMOUNT_SCALE);
    }

    private static String zoned(BigDecimal value) {
        return ZonedDecimal.encode(value, AMOUNT_INT_DIGITS, AMOUNT_SCALE);
    }
}
