package com.carddemo.programs.coacct01.copybook;

import com.carddemo.programs.coacct01.support.CobolText;
import com.carddemo.programs.coacct01.support.ZonedDecimal;

import java.math.BigDecimal;

/**
 * CVACT01Y - ACCOUNT-RECORD, VSAM KSDS ACCTDAT (RECLN 300, key ACCT-ID).
 * Values are normalised to their PIC on construction (MOVE truncation rules).
 *
 * @param acctId             ACCT-ID                PIC 9(11)
 * @param activeStatus       ACCT-ACTIVE-STATUS     PIC X(01)
 * @param currBal            ACCT-CURR-BAL          PIC S9(10)V99
 * @param creditLimit        ACCT-CREDIT-LIMIT      PIC S9(10)V99
 * @param cashCreditLimit    ACCT-CASH-CREDIT-LIMIT PIC S9(10)V99
 * @param openDate           ACCT-OPEN-DATE         PIC X(10)
 * @param expirationDate     ACCT-EXPIRAION-DATE    PIC X(10) (COBOL spelling)
 * @param reissueDate        ACCT-REISSUE-DATE      PIC X(10)
 * @param currCycCredit      ACCT-CURR-CYC-CREDIT   PIC S9(10)V99
 * @param currCycDebit       ACCT-CURR-CYC-DEBIT    PIC S9(10)V99
 * @param addrZip            ACCT-ADDR-ZIP          PIC X(10)
 * @param groupId            ACCT-GROUP-ID          PIC X(10)
 * @param filler             FILLER                 PIC X(178)
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
    public static final int ACCT_ID_LENGTH = 11;
    /** FILLER PIC X(178) at the end of the record. */
    public static final int FILLER_LENGTH = 178;

    static final int AMOUNT_INT_DIGITS = 10;
    static final int AMOUNT_SCALE = 2;
    static final int AMOUNT_WIDTH = AMOUNT_INT_DIGITS + AMOUNT_SCALE;

    public AccountRecord {
        acctId = CobolText.unsignedDisplay(acctId, ACCT_ID_LENGTH);
        activeStatus = CobolText.pic(activeStatus, 1);
        currBal = amount(currBal);
        creditLimit = amount(creditLimit);
        cashCreditLimit = amount(cashCreditLimit);
        openDate = CobolText.pic(openDate, 10);
        expirationDate = CobolText.pic(expirationDate, 10);
        reissueDate = CobolText.pic(reissueDate, 10);
        currCycCredit = amount(currCycCredit);
        currCycDebit = amount(currCycDebit);
        addrZip = CobolText.pic(addrZip, 10);
        groupId = CobolText.pic(groupId, 10);
        filler = CobolText.pic(filler, FILLER_LENGTH);
    }

    public AccountRecord(long acctId, String activeStatus, BigDecimal currBal, BigDecimal creditLimit,
                         BigDecimal cashCreditLimit, String openDate, String expirationDate,
                         String reissueDate, BigDecimal currCycCredit, BigDecimal currCycDebit,
                         String addrZip, String groupId) {
        this(acctId, activeStatus, currBal, creditLimit, cashCreditLimit, openDate, expirationDate,
                reissueDate, currCycCredit, currCycDebit, addrZip, groupId, "");
    }

    private static BigDecimal amount(BigDecimal v) {
        return ZonedDecimal.truncate(v, AMOUNT_INT_DIGITS, AMOUNT_SCALE);
    }

    /** ACCT-ID as the 11-byte key (RIDFLD) of the ACCTDAT KSDS. */
    public String key() {
        return CobolText.digits(acctId, ACCT_ID_LENGTH);
    }

    /** Parses one fixed-width 300-byte record (shorter lines are space-padded). */
    public static AccountRecord parse(String line) {
        String r = CobolText.pic(line, RECORD_LENGTH);
        int[] p = {0};
        return new AccountRecord(
                Long.parseLong(take(r, p, ACCT_ID_LENGTH)),
                take(r, p, 1),
                ZonedDecimal.parse(take(r, p, AMOUNT_WIDTH), AMOUNT_SCALE),
                ZonedDecimal.parse(take(r, p, AMOUNT_WIDTH), AMOUNT_SCALE),
                ZonedDecimal.parse(take(r, p, AMOUNT_WIDTH), AMOUNT_SCALE),
                take(r, p, 10),
                take(r, p, 10),
                take(r, p, 10),
                ZonedDecimal.parse(take(r, p, AMOUNT_WIDTH), AMOUNT_SCALE),
                ZonedDecimal.parse(take(r, p, AMOUNT_WIDTH), AMOUNT_SCALE),
                take(r, p, 10),
                take(r, p, 10),
                take(r, p, FILLER_LENGTH));
    }

    private static String take(String r, int[] pos, int len) {
        String s = r.substring(pos[0], pos[0] + len);
        pos[0] += len;
        return s;
    }

    public String toFixedWidth() {
        return key()
                + activeStatus
                + ZonedDecimal.format(currBal, AMOUNT_INT_DIGITS, AMOUNT_SCALE)
                + ZonedDecimal.format(creditLimit, AMOUNT_INT_DIGITS, AMOUNT_SCALE)
                + ZonedDecimal.format(cashCreditLimit, AMOUNT_INT_DIGITS, AMOUNT_SCALE)
                + openDate
                + expirationDate
                + reissueDate
                + ZonedDecimal.format(currCycCredit, AMOUNT_INT_DIGITS, AMOUNT_SCALE)
                + ZonedDecimal.format(currCycDebit, AMOUNT_INT_DIGITS, AMOUNT_SCALE)
                + addrZip
                + groupId
                + filler;
    }
}
