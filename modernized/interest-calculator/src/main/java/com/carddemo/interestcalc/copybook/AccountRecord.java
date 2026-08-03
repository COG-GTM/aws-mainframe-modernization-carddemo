package com.carddemo.interestcalc.copybook;

import java.math.BigDecimal;

/**
 * Copybook {@code CVACT01Y} - {@code ACCOUNT-RECORD}, RECLN 300.
 *
 * <pre>
 * 05 ACCT-ID                PIC 9(11).      bytes   1-11
 * 05 ACCT-ACTIVE-STATUS     PIC X(01).      byte     12
 * 05 ACCT-CURR-BAL          PIC S9(10)V99.  bytes  13-24
 * 05 ACCT-CREDIT-LIMIT      PIC S9(10)V99.  bytes  25-36
 * 05 ACCT-CASH-CREDIT-LIMIT PIC S9(10)V99.  bytes  37-48
 * 05 ACCT-OPEN-DATE         PIC X(10).      bytes  49-58
 * 05 ACCT-EXPIRAION-DATE    PIC X(10).      bytes  59-68
 * 05 ACCT-REISSUE-DATE      PIC X(10).      bytes  69-78
 * 05 ACCT-CURR-CYC-CREDIT   PIC S9(10)V99.  bytes  79-90
 * 05 ACCT-CURR-CYC-DEBIT    PIC S9(10)V99.  bytes  91-102
 * 05 ACCT-ADDR-ZIP          PIC X(10).      bytes 103-112
 * 05 ACCT-GROUP-ID          PIC X(10).      bytes 113-122
 * 05 FILLER                 PIC X(178).     bytes 123-300
 * </pre>
 *
 * <p>The date fields are {@code PIC X(10)} and are carried through verbatim rather than parsed
 * into {@code LocalDate}: CBACT04C never interprets them, and round-tripping them as text is
 * what makes a byte-for-byte rewrite of the record possible.
 *
 * @param filler bytes 123-300, preserved verbatim so that {@code REWRITE} is byte faithful
 */
public record AccountRecord(String accountId, String activeStatus, BigDecimal currentBalance,
                            BigDecimal creditLimit, BigDecimal cashCreditLimit, String openDate,
                            String expirationDate, String reissueDate, BigDecimal currentCycleCredit,
                            BigDecimal currentCycleDebit, String addressZip, String groupId, String filler) {

    public static final int LENGTH = 300;

    public static AccountRecord parse(String raw) {
        String rec = FixedWidth.record(raw, LENGTH);
        return new AccountRecord(
                FixedWidth.field(rec, 0, 11),
                FixedWidth.field(rec, 11, 1),
                ZonedDecimal.decode(FixedWidth.field(rec, 12, 12), 10, 2),
                ZonedDecimal.decode(FixedWidth.field(rec, 24, 12), 10, 2),
                ZonedDecimal.decode(FixedWidth.field(rec, 36, 12), 10, 2),
                FixedWidth.field(rec, 48, 10),
                FixedWidth.field(rec, 58, 10),
                FixedWidth.field(rec, 68, 10),
                ZonedDecimal.decode(FixedWidth.field(rec, 78, 12), 10, 2),
                ZonedDecimal.decode(FixedWidth.field(rec, 90, 12), 10, 2),
                FixedWidth.field(rec, 102, 10),
                FixedWidth.field(rec, 112, 10),
                FixedWidth.field(rec, 122, 178));
    }

    public String format() {
        return FixedWidth.unsigned(accountId, 11)
                + FixedWidth.alphanumeric(activeStatus, 1)
                + ZonedDecimal.encode(currentBalance, 10, 2)
                + ZonedDecimal.encode(creditLimit, 10, 2)
                + ZonedDecimal.encode(cashCreditLimit, 10, 2)
                + FixedWidth.alphanumeric(openDate, 10)
                + FixedWidth.alphanumeric(expirationDate, 10)
                + FixedWidth.alphanumeric(reissueDate, 10)
                + ZonedDecimal.encode(currentCycleCredit, 10, 2)
                + ZonedDecimal.encode(currentCycleDebit, 10, 2)
                + FixedWidth.alphanumeric(addressZip, 10)
                + FixedWidth.alphanumeric(groupId, 10)
                + FixedWidth.alphanumeric(filler, 178);
    }

    public AccountRecord withBalanceAndClearedCycleTotals(BigDecimal newBalance) {
        return new AccountRecord(accountId, activeStatus,
                CobolNumeric.store(newBalance, 10, 2), creditLimit, cashCreditLimit,
                openDate, expirationDate, reissueDate,
                BigDecimal.ZERO.setScale(2), BigDecimal.ZERO.setScale(2),
                addressZip, groupId, filler);
    }
}
