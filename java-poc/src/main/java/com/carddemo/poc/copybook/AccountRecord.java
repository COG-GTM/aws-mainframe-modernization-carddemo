package com.carddemo.poc.copybook;

import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;

/**
 * Java model of copybook {@code CVACT01Y} (account entity, RECLN 300).
 *
 * <pre>
 *  01  ACCOUNT-RECORD.
 *      05  ACCT-ID                 PIC 9(11).        offset   0, len  11
 *      05  ACCT-ACTIVE-STATUS      PIC X(01).        offset  11, len   1
 *      05  ACCT-CURR-BAL           PIC S9(10)V99.    offset  12, len  12
 *      05  ACCT-CREDIT-LIMIT       PIC S9(10)V99.    offset  24, len  12
 *      05  ACCT-CASH-CREDIT-LIMIT  PIC S9(10)V99.    offset  36, len  12
 *      05  ACCT-OPEN-DATE          PIC X(10).        offset  48, len  10
 *      05  ACCT-EXPIRAION-DATE     PIC X(10).        offset  58, len  10
 *      05  ACCT-REISSUE-DATE       PIC X(10).        offset  68, len  10
 *      05  ACCT-CURR-CYC-CREDIT    PIC S9(10)V99.    offset  78, len  12
 *      05  ACCT-CURR-CYC-DEBIT     PIC S9(10)V99.    offset  90, len  12
 *      05  ACCT-ADDR-ZIP           PIC X(10).        offset 102, len  10
 *      05  ACCT-GROUP-ID           PIC X(10).        offset 112, len  10
 *      05  FILLER                  PIC X(178).       offset 122, len 178
 * </pre>
 *
 * Money fields are zoned decimal with an overpunched sign; they are exposed both as the
 * raw picture text (for faithful {@code DISPLAY} output) and as {@link BigDecimal}.
 */
public final class AccountRecord {

    public static final int RECORD_LENGTH = 300;

    private static final int MONEY_DIGITS = 12;
    private static final int MONEY_SCALE = 2;

    private final String image;

    private AccountRecord(String image) {
        this.image = image;
    }

    public static AccountRecord fromImage(String image) {
        if (image.length() != RECORD_LENGTH) {
            throw new IllegalArgumentException(
                    "ACCOUNT-RECORD must be " + RECORD_LENGTH + " chars, got " + image.length());
        }
        return new AccountRecord(image);
    }

    public static AccountRecord fromBytes(byte[] record) {
        return fromImage(new String(record, StandardCharsets.ISO_8859_1));
    }

    private String field(int offset, int length) {
        return image.substring(offset, offset + length);
    }

    private BigDecimal money(int offset) {
        return ZonedDecimal.parse(field(offset, MONEY_DIGITS), MONEY_SCALE, true);
    }

    // --- raw picture text, exactly as DISPLAY would print it -------------------------------

    public String getAcctIdText() { return field(0, 11); }
    public String getActiveStatus() { return field(11, 1); }
    public String getCurrBalText() { return field(12, 12); }
    public String getCreditLimitText() { return field(24, 12); }
    public String getCashCreditLimitText() { return field(36, 12); }
    public String getOpenDate() { return field(48, 10); }
    public String getExpirationDate() { return field(58, 10); }
    public String getReissueDate() { return field(68, 10); }
    public String getCurrCycCreditText() { return field(78, 12); }
    public String getCurrCycDebitText() { return field(90, 12); }
    public String getAddrZip() { return field(102, 10); }
    public String getGroupId() { return field(112, 10); }

    // --- typed accessors --------------------------------------------------------------------

    public long getAcctId() { return ZonedDecimal.parseUnsignedLong(getAcctIdText()); }
    public BigDecimal getCurrBal() { return money(12); }
    public BigDecimal getCreditLimit() { return money(24); }
    public BigDecimal getCashCreditLimit() { return money(36); }
    public BigDecimal getCurrCycCredit() { return money(78); }
    public BigDecimal getCurrCycDebit() { return money(90); }

    /** The full 300-character group item, i.e. what {@code DISPLAY ACCOUNT-RECORD} prints. */
    public String toDisplayString() {
        return image;
    }

    @Override
    public String toString() {
        return "AccountRecord{acctId=" + getAcctId() + ", status=" + getActiveStatus()
                + ", currBal=" + getCurrBal() + ", creditLimit=" + getCreditLimit() + '}';
    }
}
