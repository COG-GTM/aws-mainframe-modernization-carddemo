package com.carddemo.batch.record;

import com.carddemo.batch.codec.Field;
import com.carddemo.batch.codec.FixedWidth;
import com.carddemo.batch.codec.Layout;

import java.math.BigDecimal;

/**
 * Copybook {@code CVACT01Y} - {@code ACCOUNT-RECORD} (RECLN 300), the ACCTFILE KSDS record.
 * <pre>
 * 05  ACCT-ID                  PIC 9(11).
 * 05  ACCT-ACTIVE-STATUS       PIC X(01).
 * 05  ACCT-CURR-BAL            PIC S9(10)V99.
 * 05  ACCT-CREDIT-LIMIT        PIC S9(10)V99.
 * 05  ACCT-CASH-CREDIT-LIMIT   PIC S9(10)V99.
 * 05  ACCT-OPEN-DATE           PIC X(10).
 * 05  ACCT-EXPIRAION-DATE      PIC X(10).
 * 05  ACCT-REISSUE-DATE        PIC X(10).
 * 05  ACCT-CURR-CYC-CREDIT     PIC S9(10)V99.
 * 05  ACCT-CURR-CYC-DEBIT      PIC S9(10)V99.
 * 05  ACCT-ADDR-ZIP            PIC X(10).
 * 05  ACCT-GROUP-ID            PIC X(10).
 * 05  FILLER                   PIC X(178).
 * </pre>
 */
public final class AccountRecord extends FixedWidthRecord {

    private static final Layout.Builder B = Layout.builder("ACCOUNT-RECORD");
    public static final Field ACCT_ID = B.unsigned("ACCT-ID", 11);
    public static final Field ACCT_ACTIVE_STATUS = B.text("ACCT-ACTIVE-STATUS", 1);
    public static final Field ACCT_CURR_BAL = B.zoned("ACCT-CURR-BAL", 10, 2);
    public static final Field ACCT_CREDIT_LIMIT = B.zoned("ACCT-CREDIT-LIMIT", 10, 2);
    public static final Field ACCT_CASH_CREDIT_LIMIT = B.zoned("ACCT-CASH-CREDIT-LIMIT", 10, 2);
    public static final Field ACCT_OPEN_DATE = B.text("ACCT-OPEN-DATE", 10);
    public static final Field ACCT_EXPIRAION_DATE = B.text("ACCT-EXPIRAION-DATE", 10);
    public static final Field ACCT_REISSUE_DATE = B.text("ACCT-REISSUE-DATE", 10);
    public static final Field ACCT_CURR_CYC_CREDIT = B.zoned("ACCT-CURR-CYC-CREDIT", 10, 2);
    public static final Field ACCT_CURR_CYC_DEBIT = B.zoned("ACCT-CURR-CYC-DEBIT", 10, 2);
    public static final Field ACCT_ADDR_ZIP = B.text("ACCT-ADDR-ZIP", 10);
    public static final Field ACCT_GROUP_ID = B.text("ACCT-GROUP-ID", 10);
    public static final Field FILLER = B.filler(178);
    public static final Layout LAYOUT = B.build();
    public static final int LENGTH = LAYOUT.length();

    public AccountRecord() {
        super(LAYOUT);
    }

    private AccountRecord(byte[] raw) {
        super(LAYOUT, raw);
    }

    public static AccountRecord decode(byte[] raw) {
        return new AccountRecord(raw);
    }

    @Override
    public Layout layout() {
        return LAYOUT;
    }

    /** COBOL {@code READ ... INTO ACCOUNT-RECORD}: replace the whole buffer. */
    public void moveFrom(byte[] raw) {
        System.arraycopy(FixedWidth.decode(LAYOUT, raw), 0, data, 0, data.length);
    }

    public long acctId() {
        return FixedWidth.unsigned(data, ACCT_ID);
    }

    public void setAcctId(long v) {
        FixedWidth.setUnsigned(data, ACCT_ID, v);
    }

    public String acctActiveStatus() {
        return FixedWidth.text(data, ACCT_ACTIVE_STATUS);
    }

    public void setAcctActiveStatus(String v) {
        FixedWidth.setText(data, ACCT_ACTIVE_STATUS, v);
    }

    public BigDecimal acctCurrBal() {
        return FixedWidth.decimal(data, ACCT_CURR_BAL);
    }

    public void setAcctCurrBal(BigDecimal v) {
        FixedWidth.setDecimal(data, ACCT_CURR_BAL, v);
    }

    public BigDecimal acctCreditLimit() {
        return FixedWidth.decimal(data, ACCT_CREDIT_LIMIT);
    }

    public void setAcctCreditLimit(BigDecimal v) {
        FixedWidth.setDecimal(data, ACCT_CREDIT_LIMIT, v);
    }

    public BigDecimal acctCashCreditLimit() {
        return FixedWidth.decimal(data, ACCT_CASH_CREDIT_LIMIT);
    }

    public void setAcctCashCreditLimit(BigDecimal v) {
        FixedWidth.setDecimal(data, ACCT_CASH_CREDIT_LIMIT, v);
    }

    public String acctOpenDate() {
        return FixedWidth.text(data, ACCT_OPEN_DATE);
    }

    public void setAcctOpenDate(String v) {
        FixedWidth.setText(data, ACCT_OPEN_DATE, v);
    }

    public String acctExpiraionDate() {
        return FixedWidth.text(data, ACCT_EXPIRAION_DATE);
    }

    public void setAcctExpiraionDate(String v) {
        FixedWidth.setText(data, ACCT_EXPIRAION_DATE, v);
    }

    public String acctReissueDate() {
        return FixedWidth.text(data, ACCT_REISSUE_DATE);
    }

    public void setAcctReissueDate(String v) {
        FixedWidth.setText(data, ACCT_REISSUE_DATE, v);
    }

    public BigDecimal acctCurrCycCredit() {
        return FixedWidth.decimal(data, ACCT_CURR_CYC_CREDIT);
    }

    public void setAcctCurrCycCredit(BigDecimal v) {
        FixedWidth.setDecimal(data, ACCT_CURR_CYC_CREDIT, v);
    }

    public BigDecimal acctCurrCycDebit() {
        return FixedWidth.decimal(data, ACCT_CURR_CYC_DEBIT);
    }

    public void setAcctCurrCycDebit(BigDecimal v) {
        FixedWidth.setDecimal(data, ACCT_CURR_CYC_DEBIT, v);
    }

    public String acctAddrZip() {
        return FixedWidth.text(data, ACCT_ADDR_ZIP);
    }

    public void setAcctAddrZip(String v) {
        FixedWidth.setText(data, ACCT_ADDR_ZIP, v);
    }

    public String acctGroupId() {
        return FixedWidth.text(data, ACCT_GROUP_ID);
    }

    public void setAcctGroupId(String v) {
        FixedWidth.setText(data, ACCT_GROUP_ID, v);
    }

    /** What {@code DISPLAY field} prints for one field (signed numerics get GnuCOBOL's trailing sign). */
    public String display(Field f) {
        return FixedWidth.display(data, f);
    }
}
