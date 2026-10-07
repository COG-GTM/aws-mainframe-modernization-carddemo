package com.carddemo.batch.record;

import com.carddemo.batch.codec.Field;
import com.carddemo.batch.codec.FixedWidth;
import com.carddemo.batch.codec.Layout;

import java.math.BigDecimal;

/**
 * {@code FD OUT-FILE / 01 OUT-ACCT-REC} of CBACT01C (107 bytes). Same shape as the account record up to
 * the group id, but {@code OUT-ACCT-CURR-CYC-DEBIT} is {@code COMP-3} and there is no zip / filler.
 */
public final class OutAcctRec extends FixedWidthRecord {

    private static final Layout.Builder B = Layout.builder("OUT-ACCT-REC");
    public static final Field OUT_ACCT_ID = B.unsigned("OUT-ACCT-ID", 11);
    public static final Field OUT_ACCT_ACTIVE_STATUS = B.text("OUT-ACCT-ACTIVE-STATUS", 1);
    public static final Field OUT_ACCT_CURR_BAL = B.zoned("OUT-ACCT-CURR-BAL", 10, 2);
    public static final Field OUT_ACCT_CREDIT_LIMIT = B.zoned("OUT-ACCT-CREDIT-LIMIT", 10, 2);
    public static final Field OUT_ACCT_CASH_CREDIT_LIMIT = B.zoned("OUT-ACCT-CASH-CREDIT-LIMIT", 10, 2);
    public static final Field OUT_ACCT_OPEN_DATE = B.text("OUT-ACCT-OPEN-DATE", 10);
    public static final Field OUT_ACCT_EXPIRAION_DATE = B.text("OUT-ACCT-EXPIRAION-DATE", 10);
    public static final Field OUT_ACCT_REISSUE_DATE = B.text("OUT-ACCT-REISSUE-DATE", 10);
    public static final Field OUT_ACCT_CURR_CYC_CREDIT = B.zoned("OUT-ACCT-CURR-CYC-CREDIT", 10, 2);
    public static final Field OUT_ACCT_CURR_CYC_DEBIT = B.packed("OUT-ACCT-CURR-CYC-DEBIT", 10, 2);
    public static final Field OUT_ACCT_GROUP_ID = B.text("OUT-ACCT-GROUP-ID", 10);
    public static final Layout LAYOUT = B.build();
    public static final int LENGTH = LAYOUT.length();

    public OutAcctRec() {
        super(LAYOUT);
    }

    private OutAcctRec(byte[] raw) {
        super(LAYOUT, raw);
    }

    public static OutAcctRec decode(byte[] raw) {
        return new OutAcctRec(raw);
    }

    @Override
    public Layout layout() {
        return LAYOUT;
    }

    public long outAcctId() {
        return FixedWidth.unsigned(data, OUT_ACCT_ID);
    }

    public void setOutAcctId(long v) {
        FixedWidth.setUnsigned(data, OUT_ACCT_ID, v);
    }

    public String outAcctActiveStatus() {
        return FixedWidth.text(data, OUT_ACCT_ACTIVE_STATUS);
    }

    public void setOutAcctActiveStatus(String v) {
        FixedWidth.setText(data, OUT_ACCT_ACTIVE_STATUS, v);
    }

    public BigDecimal outAcctCurrBal() {
        return FixedWidth.decimal(data, OUT_ACCT_CURR_BAL);
    }

    public void setOutAcctCurrBal(BigDecimal v) {
        FixedWidth.setDecimal(data, OUT_ACCT_CURR_BAL, v);
    }

    public BigDecimal outAcctCreditLimit() {
        return FixedWidth.decimal(data, OUT_ACCT_CREDIT_LIMIT);
    }

    public void setOutAcctCreditLimit(BigDecimal v) {
        FixedWidth.setDecimal(data, OUT_ACCT_CREDIT_LIMIT, v);
    }

    public BigDecimal outAcctCashCreditLimit() {
        return FixedWidth.decimal(data, OUT_ACCT_CASH_CREDIT_LIMIT);
    }

    public void setOutAcctCashCreditLimit(BigDecimal v) {
        FixedWidth.setDecimal(data, OUT_ACCT_CASH_CREDIT_LIMIT, v);
    }

    public String outAcctOpenDate() {
        return FixedWidth.text(data, OUT_ACCT_OPEN_DATE);
    }

    public void setOutAcctOpenDate(String v) {
        FixedWidth.setText(data, OUT_ACCT_OPEN_DATE, v);
    }

    public String outAcctExpiraionDate() {
        return FixedWidth.text(data, OUT_ACCT_EXPIRAION_DATE);
    }

    public void setOutAcctExpiraionDate(String v) {
        FixedWidth.setText(data, OUT_ACCT_EXPIRAION_DATE, v);
    }

    /** 10 characters: {@code YYYYMMDD} from COBDATFT followed by two spaces. */
    public String outAcctReissueDate() {
        return FixedWidth.text(data, OUT_ACCT_REISSUE_DATE);
    }

    public void setOutAcctReissueDate(String v) {
        FixedWidth.setText(data, OUT_ACCT_REISSUE_DATE, v);
    }

    public BigDecimal outAcctCurrCycCredit() {
        return FixedWidth.decimal(data, OUT_ACCT_CURR_CYC_CREDIT);
    }

    public void setOutAcctCurrCycCredit(BigDecimal v) {
        FixedWidth.setDecimal(data, OUT_ACCT_CURR_CYC_CREDIT, v);
    }

    /** COMP-3 field. */
    public BigDecimal outAcctCurrCycDebit() {
        return FixedWidth.decimal(data, OUT_ACCT_CURR_CYC_DEBIT);
    }

    public void setOutAcctCurrCycDebit(BigDecimal v) {
        FixedWidth.setDecimal(data, OUT_ACCT_CURR_CYC_DEBIT, v);
    }

    public String outAcctGroupId() {
        return FixedWidth.text(data, OUT_ACCT_GROUP_ID);
    }

    public void setOutAcctGroupId(String v) {
        FixedWidth.setText(data, OUT_ACCT_GROUP_ID, v);
    }
}
