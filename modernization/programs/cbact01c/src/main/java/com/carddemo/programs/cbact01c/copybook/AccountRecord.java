package com.carddemo.programs.cbact01c.copybook;

import com.carddemo.programs.cbact01c.support.FieldDef;
import com.carddemo.programs.cbact01c.support.RecordBuffer;
import com.carddemo.programs.cbact01c.support.RecordText;

import java.math.BigDecimal;
import java.util.List;

/**
 * Copybook CVACT01Y: {@code 01 ACCOUNT-RECORD} (RECLN 300), the account master KSDS record.
 * Holds the raw 300 bytes so MOVEs out of it are byte-exact; typed getters decode on demand.
 */
public final class AccountRecord extends RecordBuffer {
    public static final int LENGTH = 300;

    public static final FieldDef ACCT_ID = FieldDef.n("ACCT-ID", 0, 11);
    public static final FieldDef ACCT_ACTIVE_STATUS = FieldDef.x("ACCT-ACTIVE-STATUS", 11, 1);
    public static final FieldDef ACCT_CURR_BAL = FieldDef.sn("ACCT-CURR-BAL", 12, 10, 2);
    public static final FieldDef ACCT_CREDIT_LIMIT = FieldDef.sn("ACCT-CREDIT-LIMIT", 24, 10, 2);
    public static final FieldDef ACCT_CASH_CREDIT_LIMIT = FieldDef.sn("ACCT-CASH-CREDIT-LIMIT", 36, 10, 2);
    public static final FieldDef ACCT_OPEN_DATE = FieldDef.x("ACCT-OPEN-DATE", 48, 10);
    public static final FieldDef ACCT_EXPIRAION_DATE = FieldDef.x("ACCT-EXPIRAION-DATE", 58, 10);
    public static final FieldDef ACCT_REISSUE_DATE = FieldDef.x("ACCT-REISSUE-DATE", 68, 10);
    public static final FieldDef ACCT_CURR_CYC_CREDIT = FieldDef.sn("ACCT-CURR-CYC-CREDIT", 78, 10, 2);
    public static final FieldDef ACCT_CURR_CYC_DEBIT = FieldDef.sn("ACCT-CURR-CYC-DEBIT", 90, 10, 2);
    public static final FieldDef ACCT_ADDR_ZIP = FieldDef.x("ACCT-ADDR-ZIP", 102, 10);
    public static final FieldDef ACCT_GROUP_ID = FieldDef.x("ACCT-GROUP-ID", 112, 10);
    public static final FieldDef FILLER = FieldDef.x("FILLER", 122, 178);

    public static final List<FieldDef> FIELDS = List.of(ACCT_ID, ACCT_ACTIVE_STATUS, ACCT_CURR_BAL, ACCT_CREDIT_LIMIT,
            ACCT_CASH_CREDIT_LIMIT, ACCT_OPEN_DATE, ACCT_EXPIRAION_DATE, ACCT_REISSUE_DATE, ACCT_CURR_CYC_CREDIT,
            ACCT_CURR_CYC_DEBIT, ACCT_ADDR_ZIP, ACCT_GROUP_ID, FILLER);

    private AccountRecord(byte[] bytes) {
        super(bytes);
    }

    /** READ ... INTO: the record area is exactly 300 bytes; shorter input is space-padded, longer truncated. */
    public static AccountRecord fromBytes(byte[] bytes) {
        byte[] area = new byte[LENGTH];
        java.util.Arrays.fill(area, (byte) ' ');
        System.arraycopy(bytes, 0, area, 0, Math.min(bytes.length, LENGTH));
        return new AccountRecord(area);
    }

    public static AccountRecord fromText(String text) {
        return fromBytes(RecordText.toBytes(text));
    }

    /** Builds a record from typed values (for tests and fixtures). */
    public static AccountRecord of(long acctId, String activeStatus, BigDecimal currBal, BigDecimal creditLimit,
                                   BigDecimal cashCreditLimit, String openDate, String expirationDate,
                                   String reissueDate, BigDecimal currCycCredit, BigDecimal currCycDebit,
                                   String addrZip, String groupId) {
        AccountRecord r = new AccountRecord(new byte[LENGTH]);
        r.putNumber(ACCT_ID, BigDecimal.valueOf(acctId));
        r.putText(ACCT_ACTIVE_STATUS, activeStatus);
        r.putNumber(ACCT_CURR_BAL, currBal);
        r.putNumber(ACCT_CREDIT_LIMIT, creditLimit);
        r.putNumber(ACCT_CASH_CREDIT_LIMIT, cashCreditLimit);
        r.putText(ACCT_OPEN_DATE, openDate);
        r.putText(ACCT_EXPIRAION_DATE, expirationDate);
        r.putText(ACCT_REISSUE_DATE, reissueDate);
        r.putNumber(ACCT_CURR_CYC_CREDIT, currCycCredit);
        r.putNumber(ACCT_CURR_CYC_DEBIT, currCycDebit);
        r.putText(ACCT_ADDR_ZIP, addrZip);
        r.putText(ACCT_GROUP_ID, groupId);
        r.putText(FILLER, "");
        return r;
    }

    public long acctId() {
        return number(ACCT_ID).longValueExact();
    }

    public String acctActiveStatus() {
        return text(ACCT_ACTIVE_STATUS);
    }

    public BigDecimal acctCurrBal() {
        return number(ACCT_CURR_BAL);
    }

    public BigDecimal acctCreditLimit() {
        return number(ACCT_CREDIT_LIMIT);
    }

    public BigDecimal acctCashCreditLimit() {
        return number(ACCT_CASH_CREDIT_LIMIT);
    }

    public String acctOpenDate() {
        return text(ACCT_OPEN_DATE);
    }

    public String acctExpiraionDate() {
        return text(ACCT_EXPIRAION_DATE);
    }

    public String acctReissueDate() {
        return text(ACCT_REISSUE_DATE);
    }

    public BigDecimal acctCurrCycCredit() {
        return number(ACCT_CURR_CYC_CREDIT);
    }

    public BigDecimal acctCurrCycDebit() {
        return number(ACCT_CURR_CYC_DEBIT);
    }

    public String acctAddrZip() {
        return text(ACCT_ADDR_ZIP);
    }

    public String acctGroupId() {
        return text(ACCT_GROUP_ID);
    }
}
