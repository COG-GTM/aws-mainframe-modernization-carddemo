package com.carddemo.account.domain;

import com.carddemo.common.record.FixedRecord;

import static com.carddemo.common.record.Copybooks.CVACT01Y;

/** {@link Account} ↔ CVACT01Y record. */
public final class AccountRecords {

    private AccountRecords() {
    }

    public static Account fromRecord(String text) {
        FixedRecord r = CVACT01Y.parse(text);
        Account a = new Account();
        a.setAcctId(r.number("ACCT-ID"));
        a.setActiveStatus(r.text("ACCT-ACTIVE-STATUS"));
        a.setCurrBal(r.decimal("ACCT-CURR-BAL"));
        a.setCreditLimit(r.decimal("ACCT-CREDIT-LIMIT"));
        a.setCashCreditLimit(r.decimal("ACCT-CASH-CREDIT-LIMIT"));
        a.setOpenDate(r.text("ACCT-OPEN-DATE"));
        a.setExpirationDate(r.text("ACCT-EXPIRAION-DATE"));
        a.setReissueDate(r.text("ACCT-REISSUE-DATE"));
        a.setCurrCycCredit(r.decimal("ACCT-CURR-CYC-CREDIT"));
        a.setCurrCycDebit(r.decimal("ACCT-CURR-CYC-DEBIT"));
        a.setAddrZip(r.text("ACCT-ADDR-ZIP"));
        a.setGroupId(r.text("ACCT-GROUP-ID"));
        return a;
    }

    public static String toRecord(Account a) {
        return CVACT01Y.blank()
                .set("ACCT-ID", a.getAcctId())
                .set("ACCT-ACTIVE-STATUS", a.getActiveStatus())
                .set("ACCT-CURR-BAL", a.getCurrBal())
                .set("ACCT-CREDIT-LIMIT", a.getCreditLimit())
                .set("ACCT-CASH-CREDIT-LIMIT", a.getCashCreditLimit())
                .set("ACCT-OPEN-DATE", a.getOpenDate())
                .set("ACCT-EXPIRAION-DATE", a.getExpirationDate())
                .set("ACCT-REISSUE-DATE", a.getReissueDate())
                .set("ACCT-CURR-CYC-CREDIT", a.getCurrCycCredit())
                .set("ACCT-CURR-CYC-DEBIT", a.getCurrCycDebit())
                .set("ACCT-ADDR-ZIP", a.getAddrZip())
                .set("ACCT-GROUP-ID", a.getGroupId())
                .toString();
    }
}
