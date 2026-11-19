package com.carddemo.account.batch;

import com.carddemo.account.domain.Account;
import com.carddemo.common.cobol.CodePage;
import com.carddemo.common.record.CobolRecord;
import com.carddemo.common.record.RecordLayout;

import java.time.LocalDate;

/** CVACT01Y ACCOUNT-RECORD (ACCTDATA, 300 bytes) and its mapping to {@link Account}. */
public final class AccountRecordLayout {

    public static final RecordLayout LAYOUT = RecordLayout.builder("CVACT01Y", 300)
            .zoned("ACCT-ID", 11, 0, false)
            .alphanumeric("ACCT-ACTIVE-STATUS", 1)
            .zoned("ACCT-CURR-BAL", 10, 2, true)
            .zoned("ACCT-CREDIT-LIMIT", 10, 2, true)
            .zoned("ACCT-CASH-CREDIT-LIMIT", 10, 2, true)
            .alphanumeric("ACCT-OPEN-DATE", 10)
            .alphanumeric("ACCT-EXPIRAION-DATE", 10)
            .alphanumeric("ACCT-REISSUE-DATE", 10)
            .zoned("ACCT-CURR-CYC-CREDIT", 10, 2, true)
            .zoned("ACCT-CURR-CYC-DEBIT", 10, 2, true)
            .alphanumeric("ACCT-ADDR-ZIP", 10)
            .alphanumeric("ACCT-GROUP-ID", 10)
            .filler(178)
            .build();

    private AccountRecordLayout() {
    }

    public static Account toEntity(CobolRecord r) {
        Account a = new Account(r.getLong("ACCT-ID"));
        a.setActiveStatus(r.getString("ACCT-ACTIVE-STATUS"));
        a.setCurrBal(r.getDecimal("ACCT-CURR-BAL"));
        a.setCreditLimit(r.getDecimal("ACCT-CREDIT-LIMIT"));
        a.setCashCreditLimit(r.getDecimal("ACCT-CASH-CREDIT-LIMIT"));
        a.setOpenDate(date(r, "ACCT-OPEN-DATE"));
        a.setExpirationDate(date(r, "ACCT-EXPIRAION-DATE"));
        a.setReissueDate(date(r, "ACCT-REISSUE-DATE"));
        a.setCurrCycCredit(r.getDecimal("ACCT-CURR-CYC-CREDIT"));
        a.setCurrCycDebit(r.getDecimal("ACCT-CURR-CYC-DEBIT"));
        a.setAddrZip(text(r, "ACCT-ADDR-ZIP"));
        a.setGroupId(text(r, "ACCT-GROUP-ID"));
        return a;
    }

    public static CobolRecord toRecord(Account a, CodePage codePage) {
        return LAYOUT.blank(codePage)
                .setLong("ACCT-ID", a.getAcctId())
                .setString("ACCT-ACTIVE-STATUS", a.getActiveStatus())
                .setDecimal("ACCT-CURR-BAL", a.getCurrBal())
                .setDecimal("ACCT-CREDIT-LIMIT", a.getCreditLimit())
                .setDecimal("ACCT-CASH-CREDIT-LIMIT", a.getCashCreditLimit())
                .setString("ACCT-OPEN-DATE", text(a.getOpenDate()))
                .setString("ACCT-EXPIRAION-DATE", text(a.getExpirationDate()))
                .setString("ACCT-REISSUE-DATE", text(a.getReissueDate()))
                .setDecimal("ACCT-CURR-CYC-CREDIT", a.getCurrCycCredit())
                .setDecimal("ACCT-CURR-CYC-DEBIT", a.getCurrCycDebit())
                .setString("ACCT-ADDR-ZIP", a.getAddrZip() == null ? "" : a.getAddrZip())
                .setString("ACCT-GROUP-ID", a.getGroupId() == null ? "" : a.getGroupId());
    }

    /** X(10) {@code yyyy-MM-dd}; blank is null (D-013). Anything else is a data error and abends. */
    private static LocalDate date(CobolRecord r, String field) {
        String text = text(r, field);
        return text == null ? null : LocalDate.parse(text);
    }

    private static String text(LocalDate date) {
        return date == null ? "" : date.toString();
    }

    /** Trailing spaces dropped (leading ones kept, so the image round-trips); blank is null. */
    private static String text(CobolRecord r, String field) {
        String text = r.getString(field).stripTrailing();
        return text.isEmpty() ? null : text;
    }
}
