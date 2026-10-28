package com.carddemo.recordio.layout;

import com.carddemo.recordio.codec.FixedWidthRecord;

/** {@code app/cpy/CVACT01Y.cpy ACCOUNT-RECORD}, RECLN 300. */
public final class AccountLayout implements RecordLayout<Account> {

    public static final AccountLayout INSTANCE = new AccountLayout();

    static final int ID = 0;              // 9(11)
    static final int ACTIVE_STATUS = 11;  // X(01)
    static final int CURR_BAL = 12;       // S9(10)V99
    static final int CREDIT_LIMIT = 24;   // S9(10)V99
    static final int CASH_CREDIT_LIMIT = 36; // S9(10)V99
    static final int OPEN_DATE = 48;      // X(10)
    static final int EXPIRATION_DATE = 58; // X(10)  (ACCT-EXPIRAION-DATE in the copybook)
    static final int REISSUE_DATE = 68;   // X(10)
    static final int CURR_CYC_CREDIT = 78; // S9(10)V99
    static final int CURR_CYC_DEBIT = 90;  // S9(10)V99
    static final int ADDR_ZIP = 102;      // X(10)
    static final int GROUP_ID = 112;      // X(10)
    static final int FILLER = 122;        // X(178)

    private AccountLayout() {
    }

    @Override
    public int length() {
        return Account.LENGTH;
    }

    @Override
    public Account decode(FixedWidthRecord r) {
        return new Account(
                r.text(ID, 11),
                r.text(ACTIVE_STATUS, 1),
                r.zoned(CURR_BAL, 12, 2, true),
                r.zoned(CREDIT_LIMIT, 12, 2, true),
                r.zoned(CASH_CREDIT_LIMIT, 12, 2, true),
                r.text(OPEN_DATE, 10),
                r.text(EXPIRATION_DATE, 10),
                r.text(REISSUE_DATE, 10),
                r.zoned(CURR_CYC_CREDIT, 12, 2, true),
                r.zoned(CURR_CYC_DEBIT, 12, 2, true),
                r.text(ADDR_ZIP, 10),
                r.text(GROUP_ID, 10));
    }

    @Override
    public void encodeInto(FixedWidthRecord r, Account a) {
        r.setText(ID, 11, a.accountId());
        r.setText(ACTIVE_STATUS, 1, a.activeStatus());
        r.setZoned(CURR_BAL, 12, 2, true, a.currentBalance());
        r.setZoned(CREDIT_LIMIT, 12, 2, true, a.creditLimit());
        r.setZoned(CASH_CREDIT_LIMIT, 12, 2, true, a.cashCreditLimit());
        r.setText(OPEN_DATE, 10, a.openDate());
        r.setText(EXPIRATION_DATE, 10, a.expirationDate());
        r.setText(REISSUE_DATE, 10, a.reissueDate());
        r.setZoned(CURR_CYC_CREDIT, 12, 2, true, a.currentCycleCredit());
        r.setZoned(CURR_CYC_DEBIT, 12, 2, true, a.currentCycleDebit());
        r.setText(ADDR_ZIP, 10, a.addressZip());
        r.setText(GROUP_ID, 10, a.groupId());
    }
}
