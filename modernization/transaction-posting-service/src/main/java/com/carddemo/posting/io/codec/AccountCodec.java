package com.carddemo.posting.io.codec;

import com.carddemo.mainframe.io.RecordLayout;
import com.carddemo.mainframe.io.layout.CardDemoLayouts;
import com.carddemo.posting.domain.Account;
import com.carddemo.posting.domain.AccountId;

/**
 * Maps {@code CVACT01Y} account-master records to {@link Account} instances and back.
 */
public final class AccountCodec {

    private static final RecordLayout LAYOUT = CardDemoLayouts.ACCOUNT;

    private AccountCodec() {
    }

    public static int recordLength() {
        return LAYOUT.recordLength();
    }

    public static Account decode(byte[] record) {
        return new Account(
                AccountId.of(LAYOUT.rawText(record, "accountId")),
                LAYOUT.rawText(record, "activeStatus"),
                LAYOUT.decimal(record, "currentBalance"),
                LAYOUT.decimal(record, "creditLimit"),
                LAYOUT.decimal(record, "cashCreditLimit"),
                LAYOUT.rawText(record, "openDate"),
                LAYOUT.rawText(record, "expirationDate"),
                LAYOUT.rawText(record, "reissueDate"),
                LAYOUT.decimal(record, "currentCycleCredit"),
                LAYOUT.decimal(record, "currentCycleDebit"),
                LAYOUT.rawText(record, "addressZip"),
                LAYOUT.rawText(record, "groupId"));
    }

    /** Encodes an account into a fresh, blank record. */
    public static byte[] encode(Account account) {
        return encodeInto(LAYOUT.blankRecord(), account);
    }

    /**
     * Encodes an account over an existing record image, leaving bytes the copybook does not
     * describe untouched.
     *
     * <p>This is what {@code REWRITE FD-ACCTFILE-REC FROM ACCOUNT-RECORD}
     * ({@code app/cbl/CBTRN02C.cbl:554}) does: the record area holds the record that was read, so
     * the trailing {@code FILLER PIC X(178)} of {@code CVACT01Y} survives the rewrite unchanged.
     */
    public static byte[] encodeInto(byte[] recordImage, Account account) {
        byte[] record = recordImage.clone();
        LAYOUT.putText(record, "accountId", account.id().value());
        LAYOUT.putText(record, "activeStatus", account.activeStatus());
        LAYOUT.putDecimal(record, "currentBalance", account.currentBalance());
        LAYOUT.putDecimal(record, "creditLimit", account.creditLimit());
        LAYOUT.putDecimal(record, "cashCreditLimit", account.cashCreditLimit());
        LAYOUT.putText(record, "openDate", account.openDate());
        LAYOUT.putText(record, "expirationDate", account.expirationDate());
        LAYOUT.putText(record, "reissueDate", account.reissueDate());
        LAYOUT.putDecimal(record, "currentCycleCredit", account.currentCycleCredit());
        LAYOUT.putDecimal(record, "currentCycleDebit", account.currentCycleDebit());
        LAYOUT.putText(record, "addressZip", account.addressZip());
        LAYOUT.putText(record, "groupId", account.groupId());
        return record;
    }
}
