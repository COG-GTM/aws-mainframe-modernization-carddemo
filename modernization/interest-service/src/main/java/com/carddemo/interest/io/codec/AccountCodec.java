package com.carddemo.interest.io.codec;

import com.carddemo.interest.domain.Account;
import com.carddemo.interest.domain.AccountId;
import com.carddemo.mainframe.io.RecordLayout;
import com.carddemo.mainframe.io.layout.CardDemoLayouts;

/**
 * Maps {@code CVACT01Y} account-master records to {@link Account} instances and back.
 *
 * <p>Encoding is the Java equivalent of {@code REWRITE FD-ACCTFILE-REC FROM ACCOUNT-RECORD}
 * ({@code app/cbl/CBACT04C.cbl:356}): the whole 300-byte record area is rewritten from the
 * in-memory structure, so every field is re-encoded rather than patched in place.
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

    public static byte[] encode(Account account) {
        byte[] record = LAYOUT.blankRecord();
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
