package com.carddemo.posting;

import com.carddemo.posting.domain.DailyTransaction;
import com.carddemo.recordio.codec.RecordEncoding;
import com.carddemo.recordio.layout.Account;
import com.carddemo.recordio.layout.AccountLayout;
import com.carddemo.recordio.layout.CardXref;
import com.carddemo.recordio.layout.CardXrefLayout;
import com.carddemo.recordio.layout.Transaction;
import com.carddemo.recordio.layout.TransactionCategoryBalance;
import com.carddemo.recordio.layout.TransactionCategoryBalanceLayout;
import com.carddemo.recordio.store.KeyedRecordStore;

import java.math.BigDecimal;
import java.util.List;

/** Hand-built masters covering the CBTRN02C validation matrix. */
public final class Fixtures {

    public static final RecordEncoding ENC = RecordEncoding.EBCDIC;
    public static final String CARD_OK = "4000000000000001";
    public static final String CARD_NO_ACCOUNT = "4000000000000002";
    public static final String CARD_EXPIRED = "4000000000000003";
    public static final String CARD_UNKNOWN = "4999999999999999";
    public static final String ACCT_OK = "00000000001";
    public static final String ACCT_MISSING = "00000000002";
    public static final String ACCT_EXPIRED = "00000000003";

    private Fixtures() {
    }

    public static KeyedRecordStore<CardXref> xref() {
        return KeyedRecordStore.of("XREFFILE", List.of(
                new CardXref(CARD_OK, 1L, ACCT_OK),
                new CardXref(CARD_NO_ACCOUNT, 2L, ACCT_MISSING),
                new CardXref(CARD_EXPIRED, 3L, ACCT_EXPIRED)),
                CardXrefLayout.INSTANCE, ENC, CardXref::cardNumber);
    }

    /** ACCT_OK: limit 1000.00, cycle credit 600.00, cycle debit 100.00 -> headroom 500.00. */
    public static KeyedRecordStore<Account> accounts() {
        return KeyedRecordStore.of("ACCTFILE", List.of(
                account(ACCT_OK, "1000.00", "600.00", "100.00", "2030-12-31"),
                account(ACCT_EXPIRED, "1000.00", "0.00", "0.00", "2022-06-15")),
                AccountLayout.INSTANCE, ENC, Account::accountId);
    }

    public static Account account(String id, String limit, String cycCredit, String cycDebit, String expiry) {
        return new Account(id, "Y", new BigDecimal("250.00"), new BigDecimal(limit), new BigDecimal("500.00"),
                "2020-01-01", expiry, "2025-01-01", new BigDecimal(cycCredit), new BigDecimal(cycDebit),
                "12345", "DEFAULT");
    }

    public static KeyedRecordStore<TransactionCategoryBalance> balances() {
        return KeyedRecordStore.of("TCATBALF", List.of(
                new TransactionCategoryBalance(ACCT_OK, "01", 1, new BigDecimal("100.00"))),
                TransactionCategoryBalanceLayout.INSTANCE, ENC, TransactionCategoryBalance::key);
    }

    public static Transaction daily(String id, String card, String amount, String originalDate) {
        return daily(id, card, "01", 1, amount, originalDate);
    }

    public static Transaction daily(String id, String card, String type, int category, String amount, String originalDate) {
        return new Transaction(id, type, category, "POS TERM", "Test purchase", new BigDecimal(amount), 123456789L,
                "Merchant", "City", "12345", card, originalDate + "-10.00.00.000000", " ".repeat(26));
    }

    public static DailyTransaction item(Transaction t) {
        return DailyTransaction.of(t, ENC);
    }
}
