package com.carddemo.zerotrust.posting;

import com.carddemo.posting.domain.DailyTransaction;
import com.carddemo.posting.domain.PostingLedger;
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

/** Minimal masters for the secured posting tests (mirrors posttran's test fixtures). */
final class PostingTestFixtures {

    static final RecordEncoding ENC = RecordEncoding.EBCDIC;
    static final String CARD_OK = "4000000000000001";
    static final String CARD_BAD = "4999999999999999";
    static final String ACCT_OK = "00000000001";

    private PostingTestFixtures() {
    }

    static PostingLedger ledger() {
        KeyedRecordStore<CardXref> xref = KeyedRecordStore.of("XREFFILE", List.of(
                new CardXref(CARD_OK, 1L, ACCT_OK)),
                CardXrefLayout.INSTANCE, ENC, CardXref::cardNumber);
        KeyedRecordStore<Account> accounts = KeyedRecordStore.of("ACCTFILE", List.of(
                new Account(ACCT_OK, "Y", new BigDecimal("250.00"), new BigDecimal("1000.00"),
                        new BigDecimal("500.00"), "2020-01-01", "2030-12-31", "2025-01-01",
                        new BigDecimal("600.00"), new BigDecimal("100.00"), "12345", "DEFAULT")),
                AccountLayout.INSTANCE, ENC, Account::accountId);
        KeyedRecordStore<TransactionCategoryBalance> balances = KeyedRecordStore.of("TCATBALF",
                List.of(new TransactionCategoryBalance(ACCT_OK, "01", 1, new BigDecimal("100.00"))),
                TransactionCategoryBalanceLayout.INSTANCE, ENC, TransactionCategoryBalance::key);
        return new PostingLedger(xref, accounts, balances);
    }

    static DailyTransaction item(String id, String card, String amount) {
        Transaction t = new Transaction(id, "01", 1, "POS TERM", "Test purchase",
                new BigDecimal(amount), 123456789L, "Merchant", "City", "12345", card,
                "2025-01-01-10.00.00.000000", " ".repeat(26));
        return DailyTransaction.of(t, ENC);
    }
}
