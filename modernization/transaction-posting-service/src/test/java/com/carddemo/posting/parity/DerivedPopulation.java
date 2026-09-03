package com.carddemo.posting.parity;

import com.carddemo.posting.batch.PostingDatasets;

import java.nio.charset.Charset;

/**
 * A stress population derived from the shipped datasets.
 *
 * <p>The shipped daily transaction file only ever produces one kind of rejection and only ever
 * posts positive amounts, so on its own it leaves most of {@code CBTRN02C} unexercised. This class
 * patches the real datasets — never synthesising a record from nothing — so that every branch of
 * the program is reached: all four reject reasons, the credit and the debit bucket, creation of a
 * category balance, and accumulation past the capacity of the receiving COBOL fields.
 *
 * <p>Patching is done on raw bytes with plain code-page-037 text, so the harness input does not
 * depend on the production codecs either.
 */
final class DerivedPopulation {

    private static final Charset CP037 = Charset.forName("IBM037");

    private static final int DALYTRAN_LEN = 350;
    private static final int XREF_LEN = 50;
    private static final int ACCT_LEN = 300;

    /** A card number that appears in no cross-reference record: reject reason 100. */
    private static final String UNKNOWN_CARD = "9999999999999999";
    /** An account id that appears in no account record: reject reason 101. */
    private static final String DANGLING_ACCOUNT = "99999999999";

    private DerivedPopulation() {
    }

    static PostingDatasets from(PostingDatasets shipped) {
        byte[] xref = shipped.cardXrefImage().clone();
        byte[] accounts = shipped.accountImage().clone();
        byte[] dailyTransactions = shipped.dailyTransactionImage().clone();

        // The first cross-reference record now points at an account that does not exist, so every
        // transaction on that card is rejected with reason 101.
        putText(xref, 25, 11, DANGLING_ACCOUNT);
        String danglingCard = text(xref, 0, 16);

        for (int offset = 0; offset < accounts.length; offset += ACCT_LEN) {
            int index = offset / ACCT_LEN;
            // Lift the credit limit so that transactions get past reason 102 and actually post.
            putZoned(accounts, offset + 24, 12, 9_999_999_999_99L);
            // Every seventh account has expired: reject reason 103, which also overrides 102.
            putText(accounts, offset + 58, 10, index % 7 == 0 ? "2000-01-01" : "2099-12-31");
        }

        for (int offset = 0; offset < dailyTransactions.length; offset += DALYTRAN_LEN) {
            int index = offset / DALYTRAN_LEN;
            if (index % 25 == 7) {
                putText(dailyTransactions, offset + 262, 16, UNKNOWN_CARD);
            }
            if (index % 25 == 11) {
                putText(dailyTransactions, offset + 262, 16, danglingCard);
            }
            if (index % 10 == 0) {
                // A refund: drives the debit bucket and the negative sign overpunch.
                putZoned(dailyTransactions, offset + 132, 11,
                        -Math.abs(zoned(dailyTransactions, offset + 132, 11)));
            } else if (index % 3 == 0) {
                // The largest amount the field can hold; accumulated often enough to overflow the
                // account and category balances it feeds.
                putZoned(dailyTransactions, offset + 132, 11, 999_999_999_99L);
            }
            if (index % 17 == 0) {
                // A type/category the account has never used: forces a category balance to be
                // created rather than rewritten.
                putText(dailyTransactions, offset + 16, 2, "05");
                putText(dailyTransactions, offset + 18, 4, "0009");
            }
        }

        return new PostingDatasets(dailyTransactions, xref, accounts,
                shipped.categoryBalanceImage().clone());
    }

    /**
     * Every transaction in the file, at the largest amount its field can hold, on one card.
     *
     * <p>One account then receives three hundred amounts of 999,999,999.99, which overflows both
     * the {@code PIC S9(09)V99} category balance and the {@code PIC S9(10)V99} account balances.
     * COBOL drops the high-order digits silently; this scenario is what proves the Java module
     * drops exactly the same ones.
     */
    static PostingDatasets overflowFrom(PostingDatasets shipped) {
        byte[] accounts = shipped.accountImage().clone();
        byte[] dailyTransactions = shipped.dailyTransactionImage().clone();
        byte[] xref = shipped.cardXrefImage();
        String card = text(xref, 0, 16);

        for (int offset = 0; offset < accounts.length; offset += ACCT_LEN) {
            putZoned(accounts, offset + 24, 12, 9_999_999_999_99L);
            putText(accounts, offset + 58, 10, "2099-12-31");
        }
        for (int offset = 0; offset < dailyTransactions.length; offset += DALYTRAN_LEN) {
            putText(dailyTransactions, offset + 262, 16, card);
            putZoned(dailyTransactions, offset + 132, 11, 999_999_999_99L);
        }
        return new PostingDatasets(dailyTransactions, xref.clone(), accounts,
                shipped.categoryBalanceImage().clone());
    }

    private static String text(byte[] record, int offset, int length) {
        return new String(record, offset, length, CP037);
    }

    private static void putText(byte[] record, int offset, int length, String value) {
        System.arraycopy(value.getBytes(CP037), 0, record, offset, length);
    }

    private static long zoned(byte[] record, int offset, int length) {
        long value = 0;
        for (int index = 0; index < length; index++) {
            value = value * 10 + (record[offset + index] & 0x0F);
        }
        return (record[offset + length - 1] & 0xF0) == 0xD0 ? -value : value;
    }

    private static void putZoned(byte[] record, int offset, int length, long cents) {
        long magnitude = Math.abs(cents);
        for (int index = length - 1; index >= 0; index--) {
            int digit = (int) (magnitude % 10);
            magnitude /= 10;
            int zone = index == length - 1 ? (cents < 0 ? 0xD0 : 0xC0) : 0xF0;
            record[offset + index] = (byte) (zone | digit);
        }
    }
}
