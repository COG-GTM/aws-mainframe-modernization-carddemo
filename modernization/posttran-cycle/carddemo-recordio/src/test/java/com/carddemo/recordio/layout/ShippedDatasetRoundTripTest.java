package com.carddemo.recordio.layout;

import com.carddemo.recordio.codec.FixedWidthRecord;
import com.carddemo.recordio.codec.RecordEncoding;
import com.carddemo.recordio.store.FixedWidthFile;
import com.carddemo.recordio.store.KeyedRecordStore;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.io.IOException;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Decodes every shipped EBCDIC dataset consumed by the flow with the copybook layouts, re-encodes
 * it, and checks the bytes are identical. This proves the offsets and the codec against the real
 * data images; it does not prove program behaviour.
 */
class ShippedDatasetRoundTripTest {

    static final Path DATA = repoRoot().resolve("app/data/EBCDIC");

    static Path repoRoot() {
        Path p = Paths.get("").toAbsolutePath();
        while (p != null && !Files.exists(p.resolve("app/cbl/CBTRN02C.cbl"))) {
            p = p.getParent();
        }
        if (p == null) {
            throw new IllegalStateException("repository root with app/cbl not found");
        }
        return p;
    }

    @ParameterizedTest
    @CsvSource({
            "AWS.M2.CARDDEMO.DALYTRAN.PS, 350, 300",
            "AWS.M2.CARDDEMO.ACCTDATA.PS, 300, 50",
            "AWS.M2.CARDDEMO.CARDXREF.PS, 50, 50",
            "AWS.M2.CARDDEMO.TCATBALF.PS, 50, 50",
            "AWS.M2.CARDDEMO.DISCGRP.PS, 50, 51",
            "AWS.M2.CARDDEMO.TRANTYPE.PS, 60, 7",
            "AWS.M2.CARDDEMO.TRANCATG.PS, 60, 18",
    })
    void shippedDatasetsHaveTheCopybookRecordLength(String name, int recordLength, int records) throws IOException {
        byte[] bytes = Files.readAllBytes(DATA.resolve(name));
        assertThat(bytes.length).isEqualTo(recordLength * records);
    }

    @Test
    void dailyTransactionsRoundTrip() throws IOException {
        assertRoundTrip("AWS.M2.CARDDEMO.DALYTRAN.PS", TransactionLayout.INSTANCE);
        Transaction first = FixedWidthFile.readAll(DATA.resolve("AWS.M2.CARDDEMO.DALYTRAN.PS"),
                TransactionLayout.INSTANCE, RecordEncoding.EBCDIC).get(0);
        assertThat(first.id()).isEqualTo("0000000000683580");
        assertThat(first.typeCode()).isEqualTo("01");
        assertThat(first.categoryCode()).isEqualTo(1);
        assertThat(first.amount()).isEqualByComparingTo(new BigDecimal("504.77"));
        assertThat(first.cardNumber()).isEqualTo("4859452612877065");
        assertThat(first.originalDate()).isEqualTo("2022-06-10");
        assertThat(first.processingTimestamp()).isEqualTo(" ".repeat(26));
    }

    @Test
    void accountsRoundTrip() throws IOException {
        assertRoundTrip("AWS.M2.CARDDEMO.ACCTDATA.PS", AccountLayout.INSTANCE);
        KeyedRecordStore<Account> store = KeyedRecordStore.load("ACCTFILE", DATA.resolve("AWS.M2.CARDDEMO.ACCTDATA.PS"),
                AccountLayout.INSTANCE, RecordEncoding.EBCDIC, Account::accountId);
        Account a = store.read("00000000001").orElseThrow();
        assertThat(a.activeStatus()).isEqualTo("Y");
        assertThat(a.currentBalance()).isEqualByComparingTo("194.00");
        assertThat(a.creditLimit()).isEqualByComparingTo("2020.00");
        assertThat(a.expirationDate()).isEqualTo("2025-05-20");
        assertThat(a.groupId()).isEqualTo(" ".repeat(10));
    }

    @Test
    void xrefBalancesAndReferenceTablesRoundTrip() throws IOException {
        assertRoundTrip("AWS.M2.CARDDEMO.CARDXREF.PS", CardXrefLayout.INSTANCE);
        assertRoundTrip("AWS.M2.CARDDEMO.TCATBALF.PS", TransactionCategoryBalanceLayout.INSTANCE);
        assertRoundTrip("AWS.M2.CARDDEMO.DISCGRP.PS", DisclosureGroupLayout.INSTANCE);
        assertRoundTrip("AWS.M2.CARDDEMO.TRANTYPE.PS", TransactionTypeLayout.INSTANCE);
        assertRoundTrip("AWS.M2.CARDDEMO.TRANCATG.PS", TransactionCategoryLayout.INSTANCE);
    }

    @Test
    void asciiCopyDecodesToTheSameValuesAsEbcdic() throws IOException {
        List<Transaction> ebcdic = FixedWidthFile.readAll(DATA.resolve("AWS.M2.CARDDEMO.DALYTRAN.PS"),
                TransactionLayout.INSTANCE, RecordEncoding.EBCDIC);
        Path ascii = repoRoot().resolve("app/data/ASCII/dailytran.txt");
        byte[] asciiBytes = Files.readAllBytes(ascii);
        List<Transaction> decoded = FixedWidthFile.split(stripLineFeeds(asciiBytes, 350), 350, RecordEncoding.ASCII)
                .stream().map(TransactionLayout.INSTANCE::decode).toList();
        assertThat(decoded).hasSize(ebcdic.size());
        assertThat(decoded.get(0).amount()).isEqualByComparingTo(ebcdic.get(0).amount());
        assertThat(decoded.stream().map(Transaction::id).toList())
                .isEqualTo(ebcdic.stream().map(Transaction::id).toList());
    }

    private static <T> void assertRoundTrip(String name, RecordLayout<T> layout) throws IOException {
        byte[] bytes = Files.readAllBytes(DATA.resolve(name));
        List<FixedWidthRecord> images = FixedWidthFile.split(bytes, layout.length(), RecordEncoding.EBCDIC);
        for (FixedWidthRecord image : images) {
            T value = layout.decode(image);
            FixedWidthRecord copy = image.copy();
            layout.encodeInto(copy, value);
            assertThat(copy.bytes()).as(name).isEqualTo(image.bytes());
        }
    }

    /** The ASCII copies are the same fixed records joined with LF separators. */
    private static byte[] stripLineFeeds(byte[] bytes, int recordLength) {
        String text = new String(bytes, java.nio.charset.StandardCharsets.US_ASCII);
        StringBuilder out = new StringBuilder();
        for (String line : text.split("\n")) {
            if (line.isEmpty()) {
                continue;
            }
            String padded = line.length() >= recordLength ? line.substring(0, recordLength)
                    : line + " ".repeat(recordLength - line.length());
            out.append(padded);
        }
        return out.toString().getBytes(java.nio.charset.StandardCharsets.US_ASCII);
    }
}
