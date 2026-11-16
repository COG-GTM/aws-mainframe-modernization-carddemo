package com.carddemo.batch.model;

import com.carddemo.batch.io.CardDemoData;
import com.carddemo.batch.io.FixedWidthCodec;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Decodes every line of the ASCII sample files, re-encodes it and checks the file is reproduced byte for
 * byte, line terminators included (tcatbal.txt mixes CRLF and LF).
 */
class AsciiDataRoundTripTest {

    private static <T> List<T> roundTrip(String fileName, FixedWidthCodec<T> codec, int expectedRecords)
            throws Exception {
        byte[] original = Files.readAllBytes(CardDemoData.file(fileName));
        String text = new String(original, StandardCharsets.US_ASCII);
        StringBuilder rebuilt = new StringBuilder(text.length());
        List<T> records = new ArrayList<>();
        int start = 0;
        while (start < text.length()) {
            int nl = text.indexOf('\n', start);
            int end = nl < 0 ? text.length() : nl;
            String terminator = nl < 0 ? "" : "\n";
            if (end > start && text.charAt(end - 1) == '\r') {
                end--;
                terminator = "\r" + terminator;
            }
            String line = text.substring(start, end);
            T record = codec.decode(line);
            records.add(record);
            String encoded = codec.encode(record);
            assertThat(encoded).hasSize(codec.length());
            // A file may store a record with its trailing FILLER spaces trimmed (cardxref.txt).
            String stored = line.length() < codec.length() ? encoded.stripTrailing() : encoded;
            rebuilt.append(stored).append(terminator);
            start = nl < 0 ? text.length() : nl + 1;
        }
        assertThat(rebuilt.toString().getBytes(StandardCharsets.US_ASCII)).isEqualTo(original);
        assertThat(records).hasSize(expectedRecords);
        return records;
    }

    @Test
    void accounts() throws Exception {
        List<AccountRecord> accounts = roundTrip(CardDemoData.ACCOUNTS, AccountRecord.CODEC, 50);
        AccountRecord first = accounts.get(0);
        assertThat(first.acctId()).isEqualTo(1L);
        assertThat(first.acctActiveStatus()).isEqualTo("Y");
        assertThat(first.acctCurrBal()).isEqualTo(new BigDecimal("194.00"));
        assertThat(first.acctCreditLimit()).isEqualTo(new BigDecimal("2020.00"));
        assertThat(first.acctOpenDate()).isEqualTo("2014-11-20");
        // The ASCII sample stores "A000000000" in ACCT-ADDR-ZIP and leaves ACCT-GROUP-ID blank.
        assertThat(first.acctAddrZip()).isEqualTo("A000000000");
        assertThat(first.acctGroupId()).isBlank();
    }

    @Test
    void cardXref() throws Exception {
        List<CardXrefRecord> xref = roundTrip(CardDemoData.CARD_XREF, CardXrefRecord.CODEC, 50);
        assertThat(xref.get(0).xrefCardNum()).isEqualTo("0500024453765740");
        assertThat(xref.get(0).xrefCustId()).isEqualTo(50L);
        assertThat(xref.get(0).xrefAcctId()).isEqualTo(50L);
    }

    @Test
    void tranCatBalances() throws Exception {
        List<TranCatBalRecord> tcat = roundTrip(CardDemoData.TRAN_CAT_BALANCES, TranCatBalRecord.CODEC, 50);
        assertThat(tcat.get(0).tranCatKey()).isEqualTo(new TranCatBalRecord.TranCatKey(1, "01", 1));
        assertThat(tcat.get(0).tranCatBal()).isEqualTo(new BigDecimal("0.00"));
    }

    @Test
    void disclosureGroups() throws Exception {
        List<DisclosureGroupRecord> groups = roundTrip(CardDemoData.DISCLOSURE_GROUPS, DisclosureGroupRecord.CODEC, 51);
        assertThat(groups.get(0).disGroupKey())
                .isEqualTo(new DisclosureGroupRecord.DisGroupKey("A000000000", "01", 1));
        assertThat(groups.get(0).disIntRate()).isEqualTo(new BigDecimal("15.00"));
    }

    @Test
    void customers() throws Exception {
        List<CustomerRecord> customers = roundTrip(CardDemoData.CUSTOMERS, CustomerRecord.CODEC, 50);
        assertThat(customers.get(0).custId()).isEqualTo(1L);
        assertThat(customers.get(0).custFirstName().trim()).isEqualTo("Immanuel");
        assertThat(customers.get(0).custFicoCreditScore()).isEqualTo(274);
    }

    @Test
    void dailyTransactions() throws Exception {
        List<DailyTransactionRecord> daily = roundTrip(CardDemoData.DAILY_TRANSACTIONS, DailyTransactionRecord.CODEC, 300);
        DailyTransactionRecord first = daily.get(0);
        assertThat(first.dalytranId()).isEqualTo("0000000000683580");
        assertThat(first.dalytranTypeCd()).isEqualTo("01");
        assertThat(first.dalytranCatCd()).isEqualTo(1);
        assertThat(first.dalytranAmt()).isEqualTo(new BigDecimal("504.77"));
        assertThat(first.dalytranCardNum()).isEqualTo("4859452612877065");
    }

    @Test
    void transactionAndStatementLayoutsShareTheDailyBytes() throws Exception {
        for (String line : Files.readAllLines(CardDemoData.file(CardDemoData.DAILY_TRANSACTIONS))) {
            TransactionRecord tran = TransactionRecord.CODEC.decode(line);
            assertThat(TransactionRecord.CODEC.encode(tran)).isEqualTo(line);
            StatementTransactionRecord stmt = StatementTransactionRecord.from(tran);
            assertThat(StatementTransactionRecord.CODEC.encode(StatementTransactionRecord.CODEC.decode(
                    StatementTransactionRecord.CODEC.encode(stmt)))).isEqualTo(StatementTransactionRecord.CODEC.encode(stmt));
            assertThat(stmt.trnxKey().trnxCardNum()).isEqualTo(tran.tranCardNum());
            assertThat(stmt.trnxAmt()).isEqualTo(tran.tranAmt());
        }
    }
}
