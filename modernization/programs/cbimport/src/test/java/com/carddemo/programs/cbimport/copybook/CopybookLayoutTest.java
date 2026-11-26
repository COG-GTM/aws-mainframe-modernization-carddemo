package com.carddemo.programs.cbimport.copybook;

import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.charset.Charset;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

class CopybookLayoutTest {

    private static final Charset EBCDIC = Charset.forName("IBM037");

    @Test
    void outputRecordsMatchJclLrecl() {
        assertEquals(500, CustomerRecord.initialized().toFixedWidth().length());
        assertEquals(300, AccountRecord.initialized().toFixedWidth().length());
        assertEquals(50, CardXrefRecord.initialized().toFixedWidth().length());
        assertEquals(350, TranRecord.initialized().toFixedWidth().length());
        assertEquals(150, CardRecord.initialized().toFixedWidth().length());
    }

    @Test
    void initializeSetsNumericsToZeroAndAlphanumericsToSpaces() {
        String acct = AccountRecord.initialized().toFixedWidth();
        assertEquals("00000000000 00000000000{", acct.substring(0, 24));
        assertEquals(" ".repeat(300 - 122), acct.substring(122));
        assertEquals(" ".repeat(16) + "000000000" + "00000000000" + " ".repeat(14),
                CardXrefRecord.initialized().toFixedWidth());
    }

    @Test
    void everyRedefinesViewOfExportRecordDataIs460Bytes() {
        var cust = new ExportRecord.CustomerData(BigInteger.ONE, "F", "M", "L", List.of("1", "2", "3"),
                "NY", "USA", "12345", List.of("P1", "P2"), BigInteger.TWO, "G", "1980-01-01", "E", "Y",
                BigInteger.valueOf(700));
        var acct = new ExportRecord.AccountData(BigInteger.ONE, "Y", BigDecimal.ONE, BigDecimal.ONE,
                BigDecimal.ONE, "", "", "", BigDecimal.ONE, BigDecimal.ONE, "", "");
        var tran = new ExportRecord.TransactionData("T", "01", BigInteger.ONE, "S", "D", BigDecimal.ONE,
                BigInteger.ONE, "", "", "", "", "", "");
        var xref = new ExportRecord.CardXrefData("C", BigInteger.ONE, BigInteger.ONE);
        var card = new ExportRecord.CardData("C", BigInteger.ONE, BigInteger.ONE, "", "", "Y");
        assertEquals(460, cust.encode(EBCDIC).length);
        assertEquals(460, acct.encode(EBCDIC).length);
        assertEquals(460, tran.encode(EBCDIC).length);
        assertEquals(460, xref.encode(EBCDIC).length);
        assertEquals(460, card.encode(EBCDIC).length);
        assertEquals(500, ExportRecord.encode("C", "", 1, "", "", cust.encode(EBCDIC), EBCDIC).length);
    }

    @Test
    void outputRecordsRoundTripThroughFixedWidth() {
        AccountRecord acct = new AccountRecord(12345678901L, "Y", new BigDecimal("-1.23"),
                new BigDecimal("5000.00"), new BigDecimal("100.50"), "2020-01-01", "2025-01-01", "2024-01-01",
                new BigDecimal("0.01"), new BigDecimal("-99.99"), "12345", "GRP1");
        assertEquals(acct, AccountRecord.parse(acct.toFixedWidth()));

        TranRecord tran = new TranRecord("0000000000683580", "01", 1, "POS TERM", "Purchase",
                new BigDecimal("-504.77"), 800000000L, "Shop", "City", "72112", "4859452612877065",
                "2022-06-10 19:27:53.000000", "");
        assertEquals(tran, TranRecord.parse(tran.toFixedWidth()));

        CardRecord card = new CardRecord("0500024453765740", 50, 747, "Aniya Von", "2023-03-09", "Y");
        assertEquals(card, CardRecord.parse(card.toFixedWidth()));
        CardXrefRecord xref = new CardXrefRecord("0500024453765740", 50, 50);
        assertEquals("050002445376574000000005000000000050", xref.toFixedWidth().stripTrailing());
        assertEquals(xref, CardXrefRecord.parse(xref.toFixedWidth()));
        CustomerRecord cust = CustomerRecord.parse(CustomerRecord.initialized().toFixedWidth());
        assertEquals(CustomerRecord.initialized(), cust);
    }

    @Test
    void outputRecordsParseTheAsciiSampleData() {
        AccountRecord acct = AccountRecord.parse("00000000001Y00000001940{00000020200{00000010200{"
                + "2014-11-202025-05-202025-05-2000000000000{00000000000{A000000000");
        assertEquals(1L, acct.acctId());
        assertEquals(new BigDecimal("194.00"), acct.currBal());
        assertEquals(new BigDecimal("2020.00"), acct.creditLimit());
        assertEquals("A000000000", acct.addrZip());
    }
}
