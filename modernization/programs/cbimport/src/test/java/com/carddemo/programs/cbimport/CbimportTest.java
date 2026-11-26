package com.carddemo.programs.cbimport;

import com.carddemo.programs.cbimport.copybook.AccountRecord;
import com.carddemo.programs.cbimport.copybook.ExportRecord;
import com.carddemo.programs.cbimport.copybook.TranRecord;
import com.carddemo.programs.cbimport.io.InMemoryExportReader;
import com.carddemo.programs.cbimport.io.InMemoryRecordWriter;
import com.carddemo.programs.cbimport.io.RecordWriter;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CbimportTest {

    static final Charset EBCDIC = Charset.forName("IBM037");
    static final Clock CLOCK = Clock.fixed(Instant.parse("2026-11-26T06:22:33.456Z"), ZoneId.of("Australia/Sydney"));

    final InMemoryRecordWriter cust = new InMemoryRecordWriter();
    final InMemoryRecordWriter acct = new InMemoryRecordWriter();
    final InMemoryRecordWriter xref = new InMemoryRecordWriter();
    final InMemoryRecordWriter tran = new InMemoryRecordWriter();
    final InMemoryRecordWriter card = new InMemoryRecordWriter();
    final InMemoryRecordWriter err = new InMemoryRecordWriter();
    final ByteArrayOutputStream sysoutBytes = new ByteArrayOutputStream();
    final PrintStream sysout = new PrintStream(sysoutBytes, true, StandardCharsets.UTF_8);

    static byte[] export(String type, long seq, byte[] data) {
        return ExportRecord.encode(type, "2025-09-28 22:53:40.000000", seq, "0001", "NORTH", data, EBCDIC);
    }

    static byte[] customer(long seq, BigInteger custId) {
        return export("C", seq, new ExportRecord.CustomerData(custId, "IMMANUEL", "MADELINE", "MATHEUS",
                List.of("618 DESHAUN ROUTE", "APT. 802", "ALTENWERTHSHIRE"), "NY", "USA", "12547",
                List.of("(908)200-8310", "(908)600-8684"), BigInteger.valueOf(20973888),
                "00000000000049368437", "1979-06-08", "0053581756", "Y", BigInteger.valueOf(300)).encode(EBCDIC));
    }

    static byte[] account(long seq, BigDecimal currBal, BigDecimal currCycDebit) {
        return export("A", seq, new ExportRecord.AccountData(BigInteger.ONE, "Y", currBal,
                new BigDecimal("202.00"), new BigDecimal("1020.00"), "2020-10-22", "2025-06-20", "2025-05-20",
                BigDecimal.ZERO, currCycDebit, "", "").encode(EBCDIC));
    }

    static byte[] xrefRec(long seq, BigInteger acctId) {
        return export("X", seq, new ExportRecord.CardXrefData("0500024453765740", BigInteger.valueOf(50), acctId)
                .encode(EBCDIC));
    }

    static byte[] tranRec(long seq, BigDecimal amt) {
        return export("T", seq, new ExportRecord.TransactionData("0000000000683580", "01", BigInteger.ONE,
                "POS TERM", "Purchase at Abshire-Lowe", amt, BigInteger.valueOf(800000000), "Abshire-Lowe",
                "North Enoshaven", "72112", "4859452612877065", "2022-06-10 19:27:53.000000", "").encode(EBCDIC));
    }

    static byte[] cardRec(long seq, BigInteger cvv) {
        return export("D", seq, new ExportRecord.CardData("0500024453765740", BigInteger.valueOf(50), cvv,
                "Aniya Von", "2023-03-09", "Y").encode(EBCDIC));
    }

    Cbimport program(InMemoryExportReader reader) {
        return program(reader, cust, acct, xref, tran, card, err);
    }

    Cbimport program(InMemoryExportReader reader, RecordWriter c, RecordWriter a, RecordWriter x,
                     RecordWriter t, RecordWriter d, RecordWriter e) {
        return new Cbimport(new CbimportFiles(reader, c, a, x, t, d, e), EBCDIC, CLOCK, sysout);
    }

    List<String> sysoutLines() {
        return List.of(sysoutBytes.toString(StandardCharsets.UTF_8).split("\\R"));
    }

    // 0000-MAIN-PROCESSING / 1000-INITIALIZE / 3000-VALIDATE-IMPORT / 4000-FINALIZE
    @Test
    void emptyExportFileDisplaysFullJobLogWithZeroCounters() {
        InMemoryExportReader reader = new InMemoryExportReader(List.of());
        assertEquals(0, program(reader).run());
        assertEquals(List.of(
                "CBIMPORT: Starting Customer Data Import",
                "CBIMPORT: Import Date: 2026-11-26",
                "CBIMPORT: Import Time: 17:22:33",
                "CBIMPORT: Import validation completed",
                "CBIMPORT: No validation errors detected",
                "CBIMPORT: Import completed",
                "CBIMPORT: Total Records Read: 000000000",
                "CBIMPORT: Customers Imported: 000000000",
                "CBIMPORT: Accounts Imported: 000000000",
                "CBIMPORT: XRefs Imported: 000000000",
                "CBIMPORT: Transactions Imported: 000000000",
                "CBIMPORT: Cards Imported: 000000000",
                "CBIMPORT: Errors Written: 000000000",
                "CBIMPORT: Unknown Record Types: 000000000"), sysoutLines());
        assertTrue(reader.isClosed());
        for (InMemoryRecordWriter w : List.of(cust, acct, xref, tran, card, err)) {
            assertTrue(w.isClosed());
            assertTrue(w.records().isEmpty());
        }
    }

    // 2000-PROCESS-EXPORT-FILE / 2200-PROCESS-RECORD-BY-TYPE
    @Test
    void routesEachRecordTypeToItsFileAndCounts() {
        List<byte[]> in = List.of(customer(1, BigInteger.ONE), account(2, BigDecimal.ZERO, BigDecimal.ZERO),
                xrefRec(3, BigInteger.valueOf(50)), tranRec(4, BigDecimal.ONE), cardRec(5, BigInteger.valueOf(747)),
                export("Z", 6, new byte[0]), tranRec(7, BigDecimal.TEN));
        Cbimport p = program(new InMemoryExportReader(in));
        p.run();
        ImportStatistics s = p.statistics();
        assertEquals(7, s.totalRecordsRead());
        assertEquals(1, s.customerRecordsImported());
        assertEquals(1, s.accountRecordsImported());
        assertEquals(1, s.xrefRecordsImported());
        assertEquals(2, s.tranRecordsImported());
        assertEquals(1, s.cardRecordsImported());
        assertEquals(1, s.errorRecordsWritten());
        assertEquals(1, s.unknownRecordTypeCount());
        assertEquals(List.of(1, 1, 1, 2, 1, 1), List.of(cust.records().size(), acct.records().size(),
                xref.records().size(), tran.records().size(), card.records().size(), err.records().size()));
        assertTrue(sysoutLines().contains("CBIMPORT: Transactions Imported: 000000002"));
        assertTrue(sysoutLines().contains("CBIMPORT: Unknown Record Types: 000000001"));
    }

    @Test
    void recordTypeIsCaseSensitive() {
        Cbimport p = program(new InMemoryExportReader(List.of(export("c", 1, new byte[0]))));
        p.run();
        assertEquals(0, p.statistics().customerRecordsImported());
        assertEquals(1, p.statistics().unknownRecordTypeCount());
    }

    // 2300-PROCESS-CUSTOMER-RECORD
    @Test
    void customerRecordMapsEveryField() {
        program(new InMemoryExportReader(List.of(customer(1, BigInteger.ONE)))).run();
        String rec = cust.records().get(0);
        assertEquals(500, rec.length());
        String expected = "000000001" + pad("IMMANUEL", 25) + pad("MADELINE", 25) + pad("MATHEUS", 25)
                + pad("618 DESHAUN ROUTE", 50) + pad("APT. 802", 50) + pad("ALTENWERTHSHIRE", 50)
                + "NY" + "USA" + pad("12547", 10) + pad("(908)200-8310", 15) + pad("(908)600-8684", 15)
                + "020973888" + "00000000000049368437" + "1979-06-08" + "0053581756" + "Y" + "300";
        assertEquals(expected + " ".repeat(168), rec);
    }

    @Test
    void customerIdFromUnsignedFullwordIsTruncatedToNineDigits() {
        program(new InMemoryExportReader(List.of(customer(1, new BigInteger("4294967295"))))).run();
        assertEquals("294967295", cust.records().get(0).substring(0, 9));
    }

    // 2400-PROCESS-ACCOUNT-RECORD
    @Test
    void accountRecordConvertsPackedAndBinaryToSignedDisplay() {
        program(new InMemoryExportReader(List.of(
                account(1, new BigDecimal("-1234.56"), new BigDecimal("-987654321.09"))))).run();
        String rec = acct.records().get(0);
        assertEquals(300, rec.length());
        assertEquals("00000000001" + "Y" + "00000012345O" + "00000002020{" + "00000010200{"
                + "2020-10-22" + "2025-06-20" + "2025-05-20" + "00000000000{" + "09876543210R"
                + " ".repeat(20) + " ".repeat(178), rec);
        AccountRecord parsed = AccountRecord.parse(rec);
        assertEquals(new BigDecimal("-1234.56"), parsed.currBal());
        assertEquals(new BigDecimal("-987654321.09"), parsed.currCycDebit());
    }

    @Test
    void accountCompDebitBeyondTwelveDigitsIsTruncated() {
        program(new InMemoryExportReader(List.of(
                account(1, BigDecimal.ZERO, new BigDecimal("12345678901234.56"))))).run();
        assertEquals(new BigDecimal("5678901234.56"), AccountRecord.parse(acct.records().get(0)).currCycDebit());
    }

    // 2500-PROCESS-XREF-RECORD
    @Test
    void xrefRecordMapsCardCustomerAndAccount() {
        program(new InMemoryExportReader(List.of(xrefRec(1, BigInteger.valueOf(50))))).run();
        assertEquals("050002445376574000000005000000000050" + " ".repeat(14), xref.records().get(0));
    }

    @Test
    void xrefAccountFromDoublewordIsTruncatedToElevenDigits() {
        program(new InMemoryExportReader(List.of(xrefRec(1, new BigInteger("123456789012345"))))).run();
        assertEquals("56789012345", xref.records().get(0).substring(25, 36));
    }

    // 2600-PROCESS-TRAN-RECORD
    @Test
    void transactionRecordMapsEveryField() {
        program(new InMemoryExportReader(List.of(tranRec(1, new BigDecimal("504.77"))))).run();
        String rec = tran.records().get(0);
        assertEquals(350, rec.length());
        assertEquals("0000000000683580" + "01" + "0001" + pad("POS TERM", 10) + pad("Purchase at Abshire-Lowe", 100)
                + "0000005047G" + "800000000" + pad("Abshire-Lowe", 50) + pad("North Enoshaven", 50)
                + pad("72112", 10) + "4859452612877065" + "2022-06-10 19:27:53.000000" + " ".repeat(26)
                + " ".repeat(20), rec);
    }

    @Test
    void negativeTransactionAmountKeepsSign() {
        program(new InMemoryExportReader(List.of(tranRec(1, new BigDecimal("-998.33"))))).run();
        assertEquals("0000009983L", tran.records().get(0).substring(132, 143));
        assertEquals(new BigDecimal("-998.33"), TranRecord.parse(tran.records().get(0)).amt());
    }

    // 2650-PROCESS-CARD-RECORD
    @Test
    void cardRecordMapsEveryField() {
        program(new InMemoryExportReader(List.of(cardRec(1, BigInteger.valueOf(747))))).run();
        assertEquals("0500024453765740" + "00000000050" + "747" + pad("Aniya Von", 50) + "2023-03-09" + "Y"
                + " ".repeat(59), card.records().get(0));
    }

    @Test
    void cardCvvFromHalfwordIsTruncatedToThreeDigits() {
        program(new InMemoryExportReader(List.of(cardRec(1, BigInteger.valueOf(65535))))).run();
        assertEquals("535", card.records().get(0).substring(27, 30));
    }

    // 2700-PROCESS-UNKNOWN-RECORD / 2750-WRITE-ERROR
    @Test
    void unknownRecordWritesPipeDelimitedErrorRecord() {
        program(new InMemoryExportReader(List.of(export("Q", 12_345_678, new byte[0])))).run();
        String rec = err.records().get(0);
        assertEquals(132, rec.length());
        assertEquals("2026112617223345+1100     " + "|" + "Q" + "|" + "2345678" + "|"
                + pad("Unknown record type encountered", 50) + " ".repeat(43) + "  ", rec);
    }

    @Test
    void errorWriteFailureIsDisplayedButNotFatalAndStillCounted() {
        err.failWrite("34");
        Cbimport p = program(new InMemoryExportReader(List.of(export("Q", 1, new byte[0]), customer(2, BigInteger.ONE))));
        assertEquals(0, p.run());
        assertTrue(sysoutLines().contains("ERROR: Writing error record, Status: 34"));
        assertEquals(1, p.statistics().errorRecordsWritten());
        assertEquals(1, p.statistics().customerRecordsImported());
        assertFalse(sysoutLines().contains("CBIMPORT: ABENDING PROGRAM"));
    }

    // 1100-OPEN-FILES / 9999-ABEND-PROGRAM
    @ParameterizedTest
    @CsvSource({"0,EXPORT-INPUT", "1,CUSTOMER-OUTPUT", "2,ACCOUNT-OUTPUT", "3,XREF-OUTPUT",
            "4,TRANSACTION-OUTPUT", "5,CARD-OUTPUT", "6,ERROR-OUTPUT"})
    void openFailureAbendsWithFileNameAndStatus(int failing, String fileName) {
        InMemoryExportReader reader = new InMemoryExportReader(List.of());
        List<InMemoryRecordWriter> writers = new ArrayList<>(List.of(cust, acct, xref, tran, card, err));
        if (failing == 0) {
            reader.failOpen("35");
        } else {
            writers.get(failing - 1).failOpen("35");
        }
        Cbimport p = program(reader, cust, acct, xref, tran, card, err);
        assertThrows(AbendException.class, p::run);
        assertEquals(List.of(
                "CBIMPORT: Starting Customer Data Import",
                "ERROR: Cannot open " + fileName + ", Status: 35",
                "CBIMPORT: ABENDING PROGRAM"), sysoutLines());
    }

    @Test
    void missingCardoutDdAbendsAsWithTheShippedJcl() {
        Cbimport p = program(new InMemoryExportReader(List.of()), cust, acct, xref, tran,
                RecordWriter.unallocated(), err);
        assertThrows(AbendException.class, p::run);
        assertTrue(sysoutLines().contains("ERROR: Cannot open CARD-OUTPUT, Status: 35"));
    }

    // 2100-READ-EXPORT-RECORD
    @Test
    void readErrorAbendsAfterProcessingEarlierRecords() {
        InMemoryExportReader reader = new InMemoryExportReader(List.of(customer(1, BigInteger.ONE),
                customer(2, BigInteger.TWO))).failRead(1, "30");
        Cbimport p = program(reader);
        assertThrows(AbendException.class, p::run);
        assertEquals(1, cust.records().size());
        List<String> lines = sysoutLines();
        assertEquals("ERROR: Reading EXPORT-INPUT, Status: 30", lines.get(lines.size() - 2));
        assertEquals("CBIMPORT: ABENDING PROGRAM", lines.get(lines.size() - 1));
        assertFalse(lines.contains("CBIMPORT: Import completed"));
    }

    @Test
    void abendClosesEveryFileBeforePropagating() {
        InMemoryExportReader reader = new InMemoryExportReader(List.of(customer(1, BigInteger.ONE))).failRead(0, "30");
        assertThrows(AbendException.class, program(reader)::run);
        assertTrue(reader.isClosed());
        for (InMemoryRecordWriter w : List.of(cust, acct, xref, tran, card, err)) {
            assertTrue(w.isClosed());
        }
    }

    @ParameterizedTest
    @CsvSource({"C,customer", "A,account", "X,xref", "T,transaction", "D,card"})
    void writeFailureAbendsWithRecordKindAndStatus(String type, String kind) {
        InMemoryRecordWriter target = switch (type) {
            case "C" -> cust;
            case "A" -> acct;
            case "X" -> xref;
            case "T" -> tran;
            default -> card;
        };
        target.failWrite("37");
        byte[] rec = switch (type) {
            case "C" -> customer(1, BigInteger.ONE);
            case "A" -> account(1, BigDecimal.ONE, BigDecimal.ONE);
            case "X" -> xrefRec(1, BigInteger.ONE);
            case "T" -> tranRec(1, BigDecimal.ONE);
            default -> cardRec(1, BigInteger.ONE);
        };
        Cbimport p = program(new InMemoryExportReader(List.of(rec)));
        assertThrows(AbendException.class, p::run);
        List<String> lines = sysoutLines();
        assertEquals("ERROR: Writing " + kind + " record, Status: 37", lines.get(lines.size() - 2));
        assertEquals("CBIMPORT: ABENDING PROGRAM", lines.get(lines.size() - 1));
    }

    @Test
    void countersWrapAtNineDigitsLikeAddWithoutSizeError() {
        assertEquals(0, ImportStatistics.add1(999_999_999L));
        assertEquals("000000000", ImportStatistics.display(ImportStatistics.add1(999_999_999L)));
    }

    @Test
    void currentDateMatchesCobolIntrinsicFormat() {
        assertEquals("2026112617223345+1100", CurrentDate.of(CLOCK));
        assertEquals("2026112521223345-0900",
                CurrentDate.of(Clock.fixed(Instant.parse("2026-11-26T06:22:33.456Z"), ZoneId.of("-09:00"))));
    }

    static String pad(String s, int len) {
        return s + " ".repeat(len - s.length());
    }
}
