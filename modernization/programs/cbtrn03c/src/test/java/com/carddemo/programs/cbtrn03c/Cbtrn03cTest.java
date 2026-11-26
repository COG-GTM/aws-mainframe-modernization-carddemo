package com.carddemo.programs.cbtrn03c;

import com.carddemo.programs.cbtrn03c.copybook.ReportLayouts;
import com.carddemo.programs.cbtrn03c.copybook.TranRecord;
import com.carddemo.programs.cbtrn03c.io.InMemoryKeyedInputFile;
import com.carddemo.programs.cbtrn03c.io.InMemorySequentialInputFile;
import com.carddemo.programs.cbtrn03c.io.InMemorySequentialOutputFile;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class Cbtrn03cTest {

    private static final String CARD_A = "4111111111111111";
    private static final String CARD_B = "4222222222222222";
    private static final String DATEPARM = "2022-01-01 2022-07-06";

    private List<String> transactions;
    private InMemorySequentialInputFile tranFile;
    private InMemoryKeyedInputFile xrefFile;
    private InMemoryKeyedInputFile trantypeFile;
    private InMemoryKeyedInputFile trancatgFile;
    private InMemorySequentialOutputFile reportFile;
    private InMemorySequentialInputFile dateparmFile;
    private ByteArrayOutputStream sysoutBytes;

    @BeforeEach
    void setUp() {
        transactions = new ArrayList<>();
        xrefFile = new InMemoryKeyedInputFile(Map.of(
                CARD_A, CARD_A + "000000001" + "00000000011" + " ".repeat(14),
                CARD_B, CARD_B + "000000002" + "00000000022" + " ".repeat(14)));
        trantypeFile = new InMemoryKeyedInputFile(Map.of(
                "01", "01" + pad("Purchase", 50) + "00000000",
                "03", "03" + pad("Credit", 50) + "00000000"));
        trancatgFile = new InMemoryKeyedInputFile(Map.of(
                "010001", "010001" + pad("Regular Sales Draft", 50) + "0000",
                "030001", "030001" + pad("Credit to Account", 50) + "0000"));
        reportFile = new InMemorySequentialOutputFile(Cbtrn03c.TRANREPT_LRECL);
        dateparmFile = new InMemorySequentialInputFile(List.of(pad(DATEPARM, 80)));
        sysoutBytes = new ByteArrayOutputStream();
    }

    // ---- mainline / report structure ----

    @Test
    void mainlineWritesHeadersDetailsAccountTotalsAndFinalTotals() {
        addTx("T1", CARD_A, "01", 1, "10.00", "2022-06-10");
        addTx("T2", CARD_A, "03", 1, "-2.50", "2022-06-10");
        addTx("T3", CARD_B, "01", 1, "100.00", "2022-06-10");

        Cbtrn03c program = program();
        assertEquals(0, program.run());

        List<String> report = trimmed(reportFile.records());
        assertEquals(List.of(
                "DALYREPT                              Daily Transaction Report                 Date Range: 2022-01-01 to 2022-07-06",
                "",
                ReportLayouts.TRANSACTION_HEADER_1.stripTrailing(),
                "-".repeat(133),
                "T1               00000000011 01-Purchase        0001-Regular Sales Draft           POS TERM                10.00",
                "T2               00000000011 03-Credit          0001-Credit to Account             POS TERM      -          2.50",
                "Account Total" + ".".repeat(84) + "+          7.50",
                "-".repeat(133),
                "T3               00000000022 01-Purchase        0001-Regular Sales Draft           POS TERM               100.00",
                // EOF: the last TRAN-AMT is added again to the page total before the page/grand totals.
                "Page Total " + ".".repeat(86) + "+        207.50",
                "-".repeat(133),
                "Grand Total" + ".".repeat(86) + "+        207.50"), report);
        // the last card's account total is never written; its accumulator still holds 100 + 100.
        assertEquals(new BigDecimal("200.00"), program.accountTotal());
        assertEquals(new BigDecimal("207.50"), program.grandTotal());
        assertEquals(new BigDecimal("0.00"), program.pageTotal());
        assertTrue(program.endOfFile());

        List<String> sysout = sysout();
        assertEquals("START OF EXECUTION OF PROGRAM CBTRN03C", sysout.get(0));
        assertEquals("Reporting from 2022-01-01 to 2022-07-06", sysout.get(1));
        assertEquals(transactions.get(0), sysout.get(2));
        assertEquals("TRAN-AMT 0000001000{", sysout.get(5));
        assertEquals("WS-PAGE-TOTAL0000001075{", sysout.get(6));
        assertEquals("END OF EXECUTION OF PROGRAM CBTRN03C", sysout.get(7));
        assertEquals(8, sysout.size());
    }

    @Test
    void pageBreakEveryTwentyLinesWritesPageTotalsAndRepeatsHeaders() {
        for (int i = 1; i <= 17; i++) {
            addTx(String.format("T%02d", i), CARD_A, "01", 1, "1.00", "2022-06-10");
        }
        program().run();
        List<String> report = trimmed(reportFile.records());
        // 4 header lines + 16 details reach line 20; the 17th detail first triggers 1110 + 1120.
        assertTrue(report.get(19).startsWith("T16"));
        assertEquals("Page Total " + ".".repeat(86) + "+         16.00", report.get(20));
        assertEquals("-".repeat(133), report.get(21));
        assertTrue(report.get(22).startsWith("DALYREPT"));
        assertTrue(report.get(26).startsWith("T17"));
        assertEquals("Page Total " + ".".repeat(86) + "+          2.00", report.get(27));
        assertEquals("Grand Total" + ".".repeat(86) + "+         18.00", report.get(29));
        assertEquals(30, report.size());
    }

    @Test
    void lineCounterCountsAccountTotalLinesTowardsThePageSize() {
        // 4 headers + 15 details (19 lines), account totals add 2 -> 21: MOD(21,20) != 0, no page break.
        for (int i = 1; i <= 15; i++) {
            addTx(String.format("A%02d", i), CARD_A, "01", 1, "1.00", "2022-06-10");
        }
        addTx("B01", CARD_B, "01", 1, "1.00", "2022-06-10");
        Cbtrn03c program = program();
        program.run();
        List<String> report = trimmed(reportFile.records());
        assertTrue(report.get(19).startsWith("Account Total"));
        assertTrue(report.get(21).startsWith("B01"));
        // EOF page totals add 2 more lines.
        assertEquals(24, program.lineCounter());
    }

    @Test
    void dateRangeBoundsAreInclusive() {
        addTx("T1", CARD_A, "01", 1, "1.00", "2022-01-01");
        addTx("T2", CARD_A, "01", 1, "1.00", "2022-07-06");
        program().run();
        long details = reportFile.records().stream().filter(l -> l.startsWith("T1 ") || l.startsWith("T2 ")).count();
        assertEquals(2, details);
    }

    @Test
    void recordOutsideDateRangeEndsTheLoopViaNextSentence() {
        addTx("T1", CARD_A, "01", 1, "1.00", "2022-06-10");
        addTx("T2", CARD_A, "01", 1, "1.00", "2022-07-07");
        addTx("T3", CARD_A, "01", 1, "1.00", "2022-06-10");
        Cbtrn03c program = program();
        assertEquals(0, program.run());
        List<String> report = trimmed(reportFile.records());
        assertEquals(5, report.size());
        assertTrue(report.get(4).startsWith("T1"));
        assertFalse(program.endOfFile());
        List<String> sysout = sysout();
        assertEquals("END OF EXECUTION OF PROGRAM CBTRN03C", sysout.get(sysout.size() - 1));
        assertFalse(sysout.stream().anyMatch(l -> l.startsWith("TRAN-AMT")));
    }

    @Test
    void emptyTransactionFileProducesAnEmptyReport() {
        assertEquals(0, program().run());
        assertTrue(reportFile.records().isEmpty());
        assertEquals(List.of("START OF EXECUTION OF PROGRAM CBTRN03C", "Reporting from 2022-01-01 to 2022-07-06",
                "END OF EXECUTION OF PROGRAM CBTRN03C"), sysout());
    }

    @Test
    void dateparmEndOfFileSkipsTheReport() {
        dateparmFile = new InMemorySequentialInputFile(List.of());
        addTx("T1", CARD_A, "01", 1, "1.00", "2022-06-10");
        Cbtrn03c program = program();
        assertEquals(0, program.run());
        assertTrue(program.endOfFile());
        assertTrue(reportFile.records().isEmpty());
        assertEquals(List.of("START OF EXECUTION OF PROGRAM CBTRN03C", "END OF EXECUTION OF PROGRAM CBTRN03C"), sysout());
    }

    @Test
    void dateparmReadTakesStartAndEndDateAroundTheFiller() {
        dateparmFile = new InMemorySequentialInputFile(List.of("2022-06-01X2022-06-30 trailing text"));
        Cbtrn03c program = program();
        program.run();
        assertEquals("2022-06-01", program.startDate());
        assertEquals("2022-06-30", program.endDate());
    }

    @Test
    void accumulatorsTruncateHighOrderDigitsOnOverflow() {
        addTx("T1", CARD_A, "01", 1, "999999999.99", "2022-06-10");
        addTx("T2", CARD_A, "01", 1, "0.02", "2022-06-10");
        program().run();
        List<String> report = trimmed(reportFile.records());
        assertEquals("Page Total " + ".".repeat(86) + "+           .03", report.get(6));
    }

    // ---- error paths ----

    @Test
    void dateparmReadErrorAbends() {
        dateparmFile = new InMemorySequentialInputFile(List.of()).failRead(1, "30");
        assertAbend("ERROR READING DATEPARM FILE", "FILE STATUS IS: NNNN0030");
    }

    @Test
    void transactionReadErrorAbends() {
        addTx("T1", CARD_A, "01", 1, "1.00", "2022-06-10");
        tranFile().failRead(2, "34");
        assertAbend("ERROR READING TRANSACTION FILE", "FILE STATUS IS: NNNN0034");
        assertEquals(5, reportFile.records().size());
    }

    @Test
    void reportWriteErrorAbends() {
        addTx("T1", CARD_A, "01", 1, "1.00", "2022-06-10");
        reportFile.failWrite(3, "37");
        assertAbend("ERROR WRITING REPTFILE", "FILE STATUS IS: NNNN0037");
    }

    @ParameterizedTest(name = "open failure on {0}")
    @CsvSource({
        "TRANFILE, ERROR OPENING TRANFILE",
        "TRANREPT, ERROR OPENING REPTFILE",
        "CARDXREF, ERROR OPENING CROSS REF FILE",
        "TRANTYPE, ERROR OPENING TRANSACTION TYPE FILE",
        "TRANCATG, ERROR OPENING TRANSACTION CATG FILE",
        "DATEPARM, ERROR OPENING DATE PARM FILE",
    })
    void openFailuresAbendWithParagraphMessage(String dd, String message) {
        switch (dd) {
            case "TRANFILE" -> tranFile().failOpen("35");
            case "TRANREPT" -> reportFile.failOpen("35");
            case "CARDXREF" -> xrefFile.failOpen("35");
            case "TRANTYPE" -> trantypeFile.failOpen("35");
            case "TRANCATG" -> trancatgFile.failOpen("35");
            default -> dateparmFile.failOpen("35");
        }
        assertAbend(message, "FILE STATUS IS: NNNN0035");
    }

    @ParameterizedTest(name = "close failure on {0}")
    @CsvSource({
        "TRANFILE, ERROR CLOSING POSTED TRANSACTION FILE",
        "TRANREPT, ERROR CLOSING REPORT FILE",
        "CARDXREF, ERROR CLOSING CROSS REF FILE",
        "TRANTYPE, ERROR CLOSING TRANSACTION TYPE FILE",
        "TRANCATG, ERROR CLOSING TRANSACTION CATG FILE",
        "DATEPARM, ERROR CLOSING DATE PARM FILE",
    })
    void closeFailuresAbendWithParagraphMessage(String dd, String message) {
        switch (dd) {
            case "TRANFILE" -> tranFile().failClose("42");
            case "TRANREPT" -> reportFile.failClose("42");
            case "CARDXREF" -> xrefFile.failClose("42");
            case "TRANTYPE" -> trantypeFile.failClose("42");
            case "TRANCATG" -> trancatgFile.failClose("42");
            default -> dateparmFile.failClose("42");
        }
        assertAbend(message, "FILE STATUS IS: NNNN0042");
    }

    @Test
    void unknownCardNumberAbendsWithInvalidKey() {
        addTx("T1", "9999999999999999", "01", 1, "1.00", "2022-06-10");
        assertAbend("INVALID CARD NUMBER : 9999999999999999", "FILE STATUS IS: NNNN0023");
    }

    @Test
    void unknownTransactionTypeAbendsWithInvalidKey() {
        addTx("T1", CARD_A, "09", 1, "1.00", "2022-06-10");
        assertAbend("INVALID TRANSACTION TYPE : 09", "FILE STATUS IS: NNNN0023");
    }

    @Test
    void unknownCategoryAbendsWithInvalidKey() {
        addTx("T1", CARD_A, "01", 7, "1.00", "2022-06-10");
        assertAbend("INVALID TRAN CATG KEY : 010007", "FILE STATUS IS: NNNN0023");
    }

    @Test
    void xrefIsReadOnlyWhenTheCardNumberChanges() {
        addTx("T1", CARD_A, "01", 1, "1.00", "2022-06-10");
        addTx("T2", CARD_A, "01", 1, "1.00", "2022-06-10");
        xrefFile.failRead(CARD_A, "23");
        addTx("T3", CARD_B, "01", 1, "1.00", "2022-06-10");
        // CARD_A is looked up once (and fails) before anything is written.
        assertAbend("INVALID CARD NUMBER : " + CARD_A, "FILE STATUS IS: NNNN0023");
    }

    @Test
    void xrefLookupIsSkippedForARepeatedCardAndRedoneForANewCard() {
        addTx("T1", CARD_A, "01", 1, "1.00", "2022-06-10");
        addTx("T2", CARD_A, "01", 1, "1.00", "2022-06-10");
        addTx("T3", CARD_B, "01", 1, "1.00", "2022-06-10");
        addTx("T4", CARD_A, "01", 1, "1.00", "2022-06-10");
        List<String> keys = new ArrayList<>();
        InMemoryKeyedInputFile delegate = xrefFile;
        com.carddemo.programs.cbtrn03c.io.KeyedInputFile counting = new com.carddemo.programs.cbtrn03c.io.KeyedInputFile() {
            public String open() { return delegate.open(); }
            public com.carddemo.programs.cbtrn03c.io.ReadResult read(String key) { keys.add(key); return delegate.read(key); }
            public String close() { return delegate.close(); }
        };
        assertEquals(0, new Cbtrn03c(tranFile(), counting, trantypeFile, trancatgFile, reportFile, dateparmFile,
                new PrintStream(sysoutBytes, true, StandardCharsets.ISO_8859_1)).run());
        assertEquals(List.of(CARD_A, CARD_B, CARD_A), keys);
    }

    @Test
    void nonInvalidKeyLookupErrorKeepsThePreviousRecordAreaAndContinues() {
        addTx("T1", CARD_A, "01", 1, "1.00", "2022-06-10");
        addTx("T2", CARD_B, "01", 1, "1.00", "2022-06-10");
        xrefFile.failRead(CARD_B, "30");
        assertEquals(0, program().run());
        List<String> report = trimmed(reportFile.records());
        assertTrue(report.get(7).startsWith("T2               00000000011 "), report.get(7));
    }

    @ParameterizedTest(name = "IO-STATUS [{0}] -> {1}")
    @CsvSource({
        "00, 0000",
        "23, 0023",
        "35, 0035",
        "9A, 9065",
        "'9\u0001', 9001",
        "'4 ', 4032",
    })
    void displayIoStatusFormatsIoStatus04(String ioStatus, String expected) {
        assertEquals(expected, Cbtrn03c.formatIoStatus(ioStatus));
    }

    // ---- helpers ----

    private void assertAbend(String message, String fileStatusLine) {
        Cbtrn03c program = program();
        AbendException e = assertThrows(AbendException.class, program::run);
        assertEquals(999, e.abendCode());
        List<String> sysout = sysout();
        int n = sysout.size();
        assertEquals(message, sysout.get(n - 3));
        assertEquals(fileStatusLine, sysout.get(n - 2));
        assertEquals("ABENDING PROGRAM", sysout.get(n - 1));
    }

    private InMemorySequentialInputFile tranFile() {
        if (tranFile == null) {
            tranFile = new InMemorySequentialInputFile(transactions);
        }
        return tranFile;
    }

    private Cbtrn03c program() {
        return new Cbtrn03c(tranFile(), xrefFile, trantypeFile, trancatgFile, reportFile, dateparmFile,
                new PrintStream(sysoutBytes, true, StandardCharsets.ISO_8859_1));
    }

    private List<String> sysout() {
        return sysoutBytes.toString(StandardCharsets.ISO_8859_1).lines().toList();
    }

    private void addTx(String id, String card, String type, int cat, String amount, String procDate) {
        transactions.add(new TranRecord(id, type, cat, "POS TERM", "Purchase", new BigDecimal(amount), 800000000L,
                "Merchant", "City", "12345", card, "2022-06-10 19:27:53.000000",
                procDate + " 19:27:53.000000", "").toFixedWidth());
    }

    private static List<String> trimmed(List<String> lines) {
        return lines.stream().map(String::stripTrailing).toList();
    }

    private static String pad(String s, int n) {
        return String.format("%-" + n + "s", s);
    }
}
