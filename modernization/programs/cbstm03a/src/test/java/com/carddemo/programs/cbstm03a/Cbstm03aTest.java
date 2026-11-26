package com.carddemo.programs.cbstm03a;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.programs.cbstm03a.copybook.M03bArea;
import com.carddemo.programs.cbstm03a.io.InMemoryRecordWriter;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class Cbstm03aTest {

    private static final String CARD_A = "1111111111111111";
    private static final String CARD_B = "2222222222222222";
    private static final String CARD_C = "3333333333333333";

    private static List<String> oneCardTrnx() {
        return List.of(TestData.trnx(CARD_A, "0000000000000001", "Purchase at Shop", "183.88"),
                TestData.trnx(CARD_A, "0000000000000002", "Return item", "-47.88"));
    }

    private static TestData.Run oneCardRun() {
        return TestData.run(oneCardTrnx(), List.of(TestData.xref(CARD_A, 1, 10)),
                List.of(TestData.customer(1)), List.of(TestData.account(10, "-1234.56")));
    }

    /** Wraps CBSTM03B so the given DD/operation returns a forced status code. */
    private static Cbstm03bCall forcing(Cbstm03bCall delegate, String dd, M03bArea.Oper oper, String rc) {
        return area -> {
            delegate.call(area);
            if (area.dd().trim().equals(dd) && area.oper() == oper) {
                area.rc(rc);
            }
        };
    }

    // 5000-CREATE-STATEMENT / 6000-WRITE-TRANS / 4000-TRNXFILE-GET: exact STMTFILE layout
    @Test
    void statementFileHasExactCobolLayout() {
        TestData.Run run = oneCardRun();
        assertNull(run.abend());
        List<String> expected = List.of(
                "*******************************START OF STATEMENT*******************************",
                "Jane Q Public",
                "1 Main St",
                "Apt 2",
                "Springfield IL USA 62701",
                "-".repeat(80),
                "                                 Basic Details",
                "-".repeat(80),
                "Account ID         :00000000010",
                "Current Balance    :000001234.56-",
                "FICO Score         :700",
                "-".repeat(80),
                "                              TRANSACTION SUMMARY",
                "-".repeat(80),
                "Tran ID         Tran Details                                         Tran Amount",
                "-".repeat(80),
                "0000000000000001 Purchase at Shop                                 $      183.88",
                "0000000000000002 Return item                                      $       47.88-",
                "-".repeat(80),
                "Total EXP:                                                        $      136.00",
                "********************************END OF STATEMENT********************************");
        assertEquals(expected, run.stmt().stream().map(TestData::rtrim).toList());
        run.stmt().forEach(r -> assertEquals(80, r.length()));
    }

    // 5100-WRITE-HTML-HEADER, 5200-WRITE-HTML-NMADBS, 6000-WRITE-TRANS, 4000 HTML trailer
    @Test
    void htmlFileHasExactCobolContent() {
        TestData.Run run = oneCardRun();
        List<String> html = run.html().stream().map(TestData::rtrim).toList();
        run.html().forEach(r -> assertEquals(100, r.length()));
        assertEquals("<!DOCTYPE html>", html.get(0));
        assertEquals("<h3>Statement for Account Number: 00000000010         </h3>", html.get(10));
        assertEquals("<p style=\"font-size:16px\">Bank of XYZ</p>", html.get(15));
        assertEquals("<p style=\"font-size:16px\">Jane Q Public  </p>", html.get(22));
        assertEquals("<p>1 Main St  </p>", html.get(23));
        assertEquals("<p>Apt 2  </p>", html.get(24));
        assertEquals("<p>Springfield IL USA 62701  </p>", html.get(25));
        assertEquals("<p>Account ID         : 00000000010         </p>", html.get(35));
        assertEquals("<p>Current Balance    : 000001234.56-</p>", html.get(36));
        assertEquals("<p>FICO Score         : 700                 </p>", html.get(37));
        // 56 header lines (5100 + 5200), then 11 lines per transaction; the ID is the 3rd
        int firstTran = html.indexOf("<p>0000000000000001</p>");
        assertEquals(58, firstTran);
        assertEquals("<p>Purchase at Shop                                 </p>", html.get(firstTran + 3));
        assertEquals("<p>      183.88 </p>", html.get(firstTran + 6));
        assertEquals("<p>       47.88-</p>", html.get(firstTran + 17));
        assertEquals(List.of("<tr>",
                "<td colspan=\"3\" style=\"padding:0px 5px;background-color:#1d1d96b3;\">",
                "<h3>End of Statement</h3>", "</td>", "</tr>", "</table>", "</body>", "</html>"),
                html.subList(html.size() - 8, html.size()));
        assertEquals(56 + 2 * 11 + 8, html.size());
    }

    // 1000-MAINLINE: one statement per xref; 4000: cards without transactions get a zero total
    @Test
    void xrefWithoutTransactionsGetsEmptySummaryAndZeroTotal() {
        TestData.Run run = TestData.run(oneCardTrnx(),
                List.of(TestData.xref(CARD_A, 1, 10), TestData.xref(CARD_B, 1, 10)),
                List.of(TestData.customer(1)), List.of(TestData.account(10, "0")));
        assertNull(run.abend());
        assertEquals(2 * 19 + 2, run.stmt().size());
        List<String> second = run.stmt().subList(21, 40).stream().map(TestData::rtrim).toList();
        assertEquals("Current Balance    :000000000.00", second.get(9));
        assertEquals("-".repeat(80), second.get(15));
        assertEquals("-".repeat(80), second.get(16));
        assertEquals("Total EXP:                                                        $         .00", second.get(17));
    }

    // 8500-READTRNX-READ grouping (card break) + 4000 matching by card with early exit
    @Test
    void transactionsAreGroupedPerCardAndTotalsResetPerStatement() {
        List<String> trnx = List.of(
                TestData.trnx(CARD_A, "A000000000000001", "a1", "1.10"),
                TestData.trnx(CARD_C, "C000000000000001", "c1", "3.00"),
                TestData.trnx(CARD_A, "A000000000000002", "a2", "2.20"),
                TestData.trnx(CARD_C, "C000000000000002", "c2", "-10.00"));
        TestData.Run run = TestData.run(trnx,
                List.of(TestData.xref(CARD_A, 1, 10), TestData.xref(CARD_B, 1, 10), TestData.xref(CARD_C, 1, 10)),
                List.of(TestData.customer(1)), List.of(TestData.account(10, "5")));
        assertNull(run.abend());
        List<String> lines = run.stmt().stream().map(TestData::rtrim).toList();
        List<String> totals = lines.stream().filter(l -> l.startsWith("Total EXP:")).toList();
        assertEquals(List.of(
                "Total EXP:                                                        $        3.30",
                "Total EXP:                                                        $         .00",
                "Total EXP:                                                        $        7.00-"), totals);
        List<String> tranIds = lines.stream().filter(l -> l.contains("$") && !l.startsWith("Total"))
                .map(l -> l.substring(0, 16)).toList();
        assertEquals(List.of("A000000000000001", "A000000000000002", "C000000000000001", "C000000000000002"),
                tranIds);
    }

    // 5000 STRING ... DELIMITED BY ' ': only the first word of each name/address part is kept
    @Test
    void nameAndAddressStringStopAtFirstSpace() {
        String cust = TestData.customer(1, "Mary Ann", "", "Van Dyke", "  leading blanks", "x",
                "New York", "NY", "USA", "10001 1234", 5);
        TestData.Run run = TestData.run(oneCardTrnx(), List.of(TestData.xref(CARD_A, 1, 10)), List.of(cust),
                List.of(TestData.account(10, "1")));
        List<String> stmt = run.stmt().stream().map(TestData::rtrim).toList();
        // empty middle name contributes nothing but its ' ' separator is still appended
        assertEquals("Mary  Van", stmt.get(1));
        assertEquals("  leading blanks", stmt.get(2));
        assertEquals("New NY USA 10001", stmt.get(4));
        assertEquals("FICO Score         :005", stmt.get(10));
        List<String> html = run.html().stream().map(TestData::rtrim).toList();
        // 5200 L23-NAME DELIMITED BY '  ' stops at the double space; ST-ADD1 starting with '  ' is empty
        assertEquals("<p style=\"font-size:16px\">Mary  </p>", html.get(22));
        assertEquals("<p>  </p>", html.get(23));
    }

    // ADD TRNX-AMT TO WS-TOTAL-AMT (S9(9)V99 COMP-3, no ON SIZE ERROR) truncates high-order digits
    @Test
    void totalAmountOverflowTruncatesHighOrderDigit() {
        List<String> trnx = List.of(TestData.trnx(CARD_A, "0000000000000001", "big", "999999999.99"),
                TestData.trnx(CARD_A, "0000000000000002", "big", "0.02"));
        TestData.Run run = TestData.run(trnx, List.of(TestData.xref(CARD_A, 1, 10)),
                List.of(TestData.customer(1)), List.of(TestData.account(10, "1")));
        assertEquals("Total EXP:                                                        $         .01",
                TestData.rtrim(run.stmt().get(19)));
    }

    // PROCEDURE DIVISION prologue DISPLAYs (TIOT walk)
    @Test
    void prologueDisplaysJobStepAndDdNames() {
        List<String> display = new ArrayList<>();
        JobContext job = new JobContext("CREASTMT", "STEP040",
                List.of(new JobContext.DdEntry("STMTFILE", true), new JobContext.DdEntry("SYSOUT", false)));
        new Cbstm03a(job, Cbstm03bFiles.inMemory(oneCardTrnx(), List.of(TestData.xref(CARD_A, 1, 10)),
                List.of(TestData.customer(1)), List.of(TestData.account(10, "1"))),
                new InMemoryRecordWriter(80), new InMemoryRecordWriter(100), display::add).run();
        assertEquals(List.of("Running JCL : CREASTMT Step STEP040 ", "DD Names from TIOT: ",
                ": STMTFILE -- valid UCB", ": SYSOUT   --  null UCB", ": " + "\0".repeat(8) + " -- null  UCB"),
                display);
    }

    // 1000-MAINLINE end: CLOSE STMT-FILE HTML-FILE; 9100..9400 close all inputs
    @Test
    void normalEndClosesOutputs() {
        TestData.Run run = oneCardRun();
        assertTrue(run.stmtWriter().isClosed());
        assertTrue(run.htmlWriter().isClosed());
    }

    // 8100-TRNXFILE-OPEN: priming READ must return '00'/'04'; an empty TRNXFILE abends
    @Test
    void emptyTransactionFileAbendsOnPrimingRead() {
        TestData.Run run = TestData.run(List.of(), List.of(TestData.xref(CARD_A, 1, 10)),
                List.of(TestData.customer(1)), List.of(TestData.account(10, "1")));
        assertNotNull(run.abend());
        assertEquals("ERROR READING TRNXFILE", run.abend().reason());
        assertEquals("10", run.abend().returnCode());
        assertEquals(List.of("ERROR READING TRNXFILE", "RETURN CODE: 10", "ABENDING PROGRAM"),
                run.display().subList(run.display().size() - 3, run.display().size()));
        assertTrue(run.stmt().isEmpty());
        assertTrue(!run.stmtWriter().isClosed());
    }

    // 2000-CUSTFILE-GET / 3000-ACCTFILE-GET: any status other than '00' abends
    @Test
    void missingCustomerAbends() {
        TestData.Run run = TestData.run(oneCardTrnx(), List.of(TestData.xref(CARD_A, 2, 10)),
                List.of(TestData.customer(1)), List.of(TestData.account(10, "1")));
        assertEquals("ERROR READING CUSTFILE", run.abend().reason());
        assertEquals("23", run.abend().returnCode());
        assertTrue(run.stmt().isEmpty());
    }

    @Test
    void missingAccountAbendsAfterEarlierStatementsWereWritten() {
        TestData.Run run = TestData.run(oneCardTrnx(),
                List.of(TestData.xref(CARD_A, 1, 10), TestData.xref(CARD_B, 1, 11)),
                List.of(TestData.customer(1)), List.of(TestData.account(10, "1")));
        assertEquals("ERROR READING ACCTFILE", run.abend().reason());
        assertEquals("23", run.abend().returnCode());
        assertEquals(21, run.stmt().size());
    }

    // 8100..8400 OPEN, 8100/8500 READ, 1000 xref READ, 9100..9400 CLOSE error branches
    @ParameterizedTest
    @CsvSource({
        "TRNXFILE, OPEN,  35, ERROR OPENING TRNXFILE",
        "TRNXFILE, READ,  30, ERROR READING TRNXFILE",
        "XREFFILE, OPEN,  35, ERROR OPENING XREFFILE",
        "XREFFILE, READ,  23, ERROR READING XREFFILE",
        "CUSTFILE, OPEN,  37, ERROR OPENING CUSTFILE",
        "CUSTFILE, READ_K,92, ERROR READING CUSTFILE",
        "ACCTFILE, OPEN,  93, ERROR OPENING ACCTFILE",
        "ACCTFILE, READ_K,92, ERROR READING ACCTFILE",
        "TRNXFILE, CLOSE, 42, ERROR CLOSING TRNXFILE",
        "XREFFILE, CLOSE, 42, ERROR CLOSING XREFFILE",
        "CUSTFILE, CLOSE, 42, ERROR CLOSING CUSTFILE",
        "ACCTFILE, CLOSE, 42, ERROR CLOSING ACCTFILE"
    })
    void fileErrorsAbendWithExactMessages(String dd, M03bArea.Oper oper, String rc, String message) {
        Cbstm03bCall files = forcing(Cbstm03bFiles.inMemory(oneCardTrnx(), List.of(TestData.xref(CARD_A, 1, 10)),
                List.of(TestData.customer(1)), List.of(TestData.account(10, "1"))), dd, oper, rc);
        TestData.Run run = TestData.run(files);
        assertNotNull(run.abend());
        assertEquals(message, run.abend().reason());
        assertEquals(rc, run.abend().returnCode());
        assertEquals(message + " (RETURN CODE: " + rc + ")", run.abend().getMessage());
        assertEquals(List.of(message, "RETURN CODE: " + rc, "ABENDING PROGRAM"),
                run.display().subList(run.display().size() - 3, run.display().size()));
    }

    // IF WS-M03B-RC = '00' OR '04': status 04 on OPEN/CLOSE is accepted
    @ParameterizedTest
    @CsvSource({"TRNXFILE, OPEN", "XREFFILE, OPEN", "CUSTFILE, OPEN", "ACCTFILE, OPEN",
        "TRNXFILE, CLOSE", "XREFFILE, CLOSE", "CUSTFILE, CLOSE", "ACCTFILE, CLOSE"})
    void status04IsTreatedAsSuccess(String dd, M03bArea.Oper oper) {
        Cbstm03bCall files = forcing(Cbstm03bFiles.inMemory(oneCardTrnx(), List.of(TestData.xref(CARD_A, 1, 10)),
                List.of(TestData.customer(1)), List.of(TestData.account(10, "1"))), dd, oper, "04");
        TestData.Run run = TestData.run(files);
        assertNull(run.abend());
        assertEquals(21, run.stmt().size());
    }

    // 8500-READTRNX-READ: a status other than 00/10 on a follow-on read abends
    @Test
    void followOnTransactionReadErrorAbends() {
        Cbstm03bCall base = Cbstm03bFiles.inMemory(oneCardTrnx(), List.of(TestData.xref(CARD_A, 1, 10)),
                List.of(TestData.customer(1)), List.of(TestData.account(10, "1")));
        int[] reads = {0};
        Cbstm03bCall files = area -> {
            base.call(area);
            if (area.dd().trim().equals("TRNXFILE") && area.oper() == M03bArea.Oper.READ && ++reads[0] == 2) {
                area.rc("34");
            }
        };
        TestData.Run run = TestData.run(files);
        assertEquals("ERROR READING TRNXFILE", run.abend().reason());
        assertEquals("34", run.abend().returnCode());
    }

    // WS-TRAN-TBL OCCURS 10 / WS-CARD-TBL OCCURS 51: overflow is rejected (COBOL would overlay storage)
    @Test
    void transactionTableOverflowIsRejected() {
        List<String> trnx = new ArrayList<>();
        for (int i = 1; i <= Cbstm03a.MAX_TRANS_PER_CARD + 1; i++) {
            trnx.add(TestData.trnx(CARD_A, String.format("%016d", i), "t", "1"));
        }
        assertThrows(IllegalStateException.class, () -> TestData.run(trnx, List.of(TestData.xref(CARD_A, 1, 10)),
                List.of(TestData.customer(1)), List.of(TestData.account(10, "1"))));
    }
}
