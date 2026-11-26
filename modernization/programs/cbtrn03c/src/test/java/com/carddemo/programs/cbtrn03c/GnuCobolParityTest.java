package com.carddemo.programs.cbtrn03c;

import com.carddemo.programs.cbtrn03c.copybook.CardXrefRecord;
import com.carddemo.programs.cbtrn03c.copybook.TranCatRecord;
import com.carddemo.programs.cbtrn03c.copybook.TranRecord;
import com.carddemo.programs.cbtrn03c.copybook.TranTypeRecord;
import com.carddemo.programs.cbtrn03c.io.FixedWidthKeyedInputFile;
import com.carddemo.programs.cbtrn03c.io.InMemorySequentialInputFile;
import com.carddemo.programs.cbtrn03c.io.InMemorySequentialOutputFile;
import com.carddemo.programs.cbtrn03c.io.FixedWidthSequentialInputFile;
import com.carddemo.programs.cbtrn03c.io.RecordFormat;
import com.carddemo.programs.cbtrn03c.support.ZonedDecimal;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Fixture-based parity against the original CBTRN03C compiled with GnuCOBOL 3.1.2 (-std=ibm) over the
 * app/data/ASCII sample (src/test/gnucobol/regenerate.sh reproduces the reference output).
 */
class GnuCobolParityTest {

    @Test
    void sampleDataReportMatchesGnuCobolByteForByte() throws Exception {
        List<String> input = Fixtures.dailyTransactionsForReport("2022-01-01", "2022-07-06");
        Run run = run(input);

        List<String> expected = Files.readAllLines(Fixtures.path("tranrept-gnucobol-full.txt"), StandardCharsets.ISO_8859_1);
        assertEquals(501, expected.size());
        assertEquals(expected, run.report.records());

        BigDecimal sum = input.stream().map(r -> TranRecord.parse(r).tranAmt()).reduce(BigDecimal.ZERO, BigDecimal::add);
        BigDecimal last = TranRecord.parse(input.get(input.size() - 1)).tranAmt();
        assertEquals("Grand Total" + ".".repeat(86) + "+    105,567.20", expected.get(500).stripTrailing());
        assertEquals(new BigDecimal("105567.20"), sum.add(last));
    }

    @Test
    void sampleDataSysoutMatchesGnuCobol() throws Exception {
        Run run = run(Fixtures.dailyTransactionsForReport("2022-01-01", "2022-07-06"));
        List<String> expected = Files.readAllLines(Fixtures.path("sysout-gnucobol-full.txt"), StandardCharsets.ISO_8859_1);
        assertEquals(expected.size(), run.sysout.size());
        for (int i = 0; i < expected.size(); i++) {
            String e = expected.get(i);
            String a = run.sysout.get(i);
            if (e.startsWith("TRAN-AMT ") || e.startsWith("WS-PAGE-TOTAL")) {
                // GnuCOBOL DISPLAYs signed zoned items with a trailing separate sign; Enterprise COBOL (and
                // this port) shows the overpunched image. Compare the values.
                String prefix = e.startsWith("TRAN-AMT ") ? "TRAN-AMT " : "WS-PAGE-TOTAL";
                assertEquals(prefix, a.substring(0, prefix.length()));
                assertEquals(gnuDisplayValue(e.substring(prefix.length())),
                        ZonedDecimal.decode(a.substring(prefix.length()), 2));
            } else {
                assertEquals(e, a, "sysout line " + (i + 1));
            }
        }
    }

    @Test
    void outOfRangeRecordStopsTheReportLikeGnuCobol() throws Exception {
        List<String> input = new ArrayList<>(Fixtures.dailyTransactionsForReport("2022-01-01", "2022-07-06"));
        String tenth = input.get(9);
        input.set(9, tenth.substring(0, 304) + "2023-01-01" + tenth.substring(314));
        Run run = run(input);
        List<String> expected = Files.readAllLines(Fixtures.path("tranrept-gnucobol-out-of-range.txt"),
                StandardCharsets.ISO_8859_1);
        assertEquals(15, expected.size());
        assertEquals(expected, run.report.records());
    }

    private static BigDecimal gnuDisplayValue(String image) {
        BigDecimal v = new BigDecimal(image.substring(0, image.length() - 1)).movePointLeft(2);
        return image.endsWith("-") ? v.negate() : v;
    }

    private record Run(InMemorySequentialOutputFile report, List<String> sysout) {
    }

    private static Run run(List<String> transactions) {
        InMemorySequentialOutputFile report = new InMemorySequentialOutputFile(Cbtrn03c.TRANREPT_LRECL);
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        Cbtrn03c program = new Cbtrn03c(
                new InMemorySequentialInputFile(transactions),
                new FixedWidthKeyedInputFile(Fixtures.path("cardxref.txt"), CardXrefRecord.LENGTH, 0, 16, RecordFormat.LINE),
                new FixedWidthKeyedInputFile(Fixtures.path("trantype.txt"), TranTypeRecord.LENGTH, 0, 2, RecordFormat.LINE),
                new FixedWidthKeyedInputFile(Fixtures.path("trancatg.txt"), TranCatRecord.LENGTH, 0, 6, RecordFormat.LINE),
                report,
                new FixedWidthSequentialInputFile(Fixtures.path("dateparm.txt"), Cbtrn03c.DATEPARM_LRECL, RecordFormat.LINE),
                new PrintStream(out, true, StandardCharsets.ISO_8859_1));
        assertEquals(0, program.run());
        return new Run(report, out.toString(StandardCharsets.ISO_8859_1).lines().toList());
    }
}
