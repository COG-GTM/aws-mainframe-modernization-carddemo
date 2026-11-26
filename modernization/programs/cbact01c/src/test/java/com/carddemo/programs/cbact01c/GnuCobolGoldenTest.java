package com.carddemo.programs.cbact01c;

import com.carddemo.programs.cbact01c.copybook.AccountRecord;
import com.carddemo.programs.cbact01c.gateway.CobdatftDateFormatter;
import com.carddemo.programs.cbact01c.io.FixedRecordFileWriter;
import com.carddemo.programs.cbact01c.io.KsdsFileReader;
import com.carddemo.programs.cbact01c.io.VariableRecordFileWriter;
import com.carddemo.programs.cbact01c.record.ArrArrayRec;
import com.carddemo.programs.cbact01c.record.OutAcctRec;
import com.carddemo.programs.cbact01c.support.ZonedDecimal;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Runs the port over the 50 sample accounts in app/data/ASCII/acctdata.txt and compares every output with the
 * outputs of the original CBACT01C compiled by GnuCOBOL 3.1.2 (see src/test/golden-harness/README.md).
 * Two documented differences between GnuCOBOL and IBM Enterprise COBOL are normalised; everything else must
 * match byte for byte.
 */
class GnuCobolGoldenTest {
    private static final Path FIXTURE = Path.of("src/test/resources/fixtures/acctdata.txt");
    private static final Path GOLDEN = Path.of("src/test/resources/golden");
    private static final Set<String> NUMERIC_DISPLAY_LABELS = Set.of("ACCT-CURR-BAL           :",
            "ACCT-CREDIT-LIMIT       :", "ACCT-CASH-CREDIT-LIMIT  :", "ACCT-CURR-CYC-CREDIT    :",
            "ACCT-CURR-CYC-DEBIT     :");

    @TempDir
    Path dir;
    private final List<String> sysout = new ArrayList<>();

    @BeforeEach
    void runPort() {
        Cbact01c program = new Cbact01c(
                new KsdsFileReader(FIXTURE, AccountRecord.LENGTH, AccountRecord.ACCT_ID.length()),
                new FixedRecordFileWriter(dir.resolve("out.dat"), OutAcctRec.LENGTH),
                new FixedRecordFileWriter(dir.resolve("arry.dat"), ArrArrayRec.LENGTH),
                new VariableRecordFileWriter(dir.resolve("vbrc.dat"), 10, 80,
                        VariableRecordFileWriter.HeaderFormat.GNUCOBOL_VARSEQ_0),
                new CobdatftDateFormatter(),
                sysout::add);
        assertEquals(0, program.run());
    }

    @Test
    void outfileMatchesByteForByte() throws Exception {
        byte[] actual = Files.readAllBytes(dir.resolve("out.dat"));
        assertEquals(50 * 107, actual.length);
        assertArrayEquals(Files.readAllBytes(GOLDEN.resolve("out.dat")), actual);
    }

    @Test
    void vbrcfileMatchesByteForByte() throws Exception {
        byte[] actual = Files.readAllBytes(dir.resolve("vbrc.dat"));
        assertEquals(50 * (4 + 12 + 4 + 39), actual.length);
        assertArrayEquals(Files.readAllBytes(GOLDEN.resolve("vbrc.dat")), actual);
    }

    /**
     * GnuCOBOL's INITIALIZE writes an unsigned zero ('0') in the sign position of an S9 DISPLAY item, IBM
     * writes the preferred positive sign ('{'). Only ARR-ACCT-CURR-BAL(4) and (5) are affected.
     */
    @Test
    void arryfileMatchesExceptInitializeSignOfUnusedOccurrences() throws Exception {
        byte[] actual = Files.readAllBytes(dir.resolve("arry.dat"));
        byte[] golden = Files.readAllBytes(GOLDEN.resolve("arry.dat"));
        assertEquals(golden.length, actual.length);
        for (int rec = 0; rec < actual.length / ArrArrayRec.LENGTH; rec++) {
            for (int occ = 4; occ <= 5; occ++) {
                int signPos = rec * ArrArrayRec.LENGTH + ArrArrayRec.arrAcctCurrBal(occ).end() - 1;
                assertEquals('{', actual[signPos]);
                assertEquals('0', golden[signPos]);
                actual[signPos] = '0';
            }
        }
        assertArrayEquals(golden, actual);
    }

    /**
     * IBM DISPLAY of an S9(10)V99 DISPLAY item prints its 12 raw bytes (trailing overpunch); GnuCOBOL prints
     * the digits followed by a separate sign. Those five lines are converted before comparing.
     */
    @Test
    void sysoutMatchesExceptNumericDisplayFormat() throws Exception {
        List<String> golden = Files.readAllLines(GOLDEN.resolve("sysout.txt"), StandardCharsets.ISO_8859_1);
        List<String> normalised = sysout.stream().map(GnuCobolGoldenTest::toGnuCobolDisplay).toList();
        assertEquals(golden.size(), normalised.size());
        assertEquals(golden, normalised);
    }

    private static String toGnuCobolDisplay(String line) {
        for (String label : NUMERIC_DISPLAY_LABELS) {
            if (line.startsWith(label)) {
                BigDecimal v = ZonedDecimal.decode(line.substring(label.length()), 2);
                String digits = String.format("%012d", v.unscaledValue().abs());
                return label + digits + (v.signum() < 0 ? "-" : "+");
            }
        }
        return line;
    }
}
