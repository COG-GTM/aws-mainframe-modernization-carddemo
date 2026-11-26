package com.carddemo.programs.cbtrn02c;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Runs POSTTRAN over the CardDemo sample data (app/data/ASCII) through the CLI and compares every output
 * with the golden run of the original COBOL compiled by GnuCOBOL (golden-harness/run-golden.sh).
 */
class Cbtrn02cGoldenFixtureTest {

    static final Path DATA = Path.of("../../../app/data/ASCII");
    static final Path GOLDEN = Path.of("src/test/resources/golden");
    static final int PROC_TS_FROM = 304;
    static final int PROC_TS_TO = 330;

    @TempDir
    static Path work;
    static int rc;
    static List<String> sysout;

    @BeforeAll
    static void runPosttran() throws Exception {
        Files.copy(DATA.resolve("acctdata.txt"), work.resolve("acctdata.txt"));
        Files.copy(DATA.resolve("tcatbal.txt"), work.resolve("tcatbal.txt"));
        PrintStream original = System.out;
        ByteArrayOutputStream buf = new ByteArrayOutputStream();
        System.setOut(new PrintStream(buf, true, StandardCharsets.UTF_8));
        try {
            rc = Cbtrn02cMain.execute(new String[] {
                    "DALYTRAN=" + DATA.resolve("dailytran.txt"),
                    "TRANFILE=" + work.resolve("transact.txt"),
                    "XREFFILE=" + DATA.resolve("cardxref.txt"),
                    "DALYREJS=" + work.resolve("dalyrejs.txt"),
                    "ACCTFILE=" + work.resolve("acctdata.txt"),
                    "TCATBALF=" + work.resolve("tcatbal.txt")});
        } finally {
            System.setOut(original);
        }
        sysout = buf.toString(StandardCharsets.UTF_8).lines().toList();
    }

    static List<String> lines(Path p) throws Exception {
        return Files.readAllLines(p, StandardCharsets.ISO_8859_1).stream().map(String::stripTrailing)
                .filter(s -> !s.isEmpty()).toList();
    }

    @Test
    void returnCodeMatchesCobol() throws Exception {
        assertEquals(Integer.parseInt(Files.readString(GOLDEN.resolve("return-code.txt")).trim()), rc);
        assertEquals(4, rc);
    }

    @Test
    void sysoutMatchesCobolLineForLine() throws Exception {
        assertEquals(lines(GOLDEN.resolve("sysout.txt")), sysout.stream().map(String::stripTrailing).toList());
        assertTrue(sysout.contains("TRANSACTIONS PROCESSED :000000300"));
        assertTrue(sysout.contains("TRANSACTIONS REJECTED  :000000038"));
    }

    @Test
    void rejectsFileMatchesCobol() throws Exception {
        List<String> actual = Files.readAllLines(work.resolve("dalyrejs.txt"), StandardCharsets.ISO_8859_1);
        assertEquals(lines(GOLDEN.resolve("dalyrejs.txt")), actual.stream().map(String::stripTrailing).toList());
        assertTrue(actual.stream().allMatch(r -> r.length() == 430));
    }

    @Test
    void accountMasterMatchesCobol() throws Exception {
        assertEquals(lines(GOLDEN.resolve("acctdata.txt")), lines(work.resolve("acctdata.txt")));
    }

    @Test
    void categoryBalancesMatchCobol() throws Exception {
        assertEquals(lines(GOLDEN.resolve("tcatbal.txt")), lines(work.resolve("tcatbal.txt")));
    }

    @Test
    void transactionFileMatchesCobolExceptProcessingTimestamp() throws Exception {
        List<String> expected = lines(GOLDEN.resolve("transact.txt"));
        List<String> actual = lines(work.resolve("transact.txt"));
        assertEquals(262, actual.size());
        assertEquals(expected.size(), actual.size());
        for (int i = 0; i < expected.size(); i++) {
            String e = expected.get(i);
            String a = actual.get(i);
            assertEquals(e.substring(0, PROC_TS_FROM), a.substring(0, PROC_TS_FROM), "record " + i);
            assertEquals(e.substring(PROC_TS_TO), a.substring(PROC_TS_TO), "record " + i);
            assertTrue(a.substring(PROC_TS_FROM, PROC_TS_TO).matches("\\d{4}-\\d{2}-\\d{2}-\\d{2}\\.\\d{2}\\.\\d{2}\\.\\d{2}0000"));
        }
    }
}
