package com.carddemo.programs.cbexport;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class CbexportMainTest {

    @TempDir
    Path dir;

    private String[] args(Path cust) {
        return new String[] {
            "CUSTFILE=" + cust,
            "ACCTFILE=" + dir.resolve("acct"),
            "XREFFILE=" + dir.resolve("xref"),
            "TRANSACT=" + dir.resolve("tran"),
            "CARDFILE=" + dir.resolve("card"),
            "EXPFILE=" + dir.resolve("exp.dat"),
            "--input-format=fixed"
        };
    }

    @Test
    void missingInputFileAbendsWithStatus35AndReturnCode16() {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        int rc = CbexportMain.run(args(dir.resolve("missing")), CbexportTest.CLOCK,
                new PrintStream(out, true, StandardCharsets.UTF_8), System.err);
        assertEquals(CbexportMain.RC_ABEND, rc);
        String s = out.toString(StandardCharsets.UTF_8);
        assertTrue(s.contains("ERROR: Cannot open CUSTOMER-INPUT, Status: 35"));
        assertTrue(s.contains("CBEXPORT: ABENDING PROGRAM"));
    }

    @Test
    void emptyFixedInputsProduceEmptyExport() throws Exception {
        for (String f : new String[] {"cust", "acct", "xref", "tran", "card"}) {
            Files.write(dir.resolve(f), new byte[0]);
        }
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        int rc = CbexportMain.run(args(dir.resolve("cust")), CbexportTest.CLOCK,
                new PrintStream(out, true, StandardCharsets.UTF_8), System.err);
        assertEquals(CbexportMain.RC_OK, rc);
        assertEquals(0, Files.size(dir.resolve("exp.dat")));
    }

    @Test
    void invalidInvocationIsRejected() {
        ByteArrayOutputStream err = new ByteArrayOutputStream();
        PrintStream e = new PrintStream(err, true, StandardCharsets.UTF_8);
        assertEquals(CbexportMain.RC_ABEND, CbexportMain.run(new String[] {"BOGUS=x"}, CbexportTest.CLOCK, System.out, e));
        assertEquals(CbexportMain.RC_ABEND, CbexportMain.run(new String[] {"CUSTFILE=x"}, CbexportTest.CLOCK, System.out, e));
        String s = err.toString(StandardCharsets.UTF_8);
        assertTrue(s.contains("Unrecognised argument: BOGUS=x"));
        assertTrue(s.contains("Missing DD ACCTFILE=<path>"));
    }
}
