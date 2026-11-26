package com.carddemo.programs.cbact01c;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class Cbact01cMainTest {
    @TempDir
    Path dir;

    private final ByteArrayOutputStream captured = new ByteArrayOutputStream();
    private final PrintStream sysout = new PrintStream(captured, true, StandardCharsets.ISO_8859_1);

    @Test
    void readacctStepWritesThreeDatasetsFromDdEnvironment() throws Exception {
        Map<String, String> env = Map.of(
                "ACCTFILE", "src/test/resources/fixtures/acctdata.txt",
                "OUTFILE", dir.resolve("pscomp").toString(),
                "ARRYFILE", dir.resolve("arryps").toString(),
                "VBRCFILE", dir.resolve("vbps").toString());
        assertEquals(0, Cbact01cMain.execute(new String[0], env, sysout));
        assertEquals(50 * 107, Files.size(dir.resolve("pscomp")));
        assertEquals(50 * 110, Files.size(dir.resolve("arryps")));
        byte[] vb = Files.readAllBytes(dir.resolve("vbps"));
        assertEquals(50 * (16 + 43), vb.length);
        assertEquals(16, vb[1]);
        assertEquals(43, vb[17]);
        assertTrue(captured.toString(StandardCharsets.ISO_8859_1).endsWith("END OF EXECUTION OF PROGRAM CBACT01C"
                + System.lineSeparator()));
    }

    @Test
    void abendReturns999AndDeletesNewDatasets() {
        String[] args = {dir.resolve("missing").toString(), dir.resolve("pscomp").toString(),
                dir.resolve("arryps").toString(), dir.resolve("vbps").toString()};
        assertEquals(999, Cbact01cMain.execute(args, Map.of(), sysout));
        assertFalse(Files.exists(dir.resolve("pscomp")));
        String out = captured.toString(StandardCharsets.ISO_8859_1);
        assertTrue(out.contains("ERROR OPENING ACCTFILE"));
        assertTrue(out.contains("FILE STATUS IS: NNNN0035"));
    }

    @Test
    void missingDdIsReported() {
        assertThrows(IllegalArgumentException.class, () -> Cbact01cMain.execute(new String[0], Map.of(), sysout));
    }

    @Test
    void existingOutputIsNeitherReplacedNorDeletedOnAbend() throws Exception {
        Path existing = dir.resolve("pscomp");
        Files.writeString(existing, "keep");
        String[] args = {"src/test/resources/fixtures/acctdata.txt", existing.toString(),
                dir.resolve("arryps").toString(), dir.resolve("vbps").toString()};
        assertEquals(999, Cbact01cMain.execute(args, Map.of(), sysout));
        assertEquals("keep", Files.readString(existing));
        assertTrue(captured.toString(StandardCharsets.ISO_8859_1).contains("ERROR OPENING OUTFILE30"));
    }

    @Test
    void invalidNumericDataDeletesCreatedOutputs() throws Exception {
        String line = Files.readAllLines(Path.of("src/test/resources/fixtures/acctdata.txt"),
                StandardCharsets.ISO_8859_1).get(0);
        char[] chars = line.toCharArray();
        chars[90] = 'X';
        Path acct = dir.resolve("acct");
        Files.writeString(acct, new String(chars) + "\n", StandardCharsets.ISO_8859_1);
        String[] args = {acct.toString(), dir.resolve("pscomp").toString(), dir.resolve("arryps").toString(),
                dir.resolve("vbps").toString()};
        assertThrows(com.carddemo.programs.cbact01c.support.DataException.class,
                () -> Cbact01cMain.execute(args, Map.of(), sysout));
        assertFalse(Files.exists(dir.resolve("pscomp")));
        assertFalse(Files.exists(dir.resolve("arryps")));
        assertFalse(Files.exists(dir.resolve("vbps")));
    }

    @Test
    void laterCollisionKeepsExistingDatasetAndDeletesOutputsCreatedByTheFailedRun() throws Exception {
        Path existing = dir.resolve("arryps");
        Files.writeString(existing, "keep");
        String[] args = {"src/test/resources/fixtures/acctdata.txt", dir.resolve("pscomp").toString(),
                existing.toString(), dir.resolve("vbps").toString()};
        assertEquals(999, Cbact01cMain.execute(args, Map.of(), sysout));
        assertEquals("keep", Files.readString(existing));
        assertFalse(Files.exists(dir.resolve("pscomp")));
        assertFalse(Files.exists(dir.resolve("vbps")));
        assertTrue(captured.toString(StandardCharsets.ISO_8859_1).contains("ERROR OPENING ARRAYFILE30"));
    }
}
