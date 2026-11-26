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
}
