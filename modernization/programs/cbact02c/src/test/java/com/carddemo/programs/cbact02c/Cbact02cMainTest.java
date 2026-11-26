package com.carddemo.programs.cbact02c;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class Cbact02cMainTest {

    private final ByteArrayOutputStream buffer = new ByteArrayOutputStream();
    private final PrintStream out = new PrintStream(buffer, true, StandardCharsets.UTF_8);

    private String output() {
        return buffer.toString(StandardCharsets.UTF_8);
    }

    @Test
    void returnsZeroForSampleFile() {
        int rc = Cbact02cMain.execute(new String[] {Cbact02cFixtureTest.CARDDATA.toString()}, null, out);

        assertEquals(0, rc);
        assertTrue(output().endsWith("END OF EXECUTION OF PROGRAM CBACT02C" + System.lineSeparator()));
    }

    @Test
    void usesCardfileEnvironmentWhenNoArgument() {
        int rc = Cbact02cMain.execute(new String[0], Cbact02cFixtureTest.CARDDATA.toString(), out);

        assertEquals(0, rc);
    }

    @Test
    void returnsAbendCodeWhenCardfileMissing(@TempDir Path dir) {
        int rc = Cbact02cMain.execute(new String[] {dir.resolve("missing.txt").toString()}, null, out);

        assertEquals(999, rc);
        assertTrue(output().contains("ERROR OPENING CARDFILE"));
        assertTrue(output().contains("FILE STATUS IS: NNNN0035"));
        assertTrue(output().contains("ABENDING PROGRAM"));
    }

    @Test
    void readsEbcdicFixedBlockDatasetWithRecfmOption() {
        int rc = Cbact02cMain.execute(
                new String[] {"--recfm=FB", Cbact02cFixtureTest.CARDDATA_EBCDIC.toString()}, null, out);

        assertEquals(0, rc);
        assertEquals(52, output().split(System.lineSeparator()).length);
    }

    @Test
    void returnsSysoutWriteErrorWhenOutputFails() {
        PrintStream failing = new PrintStream(new java.io.OutputStream() {
            @Override
            public void write(int b) throws java.io.IOException {
                throw new java.io.IOException("disk full");
            }
        }, true);

        int rc = Cbact02cMain.execute(new String[] {Cbact02cFixtureTest.CARDDATA.toString()}, null, failing);

        assertEquals(Cbact02cMain.SYSOUT_WRITE_ERROR, rc);
    }

    @Test
    void returnsUsageErrorWithoutCardfile() {
        assertEquals(Cbact02cMain.USAGE_ERROR, Cbact02cMain.execute(new String[0], null, out));
    }
}
