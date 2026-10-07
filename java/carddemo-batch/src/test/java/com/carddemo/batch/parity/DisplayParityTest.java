package com.carddemo.batch.parity;

import com.carddemo.batch.support.Cbact01cRun;
import com.carddemo.batch.support.Repo;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/** The DISPLAY output (stdout) against golden-files/CBACT01C/display.txt. */
class DisplayParityTest {

    static void assertSameLines(String what, Path goldenFile, String actual) throws IOException {
        List<String> expected = Files.readAllLines(goldenFile, StandardCharsets.ISO_8859_1);
        List<String> lines = List.of(actual.split("\n", -1));
        if (!lines.isEmpty() && lines.get(lines.size() - 1).isEmpty()) {
            lines = lines.subList(0, lines.size() - 1);
        }
        int n = Math.min(expected.size(), lines.size());
        for (int i = 0; i < n; i++) {
            assertEquals(expected.get(i), lines.get(i), what + " line " + (i + 1));
        }
        assertEquals(expected.size(), lines.size(), what + " line count");
        assertEquals(Files.readString(goldenFile, StandardCharsets.ISO_8859_1), actual, what + " exact text");
    }

    @Test
    void displayOutputMatchesGoldenLineByLine() throws IOException {
        Cbact01cRun run = Cbact01cRun.sample();
        assertSameLines("display.txt", Repo.GOLDEN_CBACT01C.resolve("display.txt"), run.display);
    }

    @Test
    void displayHasStartEndAndOneBlockPerAccount() {
        String display = Cbact01cRun.sample().display;
        assertTrue(display.startsWith("START OF EXECUTION OF PROGRAM CBACT01C\n"), "first DISPLAY line");
        assertTrue(display.endsWith("END OF EXECUTION OF PROGRAM CBACT01C\n"), "last DISPLAY line");
        assertEquals(50, display.split("ACCT-ID                 :", -1).length - 1, "ACCT-ID lines");
        assertEquals(50, display.split("VBRC-REC1:", -1).length - 1, "VBRC-REC1 lines");
        assertEquals(50, display.split("VBRC-REC2:", -1).length - 1, "VBRC-REC2 lines");
        assertTrue(display.contains("ACCT-CURR-BAL           :000000019400+\n"), "zoned DISPLAY with trailing sign");
        assertEquals(50 * 15 + 2, display.split("\n", -1).length - 1,
                "total DISPLAY lines (11 fields + separator + 2 VBRC + ACCOUNT-RECORD per account, plus start/end)");
    }
}
