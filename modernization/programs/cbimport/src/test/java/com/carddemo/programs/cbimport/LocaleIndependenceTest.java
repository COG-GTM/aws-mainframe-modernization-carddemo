package com.carddemo.programs.cbimport;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Locale;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class LocaleIndependenceTest {

    private final Locale saved = Locale.getDefault();

    @TempDir
    Path dir;

    @AfterEach
    void restore() {
        Locale.setDefault(saved);
    }

    @Test
    void currentDateUsesAsciiDigitsUnderArabicLocale() {
        Locale.setDefault(Locale.forLanguageTag("ar-EG-u-nu-arab"));
        assertEquals("2026112617223345+1100", CurrentDate.of(CbimportTest.CLOCK));
    }

    @Test
    void encodingOptionsAreRecognisedUnderTurkishLocale() throws Exception {
        Locale.setDefault(Locale.forLanguageTag("tr-TR"));
        PrintStream out = new PrintStream(new ByteArrayOutputStream(), true, StandardCharsets.UTF_8);
        String[] args = {"--expfile=" + CbimportFixtureTest.EXPORT, "--custout=" + dir.resolve("c"),
                "--acctout=" + dir.resolve("a"), "--xrefout=" + dir.resolve("x"), "--trnxout=" + dir.resolve("t"),
                "--cardout=" + dir.resolve("d"), "--errout=" + dir.resolve("e"),
                "--output-encoding=US-ASCII", "--record-separator=lf"};
        assertEquals(0, CbimportMain.run(args, CbimportTest.CLOCK, out));
        var lines = Files.readAllLines(dir.resolve("a"), StandardCharsets.US_ASCII);
        assertEquals(50, lines.size());
        assertTrue(lines.get(0).startsWith("00000000001Y00000000000{"));
    }
}
