package com.carddemo.programs.csutldtc;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import org.junit.jupiter.api.Test;

class CsutldtcMainTest {
    @Test
    void exitCodeIsReturnCode() {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        PrintStream ps = new PrintStream(out, true, StandardCharsets.ISO_8859_1);
        assertEquals(0, CsutldtcMain.run(new String[] {"2022-06-10", "YYYY-MM-DD"}, ps, ps, new Csutldtc()));
        assertEquals(3, CsutldtcMain.run(new String[] {"2022-02-30", "YYYY-MM-DD"}, ps, ps, new Csutldtc()));
        String text = out.toString(StandardCharsets.ISO_8859_1);
        assertTrue(text.startsWith("0000Mesg Code: 0000 Date is valid"));
        assertTrue(text.contains("0003Mesg Code: 2508 Datevalue error"));
    }

    @Test
    void usageError() {
        ByteArrayOutputStream err = new ByteArrayOutputStream();
        PrintStream ps = new PrintStream(err, true, StandardCharsets.UTF_8);
        assertEquals(CsutldtcMain.USAGE_ERROR, CsutldtcMain.run(new String[] {"x"}, ps, ps, new Csutldtc()));
        assertTrue(err.toString(StandardCharsets.UTF_8).startsWith("usage:"));
    }
}
