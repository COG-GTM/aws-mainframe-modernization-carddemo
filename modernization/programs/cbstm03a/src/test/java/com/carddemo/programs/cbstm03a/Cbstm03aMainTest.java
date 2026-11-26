package com.carddemo.programs.cbstm03a;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

class Cbstm03aMainTest {

    @TempDir
    Path tmp;

    private Map<String, String> env(Path trnx) {
        Map<String, String> env = new HashMap<>();
        env.put("TRNXFILE", trnx.toString());
        env.put("XREFFILE", TestData.ASCII.resolve("cardxref.txt").toString());
        env.put("CUSTFILE", TestData.ASCII.resolve("custdata.txt").toString());
        env.put("ACCTFILE", TestData.ASCII.resolve("acctdata.txt").toString());
        env.put("STMTFILE", tmp.resolve("stmt.txt").toString());
        env.put("HTMLFILE", tmp.resolve("html.txt").toString());
        return env;
    }

    @Test
    void batchRunOverFilesMatchesGoldenAndExitsZero() throws IOException {
        Path trnx = tmp.resolve("trnx.txt");
        Files.write(trnx, TestData.lines(TestData.ASCII.resolve("dailytran.txt")).stream()
                .map(TestData::reshapeDailyTran).toList(), StandardCharsets.ISO_8859_1);
        assertEquals(0, Cbstm03aMain.run(env(trnx)));
        assertEquals(TestData.golden("STMTFILE.txt"), TestData.lines(tmp.resolve("stmt.txt")));
        assertEquals(TestData.golden("HTMLFILE.txt"), TestData.lines(tmp.resolve("html.txt")));
    }

    @Test
    void abendMapsToExitCode16() throws IOException {
        Path trnx = tmp.resolve("empty.txt");
        Files.write(trnx, List.of());
        assertEquals(Cbstm03aMain.ABEND_EXIT_CODE, Cbstm03aMain.run(env(trnx)));
        // DISP=(NEW,CATLG,DELETE): outputs are removed on abnormal end
        assertFalse(Files.exists(tmp.resolve("stmt.txt")));
        assertFalse(Files.exists(tmp.resolve("html.txt")));
    }

    @Test
    void missingInputDatasetAbendsWithOpenError() {
        assertEquals(Cbstm03aMain.ABEND_EXIT_CODE, Cbstm03aMain.run(env(tmp.resolve("missing.txt"))));
    }

    @Test
    void missingDdIsRejected() {
        Map<String, String> env = env(tmp.resolve("x"));
        env.remove("HTMLFILE");
        assertEquals(Cbstm03aMain.ABEND_EXIT_CODE, Cbstm03aMain.run(env));
    }
}
