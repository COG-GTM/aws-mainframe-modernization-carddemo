package com.carddemo.programs.cbtrn03c;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class Cbtrn03cMainTest {

    @TempDir
    Path dir;

    @Test
    void runsTheStepFromDdAllocationsAndWritesTheReportDataSet() throws Exception {
        Map<String, String> dd = allocate(RecordFormat.LINE);
        assertEquals(0, Cbtrn03cMain.execute(dd::get, quietSysout()));
        List<String> expected = Files.readAllLines(Fixtures.path("tranrept-gnucobol-full.txt"), StandardCharsets.ISO_8859_1);
        assertEquals(expected, Files.readAllLines(Path.of(dd.get("TRANREPT")), StandardCharsets.ISO_8859_1));
    }

    @Test
    void supportsUndelimitedRecfmFbDataSets() throws Exception {
        Map<String, String> dd = allocate(RecordFormat.FIXED);
        assertEquals(0, Cbtrn03cMain.execute(dd::get, quietSysout()));
        String report = Files.readString(Path.of(dd.get("TRANREPT")), StandardCharsets.ISO_8859_1);
        assertEquals(501 * 133, report.length());
    }

    @Test
    void abendReturnsCode999AndDeletesTheReportDataSet() throws Exception {
        Map<String, String> dd = allocate(RecordFormat.LINE);
        dd.put("CARDXREF", dir.resolve("missing.dat").toString());
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        assertEquals(999, Cbtrn03cMain.execute(dd::get, new PrintStream(out, true, StandardCharsets.ISO_8859_1)));
        assertFalse(Files.exists(Path.of(dd.get("TRANREPT"))));
        assertTrue(out.toString(StandardCharsets.ISO_8859_1).contains(
                "ERROR OPENING CROSS REF FILE\nFILE STATUS IS: NNNN0035\nABENDING PROGRAM"));
    }

    @Test
    void missingDdAllocationIsRejected() {
        Map<String, String> dd = new HashMap<>();
        assertThrows(IllegalArgumentException.class, () -> Cbtrn03cMain.execute(dd::get, quietSysout()));
    }

    private enum RecordFormat { LINE, FIXED }

    private Map<String, String> allocate(RecordFormat format) throws Exception {
        List<String> tran = Fixtures.dailyTransactionsForReport("2022-01-01", "2022-07-06");
        Map<String, String> dd = new HashMap<>();
        dd.put("TRANFILE", write("tranfile.dat", tran, format));
        dd.put("CARDXREF", write("cardxref.dat", Fixtures.lines("cardxref.txt", 50), format));
        dd.put("TRANTYPE", write("trantype.dat", Fixtures.lines("trantype.txt", 60), format));
        dd.put("TRANCATG", write("trancatg.dat", Fixtures.lines("trancatg.txt", 60), format));
        dd.put("DATEPARM", write("dateparm.dat", Fixtures.lines("dateparm.txt", 80), format));
        dd.put("TRANREPT", dir.resolve("tranrept.dat").toString());
        dd.put("RECFM", format.name());
        return dd;
    }

    private String write(String name, List<String> records, RecordFormat format) throws Exception {
        Path p = dir.resolve(name);
        String content = format == RecordFormat.FIXED ? String.join("", records) : String.join("\n", records) + "\n";
        Files.writeString(p, content, StandardCharsets.ISO_8859_1);
        return p.toString();
    }

    private static PrintStream quietSysout() {
        return new PrintStream(new ByteArrayOutputStream(), true, StandardCharsets.ISO_8859_1);
    }
}
