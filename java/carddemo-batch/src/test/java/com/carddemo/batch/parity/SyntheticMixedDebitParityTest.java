package com.carddemo.batch.parity;

import com.carddemo.batch.support.Cbact01cRun;
import com.carddemo.batch.support.FieldAsserts;
import com.carddemo.batch.support.Golden;
import com.carddemo.batch.support.JsonRecords;
import com.carddemo.batch.support.Repo;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * golden-files/CBACT01C/synthetic-mixed-debit: five accounts with ACCT-CURR-CYC-DEBIT 10.00 / 0 / 120.50 /
 * -75.25 / 0. Pins the legacy behaviour of the 2525.00 rule: OUT-ACCT-REC is never re-initialised, so a
 * non-zero input leaves the previous record's OUT-ACCT-CURR-CYC-DEBIT in place, and before the first zero
 * input the field holds never-assigned storage (LOW-VALUES, an invalid COMP-3, under GnuCOBOL).
 */
class SyntheticMixedDebitParityTest {

    private static Cbact01cRun run;

    @BeforeAll
    static void runProgram() {
        run = Cbact01cRun.syntheticMixedDebit();
    }

    @Test
    void fixtureInputMatchesGoldenInputJson() {
        List<Map<String, Object>> golden = Golden.records(Repo.GOLDEN_SYNTHETIC.resolve("input-acctdata.json"));
        FieldAsserts.recordSet("input-acctdata", "ACCT-ID", golden, run.inputJson(), Scales.ACCT);
        assertEquals(List.of("10.00", "0.00", "120.50", "-75.25", "0.00"),
                run.inputJson().stream().map(x -> x.get("ACCT-CURR-CYC-DEBIT")).toList(), "fixture ACCT-CURR-CYC-DEBIT values");
    }

    @Test
    void outfileFieldsMatchGoldenIncludingCarriedOverDebit() {
        List<Map<String, Object>> golden = Golden.records(Repo.GOLDEN_SYNTHETIC.resolve("outfile.json"));
        List<Map<String, Object>> actual = run.outfileJson();
        FieldAsserts.recordSet("OUTFILE", "OUT-ACCT-ID", golden, actual, Scales.OUTFILE);
        List<Object> debits = actual.stream().map(x -> x.get("OUT-ACCT-CURR-CYC-DEBIT")).toList();
        assertEquals(List.of(JsonRecords.INVALID_PREFIX + "00000000000000", "2525.00", "2525.00", "2525.00", "2525.00"),
                debits, "OUT-ACCT-CURR-CYC-DEBIT per record: undefined (LOW-VALUES) / 2525.00 / carried / carried / 2525.00");
    }

    @Test
    void arryfileFieldsMatchGolden() {
        FieldAsserts.recordSet("ARRYFILE", "ARR-ACCT-ID", Golden.records(Repo.GOLDEN_SYNTHETIC.resolve("arryfile.json")),
                run.arryfileJson(), Scales.ARRYFILE);
    }

    @Test
    void vbrcfileFieldsMatchGolden() {
        List<Map<String, Object>> golden = Golden.records(Repo.GOLDEN_SYNTHETIC.resolve("vbrcfile.json"));
        List<Map<String, Object>> actual = run.vbrcfileJson();
        assertEquals(golden.size(), actual.size(), "VBRCFILE record count");
        for (int i = 0; i < golden.size(); i++) {
            Map<String, Object> g = golden.get(i);
            String idField = "VBRC-REC1".equals(g.get("_record")) ? "VB1-ACCT-ID" : "VB2-ACCT-ID";
            org.junit.jupiter.api.Assertions.assertAll(FieldAsserts.record("VBRCFILE",
                    g.get("_record") + " " + idField + "=" + g.get(idField), g, actual.get(i), Scales.VBRCFILE));
        }
    }

    @Test
    void rawFilesAreByteIdentical() {
        RawOutputParityTest.assertSameBytes("synthetic OUTFILE", run.bytes(Repo.GOLDEN_SYNTHETIC.resolve("raw/OUTFILE")), run.bytes(run.outfile));
        RawOutputParityTest.assertSameBytes("synthetic ARRYFILE", run.bytes(Repo.GOLDEN_SYNTHETIC.resolve("raw/ARRYFILE")), run.bytes(run.arryfile));
        RawOutputParityTest.assertSameBytes("synthetic VBRCFILE", run.bytes(Repo.GOLDEN_SYNTHETIC.resolve("raw/VBRCFILE")), run.bytes(run.vbrcfile));
    }

    @Test
    void displayMatchesGolden() throws IOException {
        DisplayParityTest.assertSameLines("synthetic display.txt", Repo.GOLDEN_SYNTHETIC.resolve("display.txt"), run.display);
        assertTrue(run.display.contains("ACCT-CURR-CYC-DEBIT     :000000007525-\n"), "negative zoned DISPLAY of -75.25");
    }

    @Test
    void reconciliationChecksPass() {
        Reconciliation.Result result = Reconciliation.cbact01c(run);
        assertEquals(List.of(), result.failed(), "failed reconciliation checks:\n" + result.report());
        assertEquals(29, result.checks.size(), "number of checks");
        assertEquals("10100.00", result.actual("CBACT01C-TOTAL-CYC-DEBIT"), "4 defined rows x 2525.00");
        Map<String, Object> golden = Golden.object(Repo.GOLDEN_SYNTHETIC.resolve("reconciliation.json"));
        @SuppressWarnings("unchecked")
        Map<String, Object> summary = (Map<String, Object>) golden.get("summary");
        assertEquals(summary.get("passed"), result.passed(), "passed checks vs the fixture's reconciliation.json");
    }

    @Test
    void pythonHarnessAgrees() {
        PythonHarness.assertPasses(run, 29);
    }
}
