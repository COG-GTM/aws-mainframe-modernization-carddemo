package com.carddemo.batch.parity;

import com.carddemo.batch.support.Cbtrn01cRun;
import com.carddemo.batch.support.Golden;
import com.carddemo.batch.support.Repo;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Re-runs the 11 CBTRN01C checks of test-harness/RECONCILIATION_CHECKS.md on the Java output (sample data),
 * both as a Java re-implementation ({@link Reconciliation#cbtrn01c}) and through the Python harness itself.
 */
class Cbtrn01cReconciliationTest {

    private static Reconciliation.Result result;

    @BeforeAll
    static void reconcile() {
        result = Reconciliation.cbtrn01c(Cbtrn01cRun.sample());
    }

    @Test
    void allElevenChecksPass() {
        Map<String, Object> golden = Golden.object(Repo.GOLDEN_CBTRN01C.resolve("reconciliation.json"));
        @SuppressWarnings("unchecked")
        Map<String, Object> summary = (Map<String, Object>) golden.get("summary");
        assertEquals(summary.get("checks"), result.checks.size(), "number of checks (same set as reconcile.py)");
        assertEquals(List.of(), result.failed(), "failed reconciliation checks:\n" + result.report());
        assertEquals(11, result.passed(), "passed checks");
    }

    @Test
    void recordCounts() {
        result.assertGroup("counts");
        assertEquals(300, result.actual("CBTRN01C-COUNT-01"), "outcome rows");
        assertEquals(301, result.actual("CBTRN01C-COUNT-04"), "xref lookups in display (n + 1)");
    }

    @Test
    void amountTotalsPerOutcomeClass() {
        result.assertGroup("totals");
        assertEquals("104801.54", result.actual("CBTRN01C-TOTAL-01"), "sum DALYTRAN-AMT");
        assertEquals(Map.of("VERIFIED", "104801.54", "CARD_NOT_FOUND", "0.00", "ACCOUNT_NOT_FOUND", "0.00"),
                result.actual("CBTRN01C-TOTAL-02"), "per-class totals");
        assertEquals(Map.of("positive", "129200.83", "negative", "-24399.29", "zero_count", 0),
                result.actual("CBTRN01C-TOTAL-03"), "debit / credit split");
    }

    @Test
    void crossReferenceIntegrity() {
        result.assertGroup("xref");
        assertEquals(50, result.actual("CBTRN01C-XREF-04"), "distinct cards used by the transactions");
    }

    @Test
    void valuesEqualTheGoldenReconciliationValues() {
        Map<String, Object> golden = Golden.object(Repo.GOLDEN_CBTRN01C.resolve("reconciliation.json"));
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> checks = (List<Map<String, Object>>) golden.get("checks");
        assertEquals(11, checks.size());
        for (Map<String, Object> c : checks) {
            assertEquals(c.get("actual"), result.actual((String) c.get("id")),
                    "check " + c.get("id") + " actual value vs golden reconciliation.json");
        }
    }

    @Test
    void pythonHarnessAgrees() {
        PythonHarness.assertPasses(Cbtrn01cRun.sample(), 11);
    }
}
