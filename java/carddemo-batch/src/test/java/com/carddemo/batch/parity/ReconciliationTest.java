package com.carddemo.batch.parity;

import com.carddemo.batch.support.Cbact01cRun;
import com.carddemo.batch.support.Golden;
import com.carddemo.batch.support.Repo;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Re-runs the CBACT01C checks of test-harness/RECONCILIATION_CHECKS.md on the Java output (sample data),
 * both as a Java re-implementation ({@link Reconciliation}) and through the Python harness itself.
 */
class ReconciliationTest {

    private static Reconciliation.Result result;

    @BeforeAll
    static void reconcile() {
        result = Reconciliation.cbact01c(Cbact01cRun.sample());
    }

    @Test
    void allTwentyNineChecksPass() {
        Map<String, Object> golden = Golden.object(Repo.GOLDEN_CBACT01C.resolve("reconciliation.json"));
        @SuppressWarnings("unchecked")
        Map<String, Object> summary = (Map<String, Object>) golden.get("summary");
        assertEquals(summary.get("checks"), result.checks.size(), "number of checks (same set as reconcile.py)");
        assertEquals(List.of(), result.failed(), "failed reconciliation checks:\n" + result.report());
        assertEquals(29, result.passed(), "passed checks");
    }

    @Test
    void recordCounts() {
        result.assertGroup("counts");
    }

    @Test
    void fieldTotals() {
        result.assertGroup("totals");
    }

    @Test
    void derivedFields() {
        result.assertGroup("derived");
    }

    @Test
    void crossReferenceIntegrity() {
        result.assertGroup("xref");
    }

    @Test
    void totalsEqualTheGoldenReconciliationValues() {
        Map<String, Object> golden = Golden.object(Repo.GOLDEN_CBACT01C.resolve("reconciliation.json"));
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> checks = (List<Map<String, Object>>) golden.get("checks");
        for (Map<String, Object> c : checks) {
            Object actual = result.actual((String) c.get("id"));
            if (c.get("actual") instanceof String) {
                assertEquals(c.get("actual"), actual, "check " + c.get("id") + " actual value vs golden reconciliation.json");
            }
        }
    }

    @Test
    void pythonHarnessAgrees() {
        PythonHarness.assertPasses(Cbact01cRun.sample(), 29);
    }
}
