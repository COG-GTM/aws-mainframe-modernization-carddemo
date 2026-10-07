package com.carddemo.batch.parity;

import com.carddemo.batch.support.Cbact01cRun;
import com.carddemo.batch.support.Golden;
import com.carddemo.batch.support.Repo;
import org.junit.jupiter.api.Assumptions;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;

/** Runs {@code python3 test-harness/reconcile.py cbact01c --golden-dir <java output>} when python3 exists. */
final class PythonHarness {

    private PythonHarness() {
    }

    static void assertPasses(Cbact01cRun run, int expectedChecks) {
        Path dir = run.writeJsonForHarness();
        Process p;
        try {
            p = new ProcessBuilder("python3", Repo.RECONCILE_PY.toString(), "cbact01c", "--golden-dir", dir.toString(), "--write")
                    .redirectErrorStream(true).start();
        } catch (IOException e) {
            Assumptions.abort("python3 not available: " + e.getMessage());
            return;
        }
        String out;
        int rc;
        try {
            out = new String(p.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
            rc = p.waitFor();
        } catch (IOException | InterruptedException e) {
            throw new IllegalStateException(e);
        }
        assertEquals(0, rc, "reconcile.py exit status\n" + out);
        Map<String, Object> res = Golden.object(dir.resolve("reconciliation.json"));
        @SuppressWarnings("unchecked")
        Map<String, Object> summary = (Map<String, Object>) res.get("summary");
        assertEquals("PASS", summary.get("status"), "reconcile.py status\n" + out);
        assertEquals(expectedChecks, summary.get("checks"), "reconcile.py check count");
        assertEquals(expectedChecks, summary.get("passed"), "reconcile.py passed count\n" + out);
        assertEquals(List.of(), summary.get("failed_ids"), "reconcile.py failed ids");
    }

    static boolean available() {
        try {
            return new ProcessBuilder("python3", "--version").start().waitFor() == 0 && Files.exists(Repo.RECONCILE_PY);
        } catch (IOException | InterruptedException e) {
            return false;
        }
    }
}
