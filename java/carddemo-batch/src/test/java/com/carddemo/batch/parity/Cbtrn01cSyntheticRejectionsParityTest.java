package com.carddemo.batch.parity;

import com.carddemo.batch.program.TransactionOutcome;
import com.carddemo.batch.support.Cbtrn01cRun;
import com.carddemo.batch.support.FieldAsserts;
import com.carddemo.batch.support.Golden;
import com.carddemo.batch.support.Repo;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * golden-files/CBTRN01C/synthetic-rejections: the sample data never takes the INVALID KEY branches, so this
 * 3-transaction fixture (VERIFIED, CARD_NOT_FOUND, ACCOUNT_NOT_FOUND) covers both rejection paths.
 */
class Cbtrn01cSyntheticRejectionsParityTest {

    @Test
    void outcomesMatchGoldenFieldByField() {
        Cbtrn01cRun run = Cbtrn01cRun.syntheticRejections();
        assertEquals(0, run.returnCode, "CBTRN01C RETURN-CODE");
        List<Map<String, Object>> golden = Golden.records(Repo.GOLDEN_SYNTHETIC_REJECTIONS.resolve("outcomes.json"));
        assertEquals(3, golden.size());
        FieldAsserts.recordSet("synthetic-rejections outcomes", "tran_id", golden, run.outcomesJson(), Scales.OUTCOMES);
    }

    @Test
    void eachRejectionPathIsTakenOnce() {
        Cbtrn01cRun run = Cbtrn01cRun.syntheticRejections();
        List<TransactionOutcome> o = run.outcomes;
        assertEquals(3, o.size());
        assertEquals(TransactionOutcome.Outcome.VERIFIED, o.get(0).outcome(), "tran_id=" + o.get(0).tranId() + " field outcome");

        TransactionOutcome cardMissing = o.get(1);
        assertEquals("9999999999999999", cardMissing.cardNum(), "tran_id=" + cardMissing.tranId() + " field card_num");
        assertEquals(TransactionOutcome.Outcome.CARD_NOT_FOUND, cardMissing.outcome(), "tran_id=" + cardMissing.tranId() + " field outcome");
        assertNull(cardMissing.acctId(), "tran_id=" + cardMissing.tranId() + " field acct_id (3000-READ-ACCOUNT not performed)");
        assertNull(cardMissing.acctFound(), "tran_id=" + cardMissing.tranId() + " field acct_found");

        TransactionOutcome acctMissing = o.get(2);
        assertEquals("8888888888888888", acctMissing.cardNum(), "tran_id=" + acctMissing.tranId() + " field card_num");
        assertEquals(TransactionOutcome.Outcome.ACCOUNT_NOT_FOUND, acctMissing.outcome(), "tran_id=" + acctMissing.tranId() + " field outcome");
        assertTrue(acctMissing.xrefFound(), "tran_id=" + acctMissing.tranId() + " field xref_found");
        assertEquals(99999999999L, acctMissing.acctId(), "tran_id=" + acctMissing.tranId() + " field acct_id");
        assertEquals(Boolean.FALSE, acctMissing.acctFound(), "tran_id=" + acctMissing.tranId() + " field acct_found");
    }

    @Test
    void displayMatchesGoldenIncludingBothRejectionMessages() throws IOException {
        Cbtrn01cRun run = Cbtrn01cRun.syntheticRejections();
        DisplayParityTest.assertSameLines("synthetic-rejections display.txt",
                Repo.GOLDEN_SYNTHETIC_REJECTIONS.resolve("display.txt"), run.display);
        assertTrue(run.display.contains("INVALID CARD NUMBER FOR XREF\nCARD NUMBER 9999999999999999 COULD NOT BE VERIFIED."
                + " SKIPPING TRANSACTION ID-0000000001774260\n"), "INVALID KEY branch of 2000-LOOKUP-XREF");
        assertTrue(run.display.contains("INVALID ACCOUNT NUMBER FOUND\nACCOUNT 99999999999 NOT FOUND\n"),
                "INVALID KEY branch of 3000-READ-ACCOUNT");
        assertEquals(4, run.displayLookups(), "3 transactions + the post-EOF lookup of the last record");
        assertEquals(2, run.display.split("ACCOUNT 99999999999 NOT FOUND\n", -1).length - 1,
                "the post-EOF lookup repeats the ACCOUNT_NOT_FOUND path of transaction 3");
    }

    @Test
    void reconciliationChecksPassInJavaAndPython() {
        Cbtrn01cRun run = Cbtrn01cRun.syntheticRejections();
        Reconciliation.Result result = Reconciliation.cbtrn01c(run);
        assertEquals(List.of(), result.failed(), "failed reconciliation checks:\n" + result.report());
        assertEquals(11, result.passed());
        Map<String, Object> golden = Golden.object(Repo.GOLDEN_SYNTHETIC_REJECTIONS.resolve("reconciliation.json"));
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> checks = (List<Map<String, Object>>) golden.get("checks");
        for (Map<String, Object> c : checks) {
            assertEquals(c.get("actual"), result.actual((String) c.get("id")),
                    "check " + c.get("id") + " actual value vs golden reconciliation.json");
        }
        assertEquals(Map.of("VERIFIED", "504.77", "CARD_NOT_FOUND", "-919.00", "ACCOUNT_NOT_FOUND", "67.88"),
                result.actual("CBTRN01C-TOTAL-02"), "per-class totals");
        PythonHarness.assertPasses(run, 11);
    }
}
