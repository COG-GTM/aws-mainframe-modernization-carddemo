package com.carddemo.batch.parity;

import com.carddemo.batch.program.TransactionOutcome;
import com.carddemo.batch.support.Cbtrn01cRun;
import com.carddemo.batch.support.FieldAsserts;
import com.carddemo.batch.support.Golden;
import com.carddemo.batch.support.Repo;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * The CBTRN01C outcome rows (one per DALYTRAN record, file order) field by field against
 * golden-files/CBTRN01C/outcomes.json, and the Java decoding of the DALYTRAN / XREFFILE / ACCTFILE inputs
 * against the golden input JSON. Every failure message names the transaction id and the field.
 */
class Cbtrn01cOutcomesParityTest {

    private static Cbtrn01cRun run;
    private static List<Map<String, Object>> golden;
    private static List<Map<String, Object>> actual;

    @BeforeAll
    static void runProgram() {
        run = Cbtrn01cRun.sample();
        golden = Golden.records(Repo.GOLDEN_CBTRN01C.resolve("outcomes.json"));
        actual = run.outcomesJson();
    }

    @Test
    void programEndsWithReturnCodeZero() {
        assertEquals(0, run.returnCode, "CBTRN01C RETURN-CODE");
    }

    @Test
    void outcomeRowCountMatchesGoldenAndInput() {
        assertEquals(300, golden.size(), "golden outcome rows");
        assertEquals(golden.size(), actual.size(), "outcome rows");
        assertEquals(Cbtrn01cRun.readRecords(run.dalytran, 350).size(), actual.size(), "one outcome per DALYTRAN record");
    }

    @Test
    void everyFieldOfEveryOutcomeMatchesGolden() {
        FieldAsserts.recordSet("outcomes", "tran_id", golden, actual, Scales.OUTCOMES);
    }

    @Test
    void tranIdAndCardNumAreExactSixteenByteText() {
        List<Executable> checks = new ArrayList<>();
        for (int i = 0; i < golden.size(); i++) {
            Map<String, Object> g = golden.get(i);
            TransactionOutcome o = run.outcomes.get(i);
            String key = "tran_id=" + g.get("tran_id");
            checks.add(FieldAsserts.textField("outcomes", key, "tran_id", (String) g.get("tran_id"), o.tranId()));
            checks.add(FieldAsserts.textField("outcomes", key, "card_num", (String) g.get("card_num"), o.cardNum()));
            checks.add(() -> assertEquals(16, o.tranId().length(), key + " field tran_id length (PIC X(16))"));
            checks.add(() -> assertEquals(16, o.cardNum().length(), key + " field card_num length (PIC X(16))"));
        }
        assertAll("outcomes text fields", checks);
    }

    @Test
    void lookupFlagsAndAccountIdMatchGolden() {
        List<Executable> checks = new ArrayList<>();
        for (int i = 0; i < golden.size(); i++) {
            Map<String, Object> g = golden.get(i);
            TransactionOutcome o = run.outcomes.get(i);
            String key = "tran_id=" + g.get("tran_id");
            checks.add(() -> assertEquals(g.get("xref_found"), o.xrefFound(), key + " field xref_found"));
            checks.add(() -> assertEquals(g.get("acct_found"), o.acctFound(), key + " field acct_found"));
            checks.add(() -> assertEquals(g.get("outcome"), o.outcome().name(), key + " field outcome"));
            if (g.get("acct_id") == null) {
                checks.add(() -> assertNull(o.acctId(), key + " field acct_id"));
            } else {
                checks.add(FieldAsserts.decimalField("outcomes", key, "acct_id", (String) g.get("acct_id"),
                        o.acctId() == null ? null : Long.toString(o.acctId()), 0));
            }
        }
        assertAll("outcomes lookup fields", checks);
    }

    @Test
    void allSampleTransactionsAreVerified() {
        for (TransactionOutcome o : run.outcomes) {
            assertEquals(TransactionOutcome.Outcome.VERIFIED, o.outcome(), "tran_id=" + o.tranId() + " field outcome");
            assertTrue(o.xrefFound() && Boolean.TRUE.equals(o.acctFound()), "tran_id=" + o.tranId() + " lookups");
        }
    }

    @Test
    void dalytranInputDecodesFieldForFieldLikeTheGolden() {
        List<Map<String, Object>> goldenIn = Golden.records(Repo.GOLDEN_CBTRN01C.resolve("input-dailytran.json"));
        assertEquals(300, goldenIn.size(), "golden DALYTRAN records");
        FieldAsserts.recordSet("DALYTRAN", "DALYTRAN-ID", goldenIn, run.inputJson(), Scales.DALYTRAN);
    }

    @Test
    void dalytranAmountsHaveScaleTwoAndCompareEqual() {
        List<Map<String, Object>> goldenIn = Golden.records(Repo.GOLDEN_CBTRN01C.resolve("input-dailytran.json"));
        List<Map<String, Object>> in = run.inputJson();
        List<Executable> checks = new ArrayList<>();
        for (int i = 0; i < goldenIn.size(); i++) {
            String key = "tran_id=" + goldenIn.get(i).get("DALYTRAN-ID");
            Object act = in.get(i).get("DALYTRAN-AMT");
            checks.add(FieldAsserts.decimalField("DALYTRAN", key, "DALYTRAN-AMT", (String) goldenIn.get(i).get("DALYTRAN-AMT"), act, 2));
            checks.add(() -> assertEquals(2, new BigDecimal((String) act).scale(), key + " field DALYTRAN-AMT scale (PIC S9(09)V99)"));
        }
        assertAll("DALYTRAN-AMT", checks);
    }

    @Test
    void dalytranTimestampsAreExactText() {
        List<Map<String, Object>> goldenIn = Golden.records(Repo.GOLDEN_CBTRN01C.resolve("input-dailytran.json"));
        List<Map<String, Object>> in = run.inputJson();
        List<Executable> checks = new ArrayList<>();
        for (int i = 0; i < goldenIn.size(); i++) {
            String key = "tran_id=" + goldenIn.get(i).get("DALYTRAN-ID");
            for (String f : List.of("DALYTRAN-ORIG-TS", "DALYTRAN-PROC-TS")) {
                String exp = (String) goldenIn.get(i).get(f);
                checks.add(FieldAsserts.textField("DALYTRAN", key, f, exp, in.get(i).get(f)));
                checks.add(() -> assertEquals(26, exp.length(), key + " field " + f + " golden length (PIC X(26))"));
            }
        }
        assertAll("DALYTRAN timestamps", checks);
        assertTrue(in.stream().allMatch(r -> "                          ".equals(r.get("DALYTRAN-PROC-TS"))),
                "DALYTRAN-PROC-TS is 26 spaces in the sample data (trailing spaces preserved)");
    }

    @Test
    void xrefAndAccountLookupTablesDecodeLikeTheGolden() {
        FieldAsserts.recordSet("XREFFILE", "XREF-CARD-NUM",
                Golden.records(Repo.GOLDEN_CBTRN01C.resolve("input-cardxref.json")), run.xrefJson(), Scales.CARDXREF);
        FieldAsserts.recordSet("ACCTFILE", "ACCT-ID",
                Golden.records(Repo.GOLDEN_CBTRN01C.resolve("input-acctdata.json")), run.acctJson(), Scales.ACCT);
    }
}
