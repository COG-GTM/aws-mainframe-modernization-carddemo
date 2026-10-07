package com.carddemo.batch.parity;

import com.carddemo.batch.record.ArrArrayRec;
import com.carddemo.batch.support.Cbact01cRun;
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

/** ARRYFILE (110-byte ARR-ARRAY-REC, OCCURS 5) against golden-files/CBACT01C/arryfile.json. */
class ArryfileParityTest {

    private static Cbact01cRun run;
    private static List<Map<String, Object>> golden;
    private static List<Map<String, Object>> actual;

    @BeforeAll
    static void runProgram() {
        run = Cbact01cRun.sample();
        golden = Golden.records(Repo.GOLDEN_CBACT01C.resolve("arryfile.json"));
        actual = run.arryfileJson();
    }

    @Test
    void recordCountMatchesGolden() {
        assertEquals(golden.size(), actual.size(), "ARRYFILE record count");
        assertEquals(golden.size() * ArrArrayRec.LENGTH, run.bytes(run.arryfile).length, "ARRYFILE byte length (LRECL 110)");
    }

    @Test
    void everyFieldOfEveryOccurrenceMatchesGolden() {
        FieldAsserts.recordSet("ARRYFILE", "ARR-ACCT-ID", golden, actual, Scales.ARRYFILE);
    }

    @Test
    void occurrencesAreFixedAtFive() {
        for (Map<String, Object> rec : actual) {
            @SuppressWarnings("unchecked")
            List<Map<String, Object>> occ = (List<Map<String, Object>>) rec.get("ARR-ACCT-BAL");
            assertEquals(ArrArrayRec.OCCURS, occ.size(), "ARRYFILE record ARR-ACCT-ID=" + rec.get("ARR-ACCT-ID") + " OCCURS");
        }
        assertEquals(5, new ArrArrayRec().arrAcctBal().length, "ArrArrayRec.arrAcctBal() array size");
    }

    @Test
    void hardCodedConstantsAndCopiesPerOccurrence() {
        List<Map<String, Object>> input = run.inputJson();
        List<Executable> checks = new ArrayList<>();
        for (int i = 0; i < actual.size(); i++) {
            Map<String, Object> rec = actual.get(i);
            String key = "ARR-ACCT-ID=" + rec.get("ARR-ACCT-ID");
            @SuppressWarnings("unchecked")
            List<Map<String, Object>> occ = (List<Map<String, Object>>) rec.get("ARR-ACCT-BAL");
            String bal = (String) input.get(i).get("ACCT-CURR-BAL");
            checks.add(FieldAsserts.decimalField("ARRYFILE", key, "ARR-ACCT-BAL(1).ARR-ACCT-CURR-BAL", bal, occ.get(0).get("ARR-ACCT-CURR-BAL"), 2));
            checks.add(FieldAsserts.decimalField("ARRYFILE", key, "ARR-ACCT-BAL(1).ARR-ACCT-CURR-CYC-DEBIT", "1005.00", occ.get(0).get("ARR-ACCT-CURR-CYC-DEBIT"), 2));
            checks.add(FieldAsserts.decimalField("ARRYFILE", key, "ARR-ACCT-BAL(2).ARR-ACCT-CURR-BAL", bal, occ.get(1).get("ARR-ACCT-CURR-BAL"), 2));
            checks.add(FieldAsserts.decimalField("ARRYFILE", key, "ARR-ACCT-BAL(2).ARR-ACCT-CURR-CYC-DEBIT", "1525.00", occ.get(1).get("ARR-ACCT-CURR-CYC-DEBIT"), 2));
            checks.add(FieldAsserts.decimalField("ARRYFILE", key, "ARR-ACCT-BAL(3).ARR-ACCT-CURR-BAL", "-1025.00", occ.get(2).get("ARR-ACCT-CURR-BAL"), 2));
            checks.add(FieldAsserts.decimalField("ARRYFILE", key, "ARR-ACCT-BAL(3).ARR-ACCT-CURR-CYC-DEBIT", "-2500.00", occ.get(2).get("ARR-ACCT-CURR-CYC-DEBIT"), 2));
            for (int o = 4; o <= 5; o++) {
                checks.add(FieldAsserts.decimalField("ARRYFILE", key, "ARR-ACCT-BAL(" + o + ").ARR-ACCT-CURR-BAL", "0.00", occ.get(o - 1).get("ARR-ACCT-CURR-BAL"), 2));
                checks.add(FieldAsserts.decimalField("ARRYFILE", key, "ARR-ACCT-BAL(" + o + ").ARR-ACCT-CURR-CYC-DEBIT", "0.00", occ.get(o - 1).get("ARR-ACCT-CURR-CYC-DEBIT"), 2));
            }
            checks.add(FieldAsserts.textField("ARRYFILE", key, "ARR-FILLER", "    ", rec.get("ARR-FILLER")));
        }
        assertAll(checks);
    }

    @Test
    void negativeConstantsRoundTripThroughZonedAndPackedCodecs() {
        ArrArrayRec rec = ArrArrayRec.decode(run.arryfileRecords().get(0));
        assertEquals(0, new BigDecimal("-1025.00").compareTo(rec.arrAcctBal(3).arrAcctCurrBal()), "ARR-ACCT-CURR-BAL(3)");
        assertEquals(2, rec.arrAcctBal(3).arrAcctCurrBal().scale(), "ARR-ACCT-CURR-BAL(3) scale");
        assertEquals(0, new BigDecimal("-2500.00").compareTo(rec.arrAcctBal(3).arrAcctCurrCycDebit()), "ARR-ACCT-CURR-CYC-DEBIT(3)");
        assertEquals(2, rec.arrAcctBal(3).arrAcctCurrCycDebit().scale(), "ARR-ACCT-CURR-CYC-DEBIT(3) scale");
    }
}
