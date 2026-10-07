package com.carddemo.batch.parity;

import com.carddemo.batch.record.OutAcctRec;
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
import static org.junit.jupiter.api.Assertions.assertTrue;

/** OUTFILE (107-byte OUT-ACCT-REC) field by field against golden-files/CBACT01C/outfile.json. */
class OutfileParityTest {

    private static Cbact01cRun run;
    private static List<Map<String, Object>> golden;
    private static List<Map<String, Object>> actual;

    @BeforeAll
    static void runProgram() {
        run = Cbact01cRun.sample();
        golden = Golden.records(Repo.GOLDEN_CBACT01C.resolve("outfile.json"));
        actual = run.outfileJson();
    }

    @Test
    void programEndsWithReturnCodeZero() {
        assertEquals(0, run.returnCode, "CBACT01C RETURN-CODE");
    }

    @Test
    void recordCountMatchesGolden() {
        assertEquals(50, golden.size(), "golden OUTFILE record count");
        assertEquals(golden.size(), actual.size(), "OUTFILE record count");
        assertEquals(golden.size() * OutAcctRec.LENGTH, run.bytes(run.outfile).length, "OUTFILE byte length (LRECL 107)");
    }

    @Test
    void everyFieldOfEveryRecordMatchesGolden() {
        FieldAsserts.recordSet("OUTFILE", "OUT-ACCT-ID", golden, actual, Scales.OUTFILE);
    }

    @Test
    void moneyFieldsHaveScaleTwoAndCompareEqual() {
        List<Executable> checks = new ArrayList<>();
        for (int i = 0; i < golden.size(); i++) {
            String key = "OUT-ACCT-ID=" + golden.get(i).get("OUT-ACCT-ID");
            for (String f : List.of("OUT-ACCT-CURR-BAL", "OUT-ACCT-CREDIT-LIMIT", "OUT-ACCT-CASH-CREDIT-LIMIT",
                    "OUT-ACCT-CURR-CYC-CREDIT", "OUT-ACCT-CURR-CYC-DEBIT")) {
                checks.add(FieldAsserts.decimalField("OUTFILE", key, f, (String) golden.get(i).get(f),
                        actual.get(i).get(f), 2));
            }
        }
        assertAll(checks);
    }

    @Test
    void datesAreExactTextWithTrailingSpacesPreserved() {
        List<Executable> checks = new ArrayList<>();
        for (int i = 0; i < golden.size(); i++) {
            String key = "OUT-ACCT-ID=" + golden.get(i).get("OUT-ACCT-ID");
            for (String f : List.of("OUT-ACCT-OPEN-DATE", "OUT-ACCT-EXPIRAION-DATE", "OUT-ACCT-REISSUE-DATE",
                    "OUT-ACCT-GROUP-ID", "OUT-ACCT-ACTIVE-STATUS")) {
                checks.add(FieldAsserts.textField("OUTFILE", key, f, (String) golden.get(i).get(f), actual.get(i).get(f)));
            }
            String reissue = (String) actual.get(i).get("OUT-ACCT-REISSUE-DATE");
            checks.add(() -> assertEquals(10, reissue.length(), "OUTFILE record " + key + " field OUT-ACCT-REISSUE-DATE width"));
            checks.add(() -> assertTrue(reissue.matches("\\d{8}  "),
                    "OUTFILE record " + key + " field OUT-ACCT-REISSUE-DATE should be YYYYMMDD + 2 spaces, was '" + reissue + "'"));
        }
        assertAll(checks);
    }

    @Test
    void cycDebitIs2525WhenInputDebitIsZero() {
        List<Map<String, Object>> input = run.inputJson();
        BigDecimal total = BigDecimal.ZERO;
        for (int i = 0; i < input.size(); i++) {
            String key = "OUT-ACCT-ID=" + actual.get(i).get("OUT-ACCT-ID");
            assertEquals(0, new BigDecimal((String) input.get(i).get("ACCT-CURR-CYC-DEBIT")).signum(),
                    "sample input " + key + " ACCT-CURR-CYC-DEBIT is expected to be zero");
            BigDecimal debit = new BigDecimal((String) actual.get(i).get("OUT-ACCT-CURR-CYC-DEBIT"));
            assertEquals(0, new BigDecimal("2525.00").compareTo(debit), "OUTFILE record " + key + " field OUT-ACCT-CURR-CYC-DEBIT");
            total = total.add(debit);
        }
        assertEquals(0, new BigDecimal("126250.00").compareTo(total), "sum OUT-ACCT-CURR-CYC-DEBIT");
    }
}
