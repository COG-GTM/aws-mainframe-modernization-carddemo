package com.carddemo.batch.parity;

import com.carddemo.batch.record.VbrcRec1;
import com.carddemo.batch.record.VbrcRec2;
import com.carddemo.batch.support.Cbact01cRun;
import com.carddemo.batch.support.FieldAsserts;
import com.carddemo.batch.support.Golden;
import com.carddemo.batch.support.Repo;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.function.Executable;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertEquals;

/** VBRCFILE (variable: 12-byte VBRC-REC1 then 39-byte VBRC-REC2 per account) against vbrcfile.json. */
class VbrcfileParityTest {

    private static Cbact01cRun run;
    private static List<Map<String, Object>> golden;
    private static List<Map<String, Object>> actual;

    @BeforeAll
    static void runProgram() {
        run = Cbact01cRun.sample();
        golden = Golden.records(Repo.GOLDEN_CBACT01C.resolve("vbrcfile.json"));
        actual = run.vbrcfileJson();
    }

    @Test
    void recordCountIsTwoPerAccount() {
        assertEquals(100, golden.size(), "golden VBRCFILE record count");
        assertEquals(golden.size(), actual.size(), "VBRCFILE record count");
    }

    @Test
    void recordLengthsAlternate12And39() {
        List<byte[]> payloads = run.vbrcfilePayloads();
        List<Executable> checks = new ArrayList<>();
        for (int i = 0; i < payloads.size(); i++) {
            int expected = i % 2 == 0 ? VbrcRec1.LENGTH : VbrcRec2.LENGTH;
            int idx = i;
            checks.add(() -> assertEquals(expected, payloads.get(idx).length, "VBRCFILE record #" + (idx + 1) + " payload length"));
            checks.add(() -> assertEquals(idx % 2 == 0 ? "VBRC-REC1" : "VBRC-REC2", actual.get(idx).get("_record"),
                    "VBRCFILE record #" + (idx + 1) + " field _record"));
        }
        assertAll(checks);
        assertEquals(12, VbrcRec1.LENGTH);
        assertEquals(39, VbrcRec2.LENGTH);
    }

    @Test
    void everyFieldOfEveryRecordMatchesGolden() {
        assertEquals(golden.size(), actual.size(), "VBRCFILE record count");
        List<Executable> checks = new ArrayList<>();
        for (int i = 0; i < golden.size(); i++) {
            Map<String, Object> g = golden.get(i);
            String idField = "VBRC-REC1".equals(g.get("_record")) ? "VB1-ACCT-ID" : "VB2-ACCT-ID";
            String key = g.get("_record") + " " + idField + "=" + g.get(idField) + " (#" + (i + 1) + ")";
            checks.addAll(FieldAsserts.record("VBRCFILE", key, g, actual.get(i), Scales.VBRCFILE));
        }
        assertAll("VBRCFILE field-level parity", checks);
    }

    @Test
    void reissueYearIsFirstFourCharactersOfInputReissueDate() {
        List<Map<String, Object>> input = run.inputJson();
        List<Executable> checks = new ArrayList<>();
        for (int i = 0; i < input.size(); i++) {
            Map<String, Object> vb2 = actual.get(2 * i + 1);
            String key = "VB2-ACCT-ID=" + vb2.get("VB2-ACCT-ID");
            String expected = ((String) input.get(i).get("ACCT-REISSUE-DATE")).substring(0, 4);
            checks.add(FieldAsserts.textField("VBRCFILE", key, "VB2-ACCT-REISSUE-YYYY", expected, vb2.get("VB2-ACCT-REISSUE-YYYY")));
            Map<String, Object> vb1 = actual.get(2 * i);
            checks.add(FieldAsserts.textField("VBRCFILE", "VB1-ACCT-ID=" + vb1.get("VB1-ACCT-ID"), "VB1-ACCT-ACTIVE-STATUS",
                    (String) input.get(i).get("ACCT-ACTIVE-STATUS"), vb1.get("VB1-ACCT-ACTIVE-STATUS")));
        }
        assertAll(checks);
    }
}
