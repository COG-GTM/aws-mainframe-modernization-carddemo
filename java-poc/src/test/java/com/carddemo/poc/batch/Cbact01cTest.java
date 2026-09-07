package com.carddemo.poc.batch;

import com.carddemo.poc.SampleData;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

class Cbact01cTest {

    private static List<String> run(Path acctFile) {
        ByteArrayOutputStream buf = new ByteArrayOutputStream();
        new Cbact01c(acctFile, new PrintStream(buf, true, StandardCharsets.ISO_8859_1)).run();
        return buf.toString(StandardCharsets.ISO_8859_1).lines().toList();
    }

    /** The EBCDIC data set decoded with CP037 and chunked into 300-byte records (what IDCAMS REPRO would load). */
    private static List<String> ebcdicRecords() throws IOException {
        String all = new String(Files.readAllBytes(SampleData.ebcdic("AWS.M2.CARDDEMO.ACCTDATA.PS")), "IBM037");
        List<String> records = new ArrayList<>();
        for (int i = 0; i < all.length(); i += 300) {
            records.add(all.substring(i, i + 300));
        }
        return records;
    }

    /** Expected SYSOUT built independently from the record images using the CVACT01Y offsets. */
    private static List<String> expectedSysout(List<String> records) {
        List<String> expected = new ArrayList<>();
        expected.add("START OF EXECUTION OF PROGRAM CBACT01C");
        for (String r : records) {
            expected.add("ACCT-ID                 :" + r.substring(0, 11));
            expected.add("ACCT-ACTIVE-STATUS      :" + r.substring(11, 12));
            expected.add("ACCT-CURR-BAL           :" + r.substring(12, 24));
            expected.add("ACCT-CREDIT-LIMIT       :" + r.substring(24, 36));
            expected.add("ACCT-CASH-CREDIT-LIMIT  :" + r.substring(36, 48));
            expected.add("ACCT-OPEN-DATE          :" + r.substring(48, 58));
            expected.add("ACCT-EXPIRAION-DATE     :" + r.substring(58, 68));
            expected.add("ACCT-REISSUE-DATE       :" + r.substring(68, 78));
            expected.add("ACCT-CURR-CYC-CREDIT    :" + r.substring(78, 90));
            expected.add("ACCT-CURR-CYC-DEBIT     :" + r.substring(90, 102));
            expected.add("ACCT-GROUP-ID           :" + r.substring(112, 122));
            expected.add("-------------------------------------------------");
            expected.add(r);
        }
        expected.add("END OF EXECUTION OF PROGRAM CBACT01C");
        return expected;
    }

    @Test
    void reproducesCobolDisplayOutputFromEbcdicKsdsSource() throws IOException {
        List<String> out = run(SampleData.ebcdic("AWS.M2.CARDDEMO.ACCTDATA.PS"));
        assertEquals(expectedSysout(ebcdicRecords()), out);
        assertEquals(1 + 50 * 13 + 1, out.size());
    }

    /**
     * The ASCII copy of the data differs from the EBCDIC one in record 49 (ACCT-GROUP-ID
     * {@code ZEROAPR} vs blank), so it is verified against its own record images.
     */
    @Test
    void reproducesCobolDisplayOutputFromAsciiSource() throws IOException {
        List<String> records = Files.readAllLines(SampleData.ascii("acctdata.txt"), StandardCharsets.ISO_8859_1)
                .stream().map(l -> String.format("%-300s", l)).toList();
        assertEquals(expectedSysout(records), run(SampleData.ascii("acctdata.txt")));
    }

    @Test
    void firstRecordFieldsAreDisplayedAsZonedDecimalText() {
        List<String> out = run(SampleData.ascii("acctdata.txt"));
        assertEquals("ACCT-ID                 :00000000001", out.get(1));
        assertEquals("ACCT-CURR-BAL           :00000001940{", out.get(3));
        assertEquals("ACCT-CREDIT-LIMIT       :00000020200{", out.get(4));
        assertEquals("ACCT-OPEN-DATE          :2014-11-20", out.get(6));
    }
}
