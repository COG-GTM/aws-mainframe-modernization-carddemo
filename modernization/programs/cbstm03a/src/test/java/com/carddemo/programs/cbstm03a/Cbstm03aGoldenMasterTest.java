package com.carddemo.programs.cbstm03a;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.List;
import org.junit.jupiter.api.Test;

/**
 * Fixture test: app/data/ASCII samples through CREASTMT STEP010 (reshape) and CBSTM03A,
 * compared byte-for-byte with output of the original COBOL under GnuCOBOL
 * (golden/generate-golden.sh).
 */
class Cbstm03aGoldenMasterTest {

    @Test
    void sampleDataProducesExactlyTheCobolStatementAndHtmlFiles() {
        List<String> trnx = TestData.lines(TestData.ASCII.resolve("dailytran.txt")).stream()
                .map(TestData::reshapeDailyTran).toList();
        TestData.Run run = TestData.run(trnx,
                TestData.lines(TestData.ASCII.resolve("cardxref.txt")),
                TestData.lines(TestData.ASCII.resolve("custdata.txt")),
                TestData.lines(TestData.ASCII.resolve("acctdata.txt")));

        assertNull(run.abend());
        List<String> stmtGolden = TestData.golden("STMTFILE.txt");
        List<String> htmlGolden = TestData.golden("HTMLFILE.txt");
        assertEquals(1250, stmtGolden.size());
        assertEquals(6500, htmlGolden.size());
        assertEquals(stmtGolden.size(), run.stmt().size());
        assertEquals(htmlGolden.size(), run.html().size());
        for (int i = 0; i < stmtGolden.size(); i++) {
            assertEquals(stmtGolden.get(i), run.stmt().get(i), "STMTFILE record " + (i + 1));
        }
        for (int i = 0; i < htmlGolden.size(); i++) {
            assertEquals(htmlGolden.get(i), run.html().get(i), "HTMLFILE record " + (i + 1));
        }
    }
}
