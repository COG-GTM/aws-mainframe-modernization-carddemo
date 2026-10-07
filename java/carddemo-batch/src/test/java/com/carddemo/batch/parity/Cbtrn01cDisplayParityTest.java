package com.carddemo.batch.parity;

import com.carddemo.batch.record.DalytranRecord;
import com.carddemo.batch.support.Cbtrn01cRun;
import com.carddemo.batch.support.Repo;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/** The CBTRN01C DISPLAY output (stdout) against golden-files/CBTRN01C/display.txt. */
class Cbtrn01cDisplayParityTest {

    @Test
    void displayOutputMatchesGoldenLineByLine() throws IOException {
        Cbtrn01cRun run = Cbtrn01cRun.sample();
        DisplayParityTest.assertSameLines("CBTRN01C display.txt", Repo.GOLDEN_CBTRN01C.resolve("display.txt"), run.display);
    }

    @Test
    void displayHasStartEndAndOneBlockPerTransactionPlusThePostEofLookup() {
        Cbtrn01cRun run = Cbtrn01cRun.sample();
        String display = run.display;
        assertTrue(display.startsWith("START OF EXECUTION OF PROGRAM CBTRN01C\n"), "first DISPLAY line");
        assertTrue(display.endsWith("END OF EXECUTION OF PROGRAM CBTRN01C\n"), "last DISPLAY line");
        assertEquals(301, run.displayLookups(), "XREF lookups = 300 transactions + the post-EOF lookup");
        assertEquals(301, display.split("SUCCESSFUL READ OF ACCOUNT FILE\n", -1).length - 1, "account reads");
        assertEquals(300 * 6 + 5 + 2, display.split("\n", -1).length - 1,
                "DISPLAY lines: 6 per transaction (record + 4 xref lines + account line), 5 for the post-EOF lookup, start/end");
    }

    @Test
    void eachTransactionIsDisplayedAsItsRaw350ByteRecord() {
        Cbtrn01cRun run = Cbtrn01cRun.sample();
        List<String> lines = List.of(run.display.split("\n", -1));
        List<byte[]> recs = Cbtrn01cRun.readRecords(run.dalytran, DalytranRecord.LENGTH);
        for (int i = 0; i < recs.size(); i++) {
            String expected = new String(recs.get(i), java.nio.charset.StandardCharsets.ISO_8859_1);
            String line = lines.get(1 + i * 6);
            String tranId = expected.substring(0, 16);
            assertEquals(350, line.length(), "tran_id=" + tranId + " DISPLAY DALYTRAN-RECORD length");
            assertEquals(expected, line, "tran_id=" + tranId + " DISPLAY DALYTRAN-RECORD (raw bytes incl. overpunched DALYTRAN-AMT)");
        }
    }

    @Test
    void postEofLookupRepeatsTheLastTransactionWithoutDisplayingTheRecord() {
        Cbtrn01cRun run = Cbtrn01cRun.sample();
        List<String> lines = List.of(run.display.split("\n", -1));
        int n = lines.size() - 1;
        List<String> last = lines.subList(n - 6, n - 1);
        List<String> previous = lines.subList(n - 11, n - 6);
        assertEquals(previous, last, "post-EOF xref/account lookup lines equal those of the 300th transaction");
        assertTrue(lines.get(n - 7).startsWith("SUCCESSFUL READ OF ACCOUNT FILE"),
                "no DALYTRAN-RECORD line precedes the post-EOF lookup");
    }
}
