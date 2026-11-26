package com.carddemo.programs.corpt00c.copybook;

import com.carddemo.programs.corpt00c.TranReportJob;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Fixture test over app/data/ASCII/dailytran.txt (CVTRA05Y layout): the SORT SYMNAMES written by CORPT00C
 * (TRAN-CARD-NUM,263,16 and TRAN-PROC-DT,305,10) must address the copybook fields.
 */
class TranRecordFixtureTest {

    private static List<String> fixture() throws IOException {
        Path dir = Path.of(System.getProperty("carddemo.data.dir", "../../../app/data/ASCII"));
        return Files.readAllLines(dir.resolve("dailytran.txt"));
    }

    @Test
    void dailytranRecordsParseAndRoundTrip() throws IOException {
        List<String> lines = fixture();
        assertEquals(300, lines.size());
        for (String line : lines) {
            TranRecord r = TranRecord.fromFixedWidth(line);
            assertEquals(String.format("%-350s", line), r.toFixedWidth());
        }
        TranRecord first = TranRecord.fromFixedWidth(lines.get(0));
        assertEquals("0000000000683580", first.tranId());
        assertEquals("01", first.tranTypeCd());
        assertEquals(1, first.tranCatCd());
        assertEquals(new BigDecimal("504.77"), first.tranAmt());
        assertEquals("4859452612877065", first.tranCardNum());
        assertEquals("2022-06-10 19:27:53.000000", first.tranOrigTs());
        assertEquals(new BigDecimal("-919.00"), TranRecord.fromFixedWidth(lines.get(1)).tranAmt());
    }

    @Test
    void symnamesInJobDeckAddressCopybookFields() throws IOException {
        TranReportJob job = new TranReportJob();
        job.setStartDate("2022-06-01");
        job.setEndDate("2022-06-30");
        String[] card = symname(job, "TRAN-CARD-NUM");
        String[] proc = symname(job, "TRAN-PROC-DT");
        assertEquals(TranRecord.OFF_TRAN_CARD_NUM + 1, Integer.parseInt(card[1]));
        assertEquals(16, Integer.parseInt(card[2]));
        assertEquals(TranRecord.OFF_TRAN_PROC_TS + 1, Integer.parseInt(proc[1]));

        String line = fixture().get(0);
        int cardPos = Integer.parseInt(card[1]) - 1;
        assertEquals(TranRecord.fromFixedWidth(line).tranCardNum(), line.substring(cardPos, cardPos + 16));

        // INCLUDE COND=(TRAN-PROC-DT,GE,PARM-START-DATE,AND,TRAN-PROC-DT,LE,PARM-END-DATE)
        int procPos = Integer.parseInt(proc[1]) - 1;
        long included = fixture().stream()
                .map(l -> String.format("%-350s", l).substring(procPos, procPos + 10))
                .filter(d -> d.compareTo("2022-06-01") >= 0 && d.compareTo("2022-06-30") <= 0)
                .count();
        assertEquals(0, included, "dailytran has no TRAN-PROC-TS (blank) so nothing is selected");

        TranRecord f = TranRecord.fromFixedWidth(fixture().get(0));
        TranRecord posted = new TranRecord(f.tranId(), f.tranTypeCd(), f.tranCatCd(), f.tranSource(),
                f.tranDesc(), f.tranAmt(), f.tranMerchantId(), f.tranMerchantName(), f.tranMerchantCity(),
                f.tranMerchantZip(), f.tranCardNum(), f.tranOrigTs(), "2022-06-10 19:27:53.000000");
        String d = posted.toFixedWidth().substring(procPos, procPos + 10);
        assertTrue(d.compareTo("2022-06-01") >= 0 && d.compareTo("2022-06-30") <= 0);
    }

    private static String[] symname(TranReportJob job, String name) {
        return job.lines().stream().filter(l -> l.startsWith(name + ",")).findFirst()
                .orElseThrow().strip().split(",");
    }
}
