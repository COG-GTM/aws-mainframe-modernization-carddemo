package com.carddemo.programs.csutldtc;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.List;
import org.junit.jupiter.api.Test;

/**
 * Feeds TRAN-ORIG-TS (CVTRA06Y, offset 278, first 10 bytes) of every record in app/data/ASCII/dailytran.txt
 * through CSUTLDTC with 'YYYY-MM-DD', the way COTRN02C validates a transaction origination date.
 */
class DailyTranFixtureTest {
    private static final int TRAN_ORIG_TS_OFFSET = 278;

    private static List<String> origDates() throws IOException {
        try (InputStream in = DailyTranFixtureTest.class.getResourceAsStream("/fixtures/dailytran.txt")) {
            return new String(in.readAllBytes(), StandardCharsets.US_ASCII).lines()
                    .filter(l -> l.length() >= TRAN_ORIG_TS_OFFSET + 10)
                    .map(l -> l.substring(TRAN_ORIG_TS_OFFSET, TRAN_ORIG_TS_OFFSET + 10))
                    .toList();
        }
    }

    @Test
    void everyOriginationDateInSampleDataIsValid() throws IOException {
        List<String> dates = origDates();
        assertEquals(300, dates.size());
        Csutldtc program = new Csutldtc();
        for (String d : dates) {
            CsutldtcResult r = program.call(d, "YYYY-MM-DD");
            assertEquals("Date is valid  ", r.resultText(), d);
            assertEquals(0, r.returnCode(), d);
        }
    }

    @Test
    void corruptedSampleDatesAreRejected() throws IOException {
        Csutldtc program = new Csutldtc();
        for (String d : origDates().subList(0, 20)) {
            CsutldtcResult r = program.call(d.substring(0, 8) + "32", "YYYY-MM-DD");
            assertEquals("Datevalue error", r.resultText());
            assertFalse(r.toParm(d, "YYYY-MM-DD").acceptedByCallers());
            assertTrue(program.call(d, "YYYY-MM-DD").toParm(d, "YYYY-MM-DD").acceptedByCallers());
        }
    }
}
