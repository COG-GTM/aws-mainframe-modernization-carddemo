package com.carddemo.programs.csutldtc;

import static org.junit.jupiter.api.Assertions.assertEquals;

import com.carddemo.programs.csutldtc.copybook.FeedbackCode;
import com.carddemo.programs.csutldtc.copybook.VString;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class LeCeedaysTest {
    private final LeCeedays le = new LeCeedays(Clock.fixed(Instant.parse("2026-01-01T00:00:00Z"), ZoneOffset.UTC), 80);

    private CeedaysResult run(String date, String pic) {
        return le.ceedays(VString.of(date), VString.of(pic));
    }

    @ParameterizedTest(name = "{0} -> Lilian {1}")
    @CsvSource({
        "1582-10-15,1",
        "1582-10-16,2",
        "1600-03-01,6348",
        "2000-01-01,152385",
        "9999-12-31,3074324",
    })
    void lilianDayNumbers(String date, int lilian) {
        CeedaysResult r = run(date, "YYYY-MM-DD");
        assertEquals(FeedbackCode.success(), r.feedbackCode());
        assertEquals(lilian, r.outputLillian());
    }

    @ParameterizedTest(name = "{0} / {1} -> {2}")
    @CsvSource(delimiter = '|', value = {
        "2000-02-29|YYYY-MM-DD|0",
        "1900-02-29|YYYY-MM-DD|2508",
        "2024-366 |YYYY-DDD  |0",
        "2023-366 |YYYY-DDD  |2508",
        "2023-000 |YYYY-DDD  |2508",
        "10-JUN-2022|DD-MMM-YYYY|0",
        "10-Jun-2022|DD-Mmm-YYYY|0",
        "10-JUX-2022|DD-MMM-YYYY|2517",
        "06/10/2022|MM/DD/YYYY|0",
        "06.10.2022|MM/DD/YYYY|2520",
        "2022-06-10X|YYYY-MM-DD|2520",
        "2022-06|YYYY-MM-DD|2507",
        "|YYYY-MM-DD|2507",
        "2022-06-10||2518",
        "2022-06-10|yyyy-mm-dd|2518",
        "2022-06-10|YYYY-YY-DD|2518",
        "2022-06-10|<JJJJ>-MM-DD|2518",
        "2022-06-1A|YYYY-MM-DD|2520",
        "1582-10-04|YYYY-MM-DD|2513",
    })
    void feedbackCodes(String date, String pic, int msgNo) {
        CeedaysResult r = run(date == null ? "" : date, pic == null ? "" : pic);
        if (msgNo == 0) {
            assertEquals(FeedbackCode.success(), r.feedbackCode());
        } else {
            assertEquals(FeedbackCode.ofCondition(3, msgNo), r.feedbackCode());
            assertEquals(0, r.outputLillian());
        }
    }

    @Test
    void twoDigitYearUsesEightyYearCenturyWindow() {
        // window for 2026 is 1946..2045
        assertEquals(run("1946-06-10", "YYYY-MM-DD").outputLillian(), run("460610", "YYMMDD").outputLillian());
        assertEquals(run("2045-06-10", "YYYY-MM-DD").outputLillian(), run("450610", "YYMMDD").outputLillian());
    }
}
