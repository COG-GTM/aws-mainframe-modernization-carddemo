package com.carddemo.programs.corpt00c;

import com.carddemo.programs.corpt00c.Csutldtc.Feedback;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.junit.jupiter.api.Assertions.assertEquals;

class CsutldtcTest {

    private final Csutldtc csutldtc = new Csutldtc();

    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {
            "2024-02-29|0000|0000", "2023-02-29|0003|2508", "2024-13-01|0003|2517", "2024-00-10|0003|2517",
            "2024-04-31|0003|2508", "2024-01-00|0003|2508", "1582-10-14|0003|2513", "1582-10-15|0000|0000",
            "0024-01-01|0003|2513", "abcd-01-01|0003|2520", "'          '|0003|2507",
    })
    void feedbackCodes(String date, String sev, String msgNo) {
        CsutldtcResult r = csutldtc.validate(date, "YYYY-MM-DD");
        assertEquals(sev, r.sevCd());
        assertEquals(msgNo, r.msgNum());
        assertEquals(Integer.parseInt(sev), Csutldtc.returnCode(r));
    }

    @Test
    void resultLayoutMatchesWsMessage() {
        CsutldtcResult r = csutldtc.validate("2024-02-29", "YYYY-MM-DD");
        assertEquals(80, r.raw().length());
        assertEquals("0000Mesg Code: 0000 Date is valid   TstDate: 2024-02-29 Mask used:YYYY-MM-DD    ", r.raw());
    }

    @Test
    void lilianDay() {
        assertEquals(1, Csutldtc.ceedays("1582-10-15", "YYYY-MM-DD").lilian());
        assertEquals(Feedback.FC_BAD_PIC_STRING, Csutldtc.ceedays("2024-01-01", "YYYY-XX-DD").feedback());
    }
}
