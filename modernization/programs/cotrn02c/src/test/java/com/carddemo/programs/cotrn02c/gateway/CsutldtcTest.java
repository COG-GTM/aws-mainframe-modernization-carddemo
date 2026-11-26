package com.carddemo.programs.cotrn02c.gateway;

import com.carddemo.programs.cotrn02c.copybook.CsutldtcParm;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import static org.junit.jupiter.api.Assertions.assertEquals;

class CsutldtcTest {

    private final Csutldtc csutldtc = new Csutldtc();

    @ParameterizedTest
    @CsvSource({
        "2022-06-10, 0000, 0000, Date is valid",
        "2024-02-29, 0000, 0000, Date is valid",
        "2023-02-29, 0003, 2508, Datevalue error",
        "2022-13-01, 0003, 2517, Invalid month",
        "1582-10-14, 0003, 2513, Unsupp. Range",
        "1582-10-15, 0000, 0000, Date is valid",
        "1582-10-00, 0003, 2508, Datevalue error",
        "1500-02-30, 0003, 2508, Datevalue error",
        "20AB-01-01, 0003, 2520, Nonnumeric data"
    })
    void feedbackCodes(String date, String sev, String msg, String text) {
        CsutldtcParm p = csutldtc.validate(date, "YYYY-MM-DD");
        assertEquals(sev, p.resultSevCd());
        assertEquals(msg, p.resultMsgNum());
        assertEquals(text, p.resultMsg().substring(1, 16).trim());
    }

    @Test
    void resultMessageLayoutIs80Bytes() {
        CsutldtcParm p = csutldtc.validate("2022-06-10", "YYYY-MM-DD");
        assertEquals("0000Mesg Code: 0000 Date is valid   TstDate: 2022-06-10 Mask used:YYYY-MM-DD    ", p.result());
        assertEquals(80, p.result().length());
    }

    @Test
    void unsupportedPicture() {
        assertEquals("2518", csutldtc.validate("2022-06-10", "MM/DD/YYYY").resultMsgNum());
    }
}
