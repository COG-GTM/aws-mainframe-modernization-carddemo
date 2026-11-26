package com.carddemo.programs.cbact01c.gateway;

import com.carddemo.programs.cbact01c.copybook.CodatecnRec;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class CobdatftDateFormatterTest {
    private final CobdatftDateFormatter cobdatft = new CobdatftDateFormatter();

    private static CodatecnRec rec(String type, String in, String outType) {
        CodatecnRec r = new CodatecnRec();
        r.setCodatecnType(type);
        r.setCodatecnInpDate(in);
        r.setCodatecnOuttype(outType);
        return r;
    }

    @Test
    void validin2StripsSeparatorsAndLeavesTailUntouched() {
        CodatecnRec r = rec("2", "2025-05-20", "2");
        cobdatft.call(r);
        assertEquals("20250520" + " ".repeat(12), r.codatecn0utDate());
        assertEquals(" ".repeat(38), r.codatecnErrorMsg());
    }

    @Test
    void validin2DoesNotCheckSeparators() {
        CodatecnRec r = rec("2", "2025/05/20", "2");
        cobdatft.call(r);
        assertEquals("20250520", r.codatecn0utDate().substring(0, 8));
    }

    @Test
    void validin1InsertsDashes() {
        CodatecnRec r = rec("1", "20250520", "1");
        cobdatft.call(r);
        assertEquals("2025-05-20" + " ".repeat(10), r.codatecn0utDate());
    }

    @Test
    void validin1RejectsDashedInput() {
        CodatecnRec r = rec("1", "2025-05-20", "1");
        r.setCodatecn0utDate("PREVIOUS");
        cobdatft.call(r);
        assertEquals("INVALID INPUT", r.codatecnErrorMsg().trim());
        assertEquals("PREVIOUS", r.codatecn0utDate().trim());
    }

    @Test
    void mismatchedOutputTypesAreRejected() {
        CodatecnRec r1 = rec("1", "20250520", "2");
        cobdatft.call(r1);
        assertEquals("INVALID INPUT", r1.codatecnErrorMsg().trim());
        CodatecnRec r2 = rec("2", "2025-05-20", "1");
        cobdatft.call(r2);
        assertEquals("INVALID INPUT", r2.codatecnErrorMsg().trim());
    }

    @Test
    void unknownTypeIsRejected() {
        CodatecnRec r = rec("3", "2025-05-20", "2");
        cobdatft.call(r);
        assertEquals("INVALID INPUT" + " ".repeat(25), r.codatecnErrorMsg());
        assertEquals(" ".repeat(20), r.codatecn0utDate());
    }

    @Test
    void successDoesNotClearEarlierError() {
        CodatecnRec r = rec("3", "2025-05-20", "2");
        cobdatft.call(r);
        r.setCodatecnType("2");
        cobdatft.call(r);
        assertEquals("INVALID INPUT", r.codatecnErrorMsg().trim());
        assertEquals("20250520", r.codatecn0utDate().trim());
    }
}
