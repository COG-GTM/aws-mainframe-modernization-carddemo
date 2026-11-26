package com.carddemo.programs.cbact01c.copybook;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CodatecnRecTest {

    @Test
    void parameterBlockIs80BytesAndStartsAsSpaces() {
        CodatecnRec rec = new CodatecnRec();
        assertEquals(80, CodatecnRec.LENGTH);
        assertEquals(" ".repeat(80), rec.asText());
    }

    @Test
    void levelEightyEightConditions() {
        CodatecnRec rec = new CodatecnRec();
        rec.setCodatecnType("1");
        rec.setCodatecnOuttype("2");
        assertTrue(rec.isYyyymmddIn());
        assertFalse(rec.isYyyyMmDdIn());
        assertTrue(rec.isYyyymmddOp());
        assertFalse(rec.isYyyyMmDdOp());
        assertEquals(CodatecnRec.InputType.YYYY_MM_DD_IN, CodatecnRec.InputType.of("2").orElseThrow());
        assertEquals(CodatecnRec.OutputType.YYYY_MM_DD_OP, CodatecnRec.OutputType.of("1").orElseThrow());
        assertTrue(CodatecnRec.InputType.of("3").isEmpty());
    }

    @Test
    void redefinesViews() {
        CodatecnRec rec = new CodatecnRec();
        rec.setCodatecnInpDate("2025-05-20");
        assertEquals("2025", rec.codatecn1OYyyy());
        assertEquals("-", rec.codatecn1IS1());
        assertEquals("05", rec.codatecn2InpMm());
        assertEquals("20", rec.codatecn2Yy());
        assertEquals(" ".repeat(10), rec.codatecn2Fil());
        rec.setCodatecn0utDate("20250520");
        assertEquals("2025", rec.codatecn2OYyyy());
        assertEquals("05", rec.codatecn2OMm());
        assertEquals("20", rec.codatecn2ODd());
    }

    @Test
    void alphanumericMovesPadAndTruncate() {
        CodatecnRec rec = new CodatecnRec();
        rec.setCodatecnErrorMsg("INVALID INPUT");
        assertEquals(38, rec.codatecnErrorMsg().length());
        rec.setCodatecnInpDate("X".repeat(25));
        assertEquals("X".repeat(20), rec.codatecnInpDate());
    }
}
