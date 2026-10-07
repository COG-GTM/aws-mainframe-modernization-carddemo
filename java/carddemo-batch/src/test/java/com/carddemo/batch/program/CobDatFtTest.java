package com.carddemo.batch.program;

import com.carddemo.batch.record.CodatecnRec;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/** The COBDATFT date reformatting as CBACT01C calls it (type 2 -> out-type 2). */
class CobDatFtTest {

    @Test
    void yyyyMmDdToYyyymmddLeavesTrailingBytesUntouched() {
        CodatecnRec rec = new CodatecnRec();
        rec.setCodatecnInpDate("2025-05-20");
        rec.setCodatecnType(CodatecnRec.YYYY_MM_DD);
        rec.setCodatecnOuttype(CodatecnRec.YYYY_MM_DD);
        CobDatFt.call(rec);
        assertEquals("20250520            ", rec.codatecnOutDate());
        assertEquals("20250520  ", rec.codatecnOutDate().substring(0, 10), "MOVE X(20) TO X(10)");
        assertEquals("                                      ", rec.codatecnErrorMsg());
    }

    @Test
    void reusedParameterBlockKeepsPreviousBytesBeyondPositionEight() {
        CodatecnRec rec = new CodatecnRec();
        rec.setCodatecnOutDate(1, "ABCDEFGHIJKLMNOPQRST");
        rec.setCodatecnInpDate("2024-08-11");
        rec.setCodatecnType("2");
        rec.setCodatecnOuttype("2");
        CobDatFt.call(rec);
        assertEquals("20240811IJKLMNOPQRST", rec.codatecnOutDate());
    }

    @Test
    void yyyymmddToYyyyMmDdInsertsHyphens() {
        CodatecnRec rec = new CodatecnRec();
        rec.setCodatecnInpDate("20250520");
        rec.setCodatecnType(CodatecnRec.YYYYMMDD);
        rec.setCodatecnOuttype(CodatecnRec.YYYYMMDD);
        CobDatFt.call(rec);
        assertEquals("2025-05-20          ", rec.codatecnOutDate());
    }

    @Test
    void invalidCombinationsSetErrorMessageAndLeaveOutputAlone() {
        for (String[] c : new String[][] {{"2", "1", "2025-05-20"}, {"1", "2", "20250520"}, {"1", "1", "2025-05-20"}, {"9", "2", "2025-05-20"}}) {
            CodatecnRec rec = new CodatecnRec();
            rec.setCodatecnType(c[0]);
            rec.setCodatecnOuttype(c[1]);
            rec.setCodatecnInpDate(c[2]);
            CobDatFt.call(rec);
            assertEquals("INVALID INPUT                         ", rec.codatecnErrorMsg(), "type " + c[0] + " outtype " + c[1]);
            assertEquals("                    ", rec.codatecnOutDate(), "type " + c[0] + " outtype " + c[1]);
        }
    }
}
