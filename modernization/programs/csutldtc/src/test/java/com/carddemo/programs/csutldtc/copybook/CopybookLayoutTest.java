package com.carddemo.programs.csutldtc.copybook;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.HexFormat;
import java.util.Optional;
import org.junit.jupiter.api.Test;

class CopybookLayoutTest {
    @Test
    void feedbackTokensMatch88Levels() {
        assertEquals(Optional.of(FeedbackCondition.FC_INVALID_DATE), FeedbackCondition.of(FeedbackCode.success()));
        int[] msgs = {2507, 2508, 2509, 2513, 2517, 2518, 2520, 2521};
        FeedbackCondition[] conds = {
            FeedbackCondition.FC_INSUFFICIENT_DATA, FeedbackCondition.FC_BAD_DATE_VALUE,
            FeedbackCondition.FC_INVALID_ERA, FeedbackCondition.FC_UNSUPP_RANGE,
            FeedbackCondition.FC_INVALID_MONTH, FeedbackCondition.FC_BAD_PIC_STRING,
            FeedbackCondition.FC_NON_NUMERIC_DATA, FeedbackCondition.FC_YEAR_IN_ERA_ZERO
        };
        for (int i = 0; i < msgs.length; i++) {
            assertEquals(Optional.of(conds[i]), FeedbackCondition.of(FeedbackCode.ofCondition(3, msgs[i])));
        }
        assertEquals(Optional.empty(), FeedbackCondition.of(FeedbackCode.ofCondition(3, 2510)));
    }

    @Test
    void feedbackCodeLayoutAndRedefines() {
        FeedbackCode fc = FeedbackCode.ofCondition(3, 2513);
        assertEquals("000309d159c3c5c500000000", HexFormat.of().formatHex(fc.toBytes()));
        assertEquals(3, fc.classCode());
        assertEquals(2513, fc.causeCode());
        assertEquals(0x59, fc.caseSevCtl());
        assertArrayEquals(FeedbackCode.CEE_FACILITY_EBCDIC, fc.facilityId());
        assertEquals(0, fc.iSInfo());
        assertEquals(fc, FeedbackCode.fromBytes(fc.toBytes()));
        assertThrows(IllegalArgumentException.class, () -> FeedbackCode.fromBytes(new byte[8]));
    }

    @Test
    void wsMessageIsEightyBytesAfterInitialize() {
        WsMessage m = new WsMessage();
        m.initialize();
        String expected = "    " + "Mesg Code: " + "    " + " " + " ".repeat(15) + " " + "TstDate: "
                + " ".repeat(10) + " " + "Mask used:" + " ".repeat(10) + " " + "   ";
        assertEquals(expected, m.toRecord());
        assertEquals(WsMessage.LENGTH, m.toRecord().length());
        m.setResult("Unsupp. Range  overflow");
        assertEquals("Unsupp. Range  ", m.result());
    }

    @Test
    void vstringBoundsAndPadding() {
        assertEquals("2023      ", new VString((short) 10, "2023").text());
        assertThrows(IllegalArgumentException.class, () -> new VString((short) 257, ""));
        assertArrayEquals(new byte[] {0, 2, 'A', 'B'}, VString.of("AB").groupBytes());
    }

    @Test
    void csutldtcParmRecordLayout() {
        String result = "0003Mesg Code: 2513 Unsupp. Range   TstDate: xxxxxxxxxx Mask used:YYYY-MM-DD    ";
        CsutldtcParm p = CsutldtcParm.fromRecord("1500-01-01YYYY-MM-DD" + result);
        assertEquals(CsutldtcParm.LENGTH, p.toRecord().length());
        assertEquals("0003", p.resultSevCd());
        assertEquals("2513", p.resultMsgNum());
        assertEquals(61, p.resultMsg().length());
        assertTrue(p.acceptedByCallers());
        assertFalse(new CsutldtcParm("x", "y", "0003Mesg Code: 2508").acceptedByCallers());
    }
}
