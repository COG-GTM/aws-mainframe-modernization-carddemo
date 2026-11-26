package com.carddemo.programs.paudbunl.copybook;

import com.carddemo.programs.paudbunl.TestSegments;
import com.carddemo.programs.paudbunl.support.CobolData;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class CopybookTest {

    @Test
    void summaryIs100BytesAndRoundTrips() {
        PendingAuthSummary s = TestSegments.summary(42);
        byte[] b = s.toBytes(CobolData.EBCDIC);
        assertEquals(100, b.length);
        assertEquals(s, PendingAuthSummary.fromBytes(b, CobolData.EBCDIC));
        assertEquals("00000000042C", CobolData.hex(b, 0, 6));
        // FILLER X(34) at offset 66 written as spaces
        assertEquals(" ".repeat(34), CobolData.decodeAlpha(b, 66, 34, CobolData.EBCDIC));
    }

    @Test
    void summaryAmountsAreComp3WithScale2() {
        byte[] b = TestSegments.summaryBytes(1);
        assertEquals("00000012345D", CobolData.hex(b, 38, 6)); // PA-CREDIT-BALANCE -123.45
        assertEquals("0003", CobolData.hex(b, 50, 2));         // PA-APPROVED-AUTH-CNT COMP
        PendingAuthSummary s = PendingAuthSummary.fromBytes(b, CobolData.EBCDIC);
        assertEquals(2, s.creditBalance().scale());
        assertEquals(new BigDecimal("250.75"), s.approvedAuthAmt());
    }

    @Test
    void summaryAccountStatusOccurs5() {
        assertThrows(IllegalArgumentException.class, () -> new PendingAuthSummary(BigDecimal.ONE, 1, "A",
                List.of("01"), BigDecimal.ZERO, BigDecimal.ZERO, BigDecimal.ZERO, BigDecimal.ZERO, 0, 0,
                BigDecimal.ZERO, BigDecimal.ZERO));
    }

    @Test
    void acctIdNumericCheckOnRawBytes() {
        byte[] b = TestSegments.summaryBytes(7);
        assertTrue(PendingAuthSummary.isAcctIdNumeric(b));
        CobolData.encodeAlpha("", b, 0, 6, CobolData.EBCDIC);
        assertFalse(PendingAuthSummary.isAcctIdNumeric(b));
    }

    @Test
    void detailsIs200BytesAndRoundTrips() {
        PendingAuthDetails d = TestSegments.details(73669, 899999999L, "P");
        byte[] b = d.toBytes(CobolData.EBCDIC);
        assertEquals(200, b.length);
        assertEquals(d, PendingAuthDetails.fromBytes(b, CobolData.EBCDIC));
        assertEquals("73669C", CobolData.hex(b, 0, 3));
        assertEquals("899999999C", CobolData.hex(b, 3, 5));
        assertEquals("0000000001234C", CobolData.hex(b, 74, 7));
        assertEquals(" ".repeat(17), CobolData.decodeAlpha(b, PendingAuthDetails.FILLER_OFFSET, 17, CobolData.EBCDIC));
    }

    @Test
    void detailsLevel88Conditions() {
        PendingAuthDetails d = TestSegments.details(1, 1, "P");
        assertTrue(d.isAuthApproved());
        assertTrue(d.isMatchPending());
        assertFalse(d.isMatchedWithTran());
        assertTrue(TestSegments.details(1, 1, "D").isMatchAuthDeclined());
        assertTrue(TestSegments.details(1, 1, "E").isMatchPendingExpired());
        assertTrue(TestSegments.details(1, 1, "M").isMatchedWithTran());
        assertEquals(PendingAuthDetails.MatchStatus.MATCHED_WITH_TRAN, PendingAuthDetails.MatchStatus.fromCode("M").orElseThrow());
        assertTrue(PendingAuthDetails.MatchStatus.fromCode("X").isEmpty());
        assertEquals(PendingAuthDetails.AuthFraud.CONFIRMED, PendingAuthDetails.AuthFraud.fromCode("F").orElseThrow());
        assertEquals(PendingAuthDetails.AuthFraud.REMOVED, PendingAuthDetails.AuthFraud.fromCode("R").orElseThrow());
        assertFalse(d.isFraudConfirmed());
        assertFalse(d.isFraudRemoved());
    }

    @Test
    void segmentLengthIsEnforced() {
        assertThrows(IllegalArgumentException.class, () -> PendingAuthSummary.fromBytes(new byte[99], CobolData.EBCDIC));
        assertThrows(IllegalArgumentException.class, () -> PendingAuthDetails.fromBytes(new byte[201], CobolData.EBCDIC));
    }

    @Test
    void imsFunctionCodesAreFourBytes() {
        assertEquals("GN  ", ImsFunctionCodes.GN.code());
        assertEquals("GNP ", ImsFunctionCodes.GNP.code());
        assertEquals("GHNP", ImsFunctionCodes.GHNP.code());
        assertEquals(4, ImsFunctionCodes.PARMCOUNT);
        for (ImsFunctionCodes f : ImsFunctionCodes.values()) {
            assertEquals(4, f.code().length());
        }
    }

    @Test
    void pcbMaskLayoutAndStatusInitialize() {
        PautbPcb pcb = new PautbPcb("DBPAUTP0", "GOTP", 2);
        assertEquals(8 + 2 + 2 + 4 + 4 + 8 + 4 + 4 + 255, PautbPcb.LENGTH);
        assertEquals("DBPAUTP0", pcb.dbdName());
        assertEquals("GOTP", pcb.procopt());
        pcb.pcbStatus("GB");
        assertFalse(pcb.isStatusSpaces());
        pcb.initializeStatus();
        assertEquals("  ", pcb.pcbStatus());
        pcb.keyFeedback(new byte[] {0x00, 0x1C});
        assertEquals(2, pcb.keyfbName());
        assertEquals(255, pcb.keyfb().length);
        assertArrayEquals(new byte[] {0x00, 0x1C}, java.util.Arrays.copyOf(pcb.keyfb(), 2));
        assertEquals(255, pcb.keyfbDisplay(CobolData.EBCDIC).length());
    }
}
