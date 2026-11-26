package com.carddemo.programs.paudblod;

import static org.junit.jupiter.api.Assertions.*;

import com.carddemo.programs.paudblod.copybook.ImsFuncs;
import com.carddemo.programs.paudblod.copybook.PautbPcb;
import com.carddemo.programs.paudblod.copybook.PendingAuthDetails;
import com.carddemo.programs.paudblod.copybook.PendingAuthSummary;
import com.carddemo.programs.paudblod.ims.Ssa;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.util.Arrays;

class CopybookTest {

    private static final Charset CS = Paudblod.EBCDIC;

    @Test
    void cipausmyIs100BytesAndRoundTrips() {
        PendingAuthSummary s = TestData.summary(42);
        byte[] b = s.toBytes(CS);
        assertEquals(100, b.length);
        assertEquals(s, PendingAuthSummary.fromBytes(b, CS));
        assertArrayEquals(new byte[] {0, 0, 0, 0, 0x04, 0x2C}, Arrays.copyOf(b, 6));
    }

    @Test
    void cipaudtyIs200BytesAndRoundTrips() {
        PendingAuthDetails d = TestData.details(998747444L, "TXN000000000001");
        byte[] b = d.toBytes(CS);
        assertEquals(200, b.length);
        assertEquals(d, PendingAuthDetails.fromBytes(b, CS));
        assertEquals(new BigDecimal("42.17"), PendingAuthDetails.fromBytes(b, CS).paTransactionAmt());
    }

    @Test
    void cipaudty88LevelsMapToPredicatesAndEnums() {
        PendingAuthDetails d = TestData.details(1, "T");
        assertTrue(d.isPaAuthApproved());
        assertTrue(d.isPaMatchPending());
        assertFalse(d.isPaMatchAuthDeclined());
        assertFalse(d.isPaMatchPendingExpired());
        assertFalse(d.isPaMatchedWithTran());
        assertFalse(d.isPaFraudConfirmed());
        assertFalse(d.isPaFraudRemoved());
        assertEquals(PendingAuthDetails.MatchStatus.PA_MATCHED_WITH_TRAN, PendingAuthDetails.MatchStatus.of("M").orElseThrow());
        assertEquals(PendingAuthDetails.MatchStatus.PA_MATCH_PENDING_EXPIRED, PendingAuthDetails.MatchStatus.of("E").orElseThrow());
        assertEquals(PendingAuthDetails.AuthFraud.PA_FRAUD_CONFIRMED, PendingAuthDetails.AuthFraud.of("F").orElseThrow());
        assertEquals(PendingAuthDetails.AuthFraud.PA_FRAUD_REMOVED, PendingAuthDetails.AuthFraud.of("R").orElseThrow());
        assertTrue(PendingAuthDetails.AuthFraud.of(" ").isEmpty());
    }

    @Test
    void cipausmyRequiresFiveAccountStatusOccurrences() {
        assertThrows(IllegalArgumentException.class, () -> new PendingAuthSummary(BigDecimal.ONE, 1, "A",
                java.util.List.of("AC"), BigDecimal.ZERO, BigDecimal.ZERO, BigDecimal.ZERO, BigDecimal.ZERO,
                0, 0, BigDecimal.ZERO, BigDecimal.ZERO, ""));
    }

    @Test
    void imsfuncsCodesAreFourBytes() {
        for (ImsFuncs f : ImsFuncs.values()) {
            assertEquals(4, f.code().length());
        }
        assertEquals("GU  ", ImsFuncs.FUNC_GU.code());
        assertEquals("ISRT", ImsFuncs.FUNC_ISRT.code());
        assertEquals(4, ImsFuncs.PARMCOUNT);
    }

    @Test
    void pautbpcbFieldsArePaddedToPicSize() {
        PautbPcb pcb = new PautbPcb("DBPAUTP0", "A", 2);
        assertEquals("A   ", pcb.getPautPcbProcopt());
        assertTrue(pcb.isStatusSpaces());
        pcb.setPautPcbStatus("I");
        assertEquals("I ", pcb.getPautPcbStatus());
        pcb.setPautKeyfb(new byte[] {1, 2}, (byte) 0x40);
        assertEquals(255, pcb.getPautKeyfb().length);
        assertEquals(2, pcb.getPautKeyfbName());
        assertEquals(0x40, pcb.getPautKeyfb()[2] & 0xFF);
    }

    @Test
    void rootQualSsaMatchesWorkingStorageLayout() {
        byte[] b = new Ssa.RootQualSsa(new BigDecimal("12345")).toBytes(CS);
        assertEquals(26, b.length);
        assertEquals("PAUTSUM0(ACCNTID EQ", new String(b, 0, 19, CS));
        assertArrayEquals(new byte[] {0, 0, 0, 0x12, 0x34, 0x5C}, Arrays.copyOfRange(b, 19, 25));
        assertEquals(")", new String(b, 25, 1, CS));
        assertEquals("PAUTSUM0 ", new String(new Ssa.RootUnqualSsa().toBytes(CS), CS));
        assertEquals("PAUTDTL1 ", new String(new Ssa.ChildUnqualSsa().toBytes(CS), CS));
    }
}
