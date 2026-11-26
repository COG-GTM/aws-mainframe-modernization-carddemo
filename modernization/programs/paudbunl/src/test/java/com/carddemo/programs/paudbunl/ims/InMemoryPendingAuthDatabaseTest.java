package com.carddemo.programs.paudbunl.ims;

import com.carddemo.programs.paudbunl.TestSegments;
import com.carddemo.programs.paudbunl.copybook.ImsFunctionCodes;
import com.carddemo.programs.paudbunl.copybook.PautbPcb;
import com.carddemo.programs.paudbunl.support.CobolData;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class InMemoryPendingAuthDatabaseTest {

    @Test
    void gnWalksRootsThenReturnsGb() {
        InMemoryPendingAuthDatabase db = new InMemoryPendingAuthDatabase()
                .addRoot(TestSegments.summaryBytes(1)).addChild(TestSegments.detailsBytes(1, 1))
                .addRoot(TestSegments.summaryBytes(2));
        PautbPcb pcb = new PautbPcb();
        byte[] io = new byte[100];
        db.cbltdli(ImsFunctionCodes.GN, pcb, io, Ssa.ROOT_UNQUAL_SSA);
        assertTrue(pcb.isStatusSpaces());
        assertEquals("PAUTSUM0", pcb.segName());
        assertEquals("01", pcb.segLevel());
        assertEquals(6, pcb.keyfbName());
        assertArrayEquals(TestSegments.summaryBytes(1), io);
        // GN with root SSA skips the remaining children
        db.cbltdli(ImsFunctionCodes.GN, pcb, io, Ssa.ROOT_UNQUAL_SSA);
        assertArrayEquals(TestSegments.summaryBytes(2), io);
        db.cbltdli(ImsFunctionCodes.GN, pcb, io, Ssa.ROOT_UNQUAL_SSA);
        assertEquals("GB", pcb.pcbStatus());
        assertArrayEquals(TestSegments.summaryBytes(2), io, "I/O area untouched on GB");
        db.cbltdli(ImsFunctionCodes.GN, pcb, io, Ssa.ROOT_UNQUAL_SSA);
        assertEquals("GB", pcb.pcbStatus());
    }

    @Test
    void gnpReturnsChildrenOfCurrentRootThenGe() {
        InMemoryPendingAuthDatabase db = new InMemoryPendingAuthDatabase()
                .addRoot(TestSegments.summaryBytes(9))
                .addChild(TestSegments.detailsBytes(11, 22))
                .addChild(TestSegments.detailsBytes(33, 44));
        PautbPcb pcb = new PautbPcb();
        db.cbltdli(ImsFunctionCodes.GN, pcb, new byte[100], Ssa.ROOT_UNQUAL_SSA);
        byte[] io = new byte[200];
        db.cbltdli(ImsFunctionCodes.GNP, pcb, io, Ssa.CHILD_UNQUAL_SSA);
        assertTrue(pcb.isStatusSpaces());
        assertEquals("02", pcb.segLevel());
        assertEquals("PAUTDTL1", pcb.segName());
        assertEquals(14, pcb.keyfbName());
        assertEquals("00000000009C00011C000000022C", CobolData.hex(pcb.keyfb(), 0, 14));
        db.cbltdli(ImsFunctionCodes.GNP, pcb, io, Ssa.CHILD_UNQUAL_SSA);
        assertArrayEquals(TestSegments.detailsBytes(33, 44), io);
        db.cbltdli(ImsFunctionCodes.GNP, pcb, io, Ssa.CHILD_UNQUAL_SSA);
        assertEquals("GE", pcb.pcbStatus());
    }

    @Test
    void gnpWithoutParentIsGpAndUnsupportedCallsAreAd() {
        InMemoryPendingAuthDatabase db = new InMemoryPendingAuthDatabase().addRoot(TestSegments.summaryBytes(1));
        PautbPcb pcb = new PautbPcb();
        db.cbltdli(ImsFunctionCodes.GNP, pcb, new byte[200], Ssa.CHILD_UNQUAL_SSA);
        assertEquals("GP", pcb.pcbStatus());
        db.cbltdli(ImsFunctionCodes.ISRT, pcb, new byte[200], Ssa.CHILD_UNQUAL_SSA);
        assertEquals("AD", pcb.pcbStatus());
        db.cbltdli(ImsFunctionCodes.GN, pcb, new byte[200], Ssa.CHILD_UNQUAL_SSA);
        assertEquals("AD", pcb.pcbStatus());
    }

    @Test
    void childNeedsParentAndSegmentsNeedExactLength() {
        InMemoryPendingAuthDatabase db = new InMemoryPendingAuthDatabase();
        assertThrows(IllegalStateException.class, () -> db.addChild(new byte[200]));
        assertThrows(IllegalArgumentException.class, () -> db.addRoot(new byte[101]));
    }

    @Test
    void ssaTextIsNameAndBlank() {
        assertEquals("PAUTSUM0 ", Ssa.ROOT_UNQUAL_SSA.text());
        assertEquals("PAUTDTL1 ", Ssa.CHILD_UNQUAL_SSA.text());
        assertTrue(ImsStatus.isStatusOk("FW"));
        assertTrue(ImsStatus.isRetryCondition("TE"));
    }
}
