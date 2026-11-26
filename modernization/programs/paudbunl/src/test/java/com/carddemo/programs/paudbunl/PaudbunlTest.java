package com.carddemo.programs.paudbunl;

import com.carddemo.programs.paudbunl.copybook.ImsFunctionCodes;
import com.carddemo.programs.paudbunl.copybook.PautbPcb;
import com.carddemo.programs.paudbunl.ims.DliGateway;
import com.carddemo.programs.paudbunl.ims.InMemoryPendingAuthDatabase;
import com.carddemo.programs.paudbunl.io.InMemoryRecordWriter;
import com.carddemo.programs.paudbunl.support.CobolData;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class PaudbunlTest {

    private static final Clock CLOCK = Clock.fixed(Instant.parse("2026-11-26T06:30:00Z"), ZoneOffset.UTC);

    private final List<String> sysout = new ArrayList<>();
    private final InMemoryRecordWriter opfile1 = new InMemoryRecordWriter(100);
    private final InMemoryRecordWriter opfile2 = new InMemoryRecordWriter(206);

    private PaudbunlResult run(DliGateway dli) {
        return new Paudbunl(dli, Paudbunl.pautbunlPcb(), opfile1, opfile2, CLOCK, sysout::add, CobolData.EBCDIC).run();
    }

    /** Overrides the PCB status on the n-th call of the given function (1-based). */
    private static DliGateway injectStatus(DliGateway delegate, ImsFunctionCodes fn, int nth, String status) {
        AtomicInteger calls = new AtomicInteger();
        return (f, pcb, io, ssa) -> {
            if (f == fn && calls.incrementAndGet() == nth) {
                pcb.pcbStatus(status);
                pcb.keyFeedback(new byte[] {(byte) 0xC1, (byte) 0xC2});
                return;
            }
            delegate.cbltdli(f, pcb, io, ssa);
        };
    }

    private static InMemoryPendingAuthDatabase twoAccounts() {
        return new InMemoryPendingAuthDatabase()
                .addRoot(TestSegments.summaryBytes(1))
                .addChild(TestSegments.detailsBytes(73669, 899999999L))
                .addChild(TestSegments.detailsBytes(73668, 899999998L))
                .addRoot(TestSegments.summaryBytes(5));
    }

    @Test
    void initializeDisplaysBannerWithYymmddDate() {
        PaudbunlResult r = run(new InMemoryPendingAuthDatabase());
        assertEquals(List.of("STARTING PROGRAM PAUDBUNL::", "*-------------------------------------*",
                "TODAYS DATE            :261126", " "), sysout.subList(0, 4));
        assertEquals("261126", r.currentDate());
        assertEquals("26330", r.currentYyddd());
    }

    @Test
    void emptyDatabaseEndsOnGbWithRc0AndEmptyFiles() {
        PaudbunlResult r = run(new InMemoryPendingAuthDatabase());
        assertEquals(0, r.returnCode());
        assertTrue(r.endOfAuthdb());
        assertEquals(0, r.wsNoSumryRead());
        assertTrue(opfile1.records().isEmpty());
        assertTrue(opfile2.records().isEmpty());
        assertEquals("CLOSING THE FILE", sysout.get(sysout.size() - 1));
        assertFalse(opfile1.isOpen());
        assertFalse(opfile2.isOpen());
    }

    @Test
    void unloadsRootsToOutfil1AndChildrenWithParentKeyToOutfil2() {
        PaudbunlResult r = run(twoAccounts());
        assertEquals(0, r.returnCode());
        assertEquals(2, opfile1.records().size());
        assertArrayEquals(TestSegments.summaryBytes(1), opfile1.records().get(0));
        assertArrayEquals(TestSegments.summaryBytes(5), opfile1.records().get(1));

        assertEquals(2, opfile2.records().size());
        for (byte[] rec : opfile2.records()) {
            assertEquals(206, rec.length);
            assertEquals("00000000001C", CobolData.hex(rec, 0, 6));
        }
        assertArrayEquals(TestSegments.detailsBytes(73669, 899999999L), Opfil2Rec.childSegRecOf(opfile2.records().get(0)));
        assertArrayEquals(TestSegments.detailsBytes(73668, 899999998L), Opfil2Rec.childSegRecOf(opfile2.records().get(1)));
        assertEquals(BigDecimal.ONE, Opfil2Rec.rootSegKeyOf(opfile2.records().get(1)));
    }

    @Test
    void countersIncrementForRootsAndChildrenAlike() {
        PaudbunlResult r = run(twoAccounts());
        assertEquals(4, r.wsNoSumryRead());
        assertEquals(4, r.wsAuthSmryProcCnt());
        assertEquals(2, r.rootRecordsWritten());
        assertEquals(2, r.childRecordsWritten());
        assertTrue(r.moreAuths());
    }

    @Test
    void geOnEachRootDisplaysChildFlagAndCloses() {
        run(twoAccounts());
        assertEquals(2, sysout.stream().filter("CHILD SEG FLAG GE : Y"::equals).count());
        assertEquals("CLOSING THE FILE", sysout.get(sysout.size() - 1));
        assertEquals(1, opfile1.closeCount());
        assertEquals(1, opfile2.closeCount());
    }

    @Test
    void nonNumericAcctIdSkipsRootAndItsChildrenButStillCounts() {
        byte[] badRoot = TestSegments.summaryBytes(3);
        CobolData.encodeAlpha("", badRoot, 0, 6, CobolData.EBCDIC);
        InMemoryPendingAuthDatabase db = new InMemoryPendingAuthDatabase()
                .addRoot(badRoot).addChild(TestSegments.detailsBytes(1, 1))
                .addRoot(TestSegments.summaryBytes(4)).addChild(TestSegments.detailsBytes(2, 2));
        PaudbunlResult r = run(db);
        assertEquals(0, r.returnCode());
        assertEquals(1, opfile1.records().size());
        assertArrayEquals(TestSegments.summaryBytes(4), opfile1.records().get(0));
        assertEquals(1, opfile2.records().size());
        assertEquals("00000000004C", CobolData.hex(opfile2.records().get(0), 0, 6));
        assertEquals(3, r.wsNoSumryRead());
        assertEquals(1, sysout.stream().filter("CHILD SEG FLAG GE : Y"::equals).count());
    }

    @Test
    void gnFailureDisplaysStatusAndKeyFeedbackThenAbendsRc16() {
        PaudbunlResult r = run(injectStatus(twoAccounts(), ImsFunctionCodes.GN, 2, "AI"));
        assertEquals(16, r.returnCode());
        assertTrue(r.abended());
        assertTrue(sysout.contains("AUTH SUM  GN FAILED  :AI"));
        String kfb = sysout.stream().filter(s -> s.startsWith("KEY FEEDBACK AREA    :")).findFirst().orElseThrow();
        assertEquals("KEY FEEDBACK AREA    :".length() + 255, kfb.length());
        assertTrue(kfb.startsWith("KEY FEEDBACK AREA    :AB"));
        assertEquals("IMSUNLOD ABENDING ...", sysout.get(sysout.size() - 1));
        assertFalse(sysout.contains("CLOSING THE FILE"));
        // records written before the abend are kept and the files are closed at run-unit end
        assertEquals(1, opfile1.records().size());
        assertEquals(2, opfile2.records().size());
        assertFalse(opfile1.isOpen());
        assertFalse(opfile2.isOpen());
    }

    @Test
    void fwStatusOnGnIsNotTreatedAsSuccess() {
        PaudbunlResult r = run(injectStatus(twoAccounts(), ImsFunctionCodes.GN, 1, "FW"));
        assertEquals(16, r.returnCode());
        assertTrue(sysout.contains("AUTH SUM  GN FAILED  :FW"));
        assertTrue(opfile1.records().isEmpty());
    }

    @Test
    void gnpFailureDisplaysStatusAndAbendsRc16() {
        PaudbunlResult r = run(injectStatus(twoAccounts(), ImsFunctionCodes.GNP, 2, "GP"));
        assertEquals(16, r.returnCode());
        assertTrue(sysout.contains("GNP CALL FAILED  :GP"));
        assertTrue(sysout.stream().anyMatch(s -> s.startsWith("KFB AREA IN CHILD:AB")));
        assertEquals("IMSUNLOD ABENDING ...", sysout.get(sysout.size() - 1));
        assertEquals(1, opfile1.records().size());
        assertEquals(1, opfile2.records().size());
    }

    @Test
    void opfile1OpenErrorAbendsBeforeOpeningOpfile2() {
        opfile1.failOpenWith("30");
        PaudbunlResult r = run(twoAccounts());
        assertEquals(16, r.returnCode());
        assertEquals(List.of("ERROR IN OPENING OPFILE1:30", "IMSUNLOD ABENDING ..."), sysout.subList(4, 6));
        assertEquals(0, opfile2.openCount());
        assertEquals(0, r.wsNoSumryRead());
    }

    @Test
    void opfile2OpenErrorAbendsAndClosesOpfile1() {
        opfile2.failOpenWith("37");
        PaudbunlResult r = run(twoAccounts());
        assertEquals(16, r.returnCode());
        assertEquals(List.of("ERROR IN OPENING OPFILE2:37", "IMSUNLOD ABENDING ..."), sysout.subList(4, 6));
        assertFalse(opfile1.isOpen());
        assertTrue(opfile1.records().isEmpty());
    }

    @Test
    void openStatusSpacesIsAccepted() {
        opfile1.failOpenWith("  ");
        assertEquals(0, run(twoAccounts()).returnCode());
        assertEquals(2, opfile1.records().size());
    }

    @Test
    void closeErrorsAreDisplayedButDoNotChangeReturnCode() {
        opfile1.failCloseWith("30");
        opfile2.failCloseWith("90");
        PaudbunlResult r = run(twoAccounts());
        assertEquals(0, r.returnCode());
        assertEquals(List.of("CLOSING THE FILE", "ERROR IN CLOSING 1ST FILE:30", "ERROR IN CLOSING 2ND FILE:90"),
                sysout.subList(sysout.size() - 3, sysout.size()));
    }

    @Test
    void writeErrorsAreNotCheckedLikeCobolButAreNotCountedAsWritten() {
        opfile2.failWriteWith("34");
        PaudbunlResult r = run(twoAccounts());
        // PAUDBUNL never tests the FILE STATUS after WRITE, so the run still ends with RC 0
        assertEquals(0, r.returnCode());
        assertEquals(2, r.rootRecordsWritten());
        assertEquals(0, r.childRecordsWritten());
        assertEquals(4, r.wsNoSumryRead());
        assertTrue(opfile2.records().isEmpty());
    }

    @Test
    void rootWithoutChildrenWritesOnlyOutfil1() {
        PaudbunlResult r = run(new InMemoryPendingAuthDatabase().addRoot(TestSegments.summaryBytes(8)));
        assertEquals(1, opfile1.records().size());
        assertTrue(opfile2.records().isEmpty());
        assertFalse(r.moreAuths());
        assertEquals(1, r.wsNoSumryRead());
    }

    @Test
    void pcbStatusIsReinitialisedAfterEveryGnpSoGeDoesNotLeakIntoRootChecks() {
        List<String> statusesSeenByGn = new ArrayList<>();
        InMemoryPendingAuthDatabase db = twoAccounts();
        DliGateway spy = (f, pcb, io, ssa) -> {
            if (f == ImsFunctionCodes.GN) {
                statusesSeenByGn.add(pcb.pcbStatus());
            }
            db.cbltdli(f, pcb, io, ssa);
        };
        run(spy);
        assertEquals(List.of("  ", "  ", "  "), statusesSeenByGn);
    }

    @Test
    void pautbunlPcbMatchesPsb() {
        PautbPcb pcb = Paudbunl.pautbunlPcb();
        assertEquals("DBPAUTP0", pcb.dbdName());
        assertEquals("GOTP", pcb.procopt());
        assertEquals(2, pcb.numSensegs());
    }
}
