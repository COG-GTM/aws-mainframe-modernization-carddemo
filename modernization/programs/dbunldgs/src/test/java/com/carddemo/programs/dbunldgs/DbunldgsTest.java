package com.carddemo.programs.dbunldgs;

import com.carddemo.programs.dbunldgs.copybook.CobolCodec;
import com.carddemo.programs.dbunldgs.copybook.PendingAuthDetails;
import com.carddemo.programs.dbunldgs.copybook.PendingAuthSummary;
import org.junit.jupiter.api.Test;

import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;

import static com.carddemo.programs.dbunldgs.TestSegments.detailBytes;
import static com.carddemo.programs.dbunldgs.TestSegments.nonNumericSummaryBytes;
import static com.carddemo.programs.dbunldgs.TestSegments.summaryBytes;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class DbunldgsTest {

    private static final Clock CLOCK = Clock.fixed(Instant.parse("2026-11-26T01:00:00Z"), ZoneOffset.UTC);

    private final List<String> sysout = new ArrayList<>();
    private final InMemoryGsamDataset pasfl = new InMemoryGsamDataset(PendingAuthSummary.LENGTH);
    private final InMemoryGsamDataset padfl = new InMemoryGsamDataset(PendingAuthDetails.LENGTH);

    private Dbunldgs program(InMemoryPautDatabase db) {
        return new Dbunldgs(db, pasfl, padfl, sysout::add, CLOCK, CobolCodec.EBCDIC);
    }

    private static final List<String> BANNER = List.of(
            "STARTING PROGRAM DBUNLDGS::",
            "*-------------------------------------*",
            "TODAYS DATE            :261126",
            " ");

    @Test
    void initializeAcceptsDateAndDayAndDisplaysBanner() {
        Dbunldgs p = program(new InMemoryPautDatabase());
        p.initialize1000();
        assertEquals(BANNER, sysout);
        assertEquals("261126", p.getCurrentDate());
        assertEquals("26330", p.getCurrentYyddd());
    }

    @Test
    void emptyDatabaseEndsOnGbWithReturnCodeZero() {
        Dbunldgs p = program(new InMemoryPautDatabase());
        assertEquals(0, p.run());
        List<String> expected = new ArrayList<>(BANNER);
        expected.add("CLOSING THE FILE");
        assertEquals(expected, sysout);
        assertTrue(pasfl.records().isEmpty());
        assertTrue(padfl.records().isEmpty());
        assertTrue(p.isEndOfAuthdb());
        assertEquals(0, p.getWsNoSumryRead());
    }

    @Test
    void unloadsRootsAndChildrenInHierarchicSequenceByteForByte() {
        byte[] r1 = summaryBytes(1);
        byte[] r2 = summaryBytes(2);
        List<byte[]> c1 = List.of(detailBytes(1), detailBytes(2));
        List<byte[]> c2 = List.of(detailBytes(3));
        InMemoryPautDatabase db = new InMemoryPautDatabase().addRecord(r1, c1).addRecord(r2, c2);
        Dbunldgs p = program(db);

        assertEquals(Dbunldgs.RC_NORMAL, p.run());

        assertEquals(2, pasfl.records().size());
        assertArrayEquals(r1, pasfl.records().get(0));
        assertArrayEquals(r2, pasfl.records().get(1));
        assertEquals(3, padfl.records().size());
        assertArrayEquals(c1.get(0), padfl.records().get(0));
        assertArrayEquals(c1.get(1), padfl.records().get(1));
        assertArrayEquals(c2.get(0), padfl.records().get(2));

        List<String> expected = new ArrayList<>(BANNER);
        expected.add("CHILD SEG FLAG GE : Y");
        expected.add("CHILD SEG FLAG GE : Y");
        expected.add("CLOSING THE FILE");
        assertEquals(expected, sysout);
        // the COBOL adds roots and children to the same summary counters
        assertEquals(5, p.getWsNoSumryRead());
        assertEquals(5, p.getWsAuthSmryProcCnt());
        assertTrue(p.isMoreAuths());
        assertTrue(p.isEndOfAuthdb());
    }

    @Test
    void workingStorageRecordsMirrorLastSegments() {
        byte[] r1 = summaryBytes(7);
        byte[] c1 = detailBytes(9);
        Dbunldgs p = program(new InMemoryPautDatabase().addRecord(r1, List.of(c1)));
        p.run();
        assertArrayEquals(r1, p.getOpfil1Rec());
        assertArrayEquals(java.util.Arrays.copyOfRange(r1, 0, 6), p.getRootSegKey());
        assertArrayEquals(c1, p.getChildSegRec());
    }

    @Test
    void rootWithoutChildrenIsWrittenAndGeEndsChildLoop() {
        byte[] r1 = summaryBytes(42);
        Dbunldgs p = program(new InMemoryPautDatabase().addRecord(r1, List.of()));
        assertEquals(0, p.run());
        assertEquals(1, pasfl.records().size());
        assertTrue(padfl.records().isEmpty());
        assertTrue(sysout.contains("CHILD SEG FLAG GE : Y"));
        assertFalse(p.isMoreAuths());
    }

    @Test
    void nonNumericAccountIdSkipsRootAndItsChildren() {
        byte[] bad = nonNumericSummaryBytes();
        byte[] good = summaryBytes(3);
        InMemoryPautDatabase db = new InMemoryPautDatabase()
                .addRecord(bad, List.of(detailBytes(1), detailBytes(2)))
                .addRecord(good, List.of(detailBytes(3)));
        Dbunldgs p = program(db);

        assertEquals(0, p.run());

        assertEquals(1, pasfl.records().size());
        assertArrayEquals(good, pasfl.records().get(0));
        assertEquals(1, padfl.records().size());
        assertArrayEquals(detailBytes(3), padfl.records().get(0));
        assertEquals(1, sysout.stream().filter("CHILD SEG FLAG GE : Y"::equals).count());
        // the skipped root is still counted as read
        assertEquals(3, p.getWsNoSumryRead());
    }

    @Test
    void gnFailureDisplaysStatusAndKeyFeedbackThenAbendsWith16() {
        InMemoryPautDatabase db = new InMemoryPautDatabase()
                .addRecord(summaryBytes(1), List.of())
                .injectStatus(1, "AI");
        Dbunldgs p = program(db);

        assertEquals(16, p.run());

        assertEquals(BANNER.size() + 3, sysout.size());
        assertEquals("AUTH SUM  GN FAILED  :AI", sysout.get(4));
        assertTrue(sysout.get(5).startsWith("KEY FEEDBACK AREA    :"));
        assertEquals("KEY FEEDBACK AREA    :".length() + 255, sysout.get(5).length());
        assertEquals("DBUNLDGS ABENDING ...", sysout.get(6));
        assertFalse(sysout.contains("CLOSING THE FILE"));
        assertTrue(pasfl.records().isEmpty());
    }

    @Test
    void gnFailureOnSecondRootKeepsEarlierOutput() {
        InMemoryPautDatabase db = new InMemoryPautDatabase()
                .addRecord(summaryBytes(1), List.of(detailBytes(1)))
                .addRecord(summaryBytes(2), List.of())
                .injectStatus(4, "BA"); // GN, GNP, GNP(GE), GN <- fails
        Dbunldgs p = program(db);

        assertEquals(Dbunldgs.RC_ABEND, p.run());
        assertEquals(1, pasfl.records().size());
        assertEquals(1, padfl.records().size());
        assertTrue(sysout.contains("AUTH SUM  GN FAILED  :BA"));
        assertEquals("DBUNLDGS ABENDING ...", sysout.get(sysout.size() - 1));
    }

    @Test
    void gnpFailureDisplaysStatusAndKfbThenAbends() {
        InMemoryPautDatabase db = new InMemoryPautDatabase()
                .addRecord(summaryBytes(1), List.of(detailBytes(1)))
                .injectStatus(2, "GP");
        Dbunldgs p = program(db);

        assertEquals(16, p.run());
        assertEquals(1, pasfl.records().size());
        assertTrue(padfl.records().isEmpty());
        int i = sysout.indexOf("GNP CALL FAILED  :GP");
        assertTrue(i > 0);
        assertTrue(sysout.get(i + 1).startsWith("KFB AREA IN CHILD:"));
        assertEquals("DBUNLDGS ABENDING ...", sysout.get(i + 2));
        assertEquals(i + 3, sysout.size());
    }

    @Test
    void parentGsamInsertFailureAbends() {
        pasfl.injectStatus(1, "AO");
        Dbunldgs p = program(new InMemoryPautDatabase().addRecord(summaryBytes(1), List.of(detailBytes(1))));

        assertEquals(16, p.run());
        List<String> tail = sysout.subList(BANNER.size(), sysout.size());
        assertEquals(3, tail.size());
        assertEquals("GSAM PARENT FAIL :AO", tail.get(0));
        assertEquals("KFB AREA IN GSAM:" + " ".repeat(100), tail.get(1));
        assertEquals("DBUNLDGS ABENDING ...", tail.get(2));
        assertTrue(padfl.records().isEmpty());
    }

    @Test
    void childGsamInsertFailureReusesParentFailMessage() {
        padfl.injectStatus(2, "AO");
        Dbunldgs p = program(new InMemoryPautDatabase()
                .addRecord(summaryBytes(1), List.of(detailBytes(1), detailBytes(2), detailBytes(3))));

        assertEquals(16, p.run());
        assertEquals(1, padfl.records().size());
        List<String> tail = sysout.subList(BANNER.size(), sysout.size());
        assertEquals(List.of("GSAM PARENT FAIL :AO", "KFB AREA IN GSAM:" + " ".repeat(255), "DBUNLDGS ABENDING ..."), tail);
    }

    @Test
    void gnIssuesUnqualifiedRootSsaAndGnpUnqualifiedChildSsa() {
        List<String> calls = new ArrayList<>();
        InMemoryPautDatabase db = new InMemoryPautDatabase().addRecord(summaryBytes(1), List.of(detailBytes(1)));
        DliCallHandler spy = (f, pcb, io, ssa) -> {
            calls.add(f + "|" + ssa);
            db.cbltdli(f, pcb, io, ssa);
        };
        new Dbunldgs(spy, pasfl, padfl, sysout::add, CLOCK, CobolCodec.EBCDIC).run();
        assertEquals(List.of("GN  |PAUTSUM0 ", "GNP |PAUTDTL1 ", "GNP |PAUTDTL1 ", "GN  |PAUTSUM0 "), calls);
    }

    @Test
    void pcbStatusIsInitializedAfterEachChildCall() {
        Dbunldgs p = program(new InMemoryPautDatabase().addRecord(summaryBytes(1), List.of()));
        p.run();
        // last call was GN returning GB, which 2000 does not reset
        assertEquals("GB", p.getPautbPcb().getPcbStatus());
        Dbunldgs q = program(new InMemoryPautDatabase().addRecord(summaryBytes(1), List.of()));
        q.initialize1000();
        q.findNextAuthSummary2000();
        assertEquals("  ", q.getPautbPcb().getPcbStatus());
    }

    @Test
    void imsReturnCodeConditionNames() {
        assertTrue(ImsReturnCode.statusOk("  "));
        assertTrue(ImsReturnCode.statusOk("FW"));
        assertFalse(ImsReturnCode.statusOk("GE"));
        assertTrue(ImsReturnCode.segmentNotFound("GE"));
        assertTrue(ImsReturnCode.duplicateSegmentFound("II"));
        assertTrue(ImsReturnCode.wrongParentage("GP"));
        assertTrue(ImsReturnCode.endOfDb("GB"));
        assertTrue(ImsReturnCode.databaseUnavailable("BA"));
        assertTrue(ImsReturnCode.psbScheduledMoreThanOnce("TC"));
        assertTrue(ImsReturnCode.couldNotSchedulePsb("TE"));
        assertTrue(ImsReturnCode.retryCondition("BA"));
        assertTrue(ImsReturnCode.retryCondition("FH"));
        assertTrue(ImsReturnCode.retryCondition("TE"));
        assertFalse(ImsReturnCode.retryCondition("GB"));
    }

    @Test
    void inMemoryDatabaseRejectsGnpWithoutParentageAndUnknownCalls() {
        InMemoryPautDatabase db = new InMemoryPautDatabase().addRecord(summaryBytes(1), List.of());
        com.carddemo.programs.dbunldgs.copybook.PautbPcb pcb = new com.carddemo.programs.dbunldgs.copybook.PautbPcb();
        db.cbltdli("GNP ", pcb, new byte[200], Dbunldgs.CHILD_UNQUAL_SSA);
        assertEquals("GP", pcb.getPcbStatus());
        db.cbltdli("DLET", pcb, new byte[100], Dbunldgs.ROOT_UNQUAL_SSA);
        assertEquals("AD", pcb.getPcbStatus());
    }
}
