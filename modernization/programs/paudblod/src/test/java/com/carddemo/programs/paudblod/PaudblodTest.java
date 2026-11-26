package com.carddemo.programs.paudblod;

import static org.junit.jupiter.api.Assertions.*;

import com.carddemo.programs.paudblod.copybook.ImsFuncs;
import com.carddemo.programs.paudblod.copybook.PautbPcb;
import com.carddemo.programs.paudblod.copybook.PendingAuthDetails;
import com.carddemo.programs.paudblod.ims.InMemoryPautDatabase;
import com.carddemo.programs.paudblod.ims.Ssa;
import com.carddemo.programs.paudblod.io.InMemoryRecordReader;
import com.carddemo.programs.paudblod.io.ReadResult;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;

class PaudblodTest {

    private static final Charset CS = Paudblod.EBCDIC;
    private static final Clock CLOCK = Clock.fixed(Instant.parse("2024-02-29T10:15:00Z"), ZoneOffset.UTC);

    private final List<String> out = new ArrayList<>();
    private InMemoryPautDatabase db;
    private TestData.ScriptedGateway gw;
    private PautbPcb pcb;

    @BeforeEach
    void setUp() {
        db = new InMemoryPautDatabase((byte) 0x40);
        gw = new TestData.ScriptedGateway(db);
        pcb = new PautbPcb("DBPAUTP0", "A", 2);
    }

    private Paudblod program(InMemoryRecordReader in1, InMemoryRecordReader in2) {
        return new Paudblod(in1, in2, gw, pcb, CLOCK, CS, out::add);
    }

    private static byte[] root(long acct) {
        return TestData.summary(acct).toBytes(CS);
    }

    private static byte[] child(long acct, long time) {
        return TestData.childRecord(BigDecimal.valueOf(acct),
                TestData.details(time, "TXN" + time).toBytes(CS));
    }

    private static InMemoryRecordReader records(byte[]... recs) {
        return InMemoryRecordReader.ofRecords(List.of(recs));
    }

    @Test
    void mainParaLoadsRootsThenChildrenAndReturnsZero() {
        InMemoryRecordReader in1 = records(root(1), root(2));
        InMemoryRecordReader in2 = records(child(1, 10), child(1, 11), child(2, 20));

        int rc = program(in1, in2).run();

        assertEquals(0, rc);
        assertEquals(2, db.rootCount());
        assertEquals(3, db.childCount());
        assertTrue(in1.isClosed());
        assertTrue(in2.isClosed());
        assertEquals(List.of(
                "STARTING PAUDBLOD",
                "*-------------------------------------*",
                "TODAYS DATE            :240229",
                " ",
                " *******************************",
                " *******************************",
                "ROOT INSERT SUCCESS    ",
                " *******************************",
                " *******************************",
                "ROOT INSERT SUCCESS    ",
                "***************************",
                "***************************",
                "GU CALL TO ROOT SEG SUCCESS",
                "CHILD SEGMENT INSERTED SUCCESS",
                "***************************",
                "***************************",
                "GU CALL TO ROOT SEG SUCCESS",
                "CHILD SEGMENT INSERTED SUCCESS",
                "***************************",
                "***************************",
                "GU CALL TO ROOT SEG SUCCESS",
                "CHILD SEGMENT INSERTED SUCCESS",
                "CLOSING THE FILE"), out);
    }

    @Test
    void mainParaIssuesIsrtGuIsrtWithTheWorkingStorageSsas() {
        program(records(root(7)), records(child(7, 1))).run();

        assertEquals(3, gw.calls.size());
        assertEquals(ImsFuncs.FUNC_ISRT, gw.calls.get(0).function());
        assertInstanceOf(Ssa.RootUnqualSsa.class, gw.calls.get(0).ssa());
        assertEquals(ImsFuncs.FUNC_GU, gw.calls.get(1).function());
        assertEquals(new Ssa.RootQualSsa(BigDecimal.valueOf(7)), gw.calls.get(1).ssa());
        assertEquals(ImsFuncs.FUNC_ISRT, gw.calls.get(2).function());
        assertInstanceOf(Ssa.ChildUnqualSsa.class, gw.calls.get(2).ssa());
    }

    @Test
    void initializeAcceptsDateAsYymmddAndDayAsYyddd() {
        Paudblod p = program(records(), records());
        p.initialize();
        assertEquals("240229", p.currentDate());
        assertEquals("24060", p.currentYyddd());
    }

    @Test
    void initializeAbendsWhenInfile1CannotBeOpened() {
        InMemoryRecordReader in1 = records().withOpenStatus("35");
        InMemoryRecordReader in2 = records();

        int rc = program(in1, in2).run();

        assertEquals(16, rc);
        assertFalse(in2.isOpened());
        assertFalse(in1.isClosed());
        assertEquals(List.of("ERROR IN OPENING INFILE1:35", "IMS LOAD ABENDING ..."),
                out.subList(out.size() - 2, out.size()));
    }

    @Test
    void initializeAbendsWhenInfile2CannotBeOpened() {
        InMemoryRecordReader in1 = records(root(1));
        int rc = program(in1, records().withOpenStatus("35")).run();

        assertEquals(16, rc);
        assertEquals(0, db.rootCount());
        assertEquals(List.of("ERROR IN OPENING INFILE2:35", "IMS LOAD ABENDING ..."),
                out.subList(out.size() - 2, out.size()));
    }

    @Test
    void initializeTreatsSpacesFileStatusAsSuccess() {
        assertEquals(0, program(records().withOpenStatus("  "), records().withOpenStatus("  ")).run());
    }

    @Test
    void readRootSegFileReportsReadErrorAndKeepsReading() {
        InMemoryRecordReader in1 = InMemoryRecordReader.ofResults(List.of(
                ReadResult.error("30"), ReadResult.ok(root(5))));

        int rc = program(in1, records()).run();

        assertEquals(0, rc);
        assertEquals(1, db.rootCount());
        assertTrue(out.contains("ERROR READING ROOT SEG INFILE"));
    }

    @Test
    void insertRootSegReportsDuplicateAndContinues() {
        db.preloadRoot(root(1));

        int rc = program(records(root(1)), records()).run();

        assertEquals(0, rc);
        assertTrue(out.contains("ROOT SEGMENT ALREADY IN DB"));
        assertFalse(out.contains("ROOT INSERT SUCCESS    "));
    }

    @Test
    void insertRootSegAbendsOnAnyOtherStatus() {
        gw.force((f, s) -> s instanceof Ssa.RootUnqualSsa, "AI");
        InMemoryRecordReader in2 = records(child(1, 1));

        int rc = program(records(root(1), root(2)), in2).run();

        assertEquals(16, rc);
        assertEquals(1, gw.calls.size());
        assertFalse(in2.isClosed());
        assertFalse(out.contains("CLOSING THE FILE"));
        assertEquals(List.of("ROOT INSERT FAILED  :AI", "IMS LOAD ABENDING ..."),
                out.subList(out.size() - 2, out.size()));
    }

    @Test
    void readChildSegFileSkipsRecordsWhoseRootKeyIsNotNumeric() {
        byte[] bad = child(1, 1);
        bad[0] = 0x40;
        bad[5] = 0x40;

        int rc = program(records(root(1)), records(bad)).run();

        assertEquals(0, rc);
        assertEquals(1, gw.calls.size());
        assertEquals(0, db.childCount());
        assertFalse(out.contains("GU CALL TO ROOT SEG SUCCESS"));
    }

    @Test
    void readChildSegFileAcceptsUnsignedPackedKey() {
        byte[] rec = child(3, 1);
        rec[5] = (byte) ((rec[5] & 0xF0) | 0x0F);

        program(records(root(3)), records(rec)).run();

        assertEquals(1, db.childCount());
        assertEquals(new Ssa.RootQualSsa(BigDecimal.valueOf(3)), gw.calls.get(1).ssa());
    }

    @Test
    void readChildSegFileReportsReadErrorAndKeepsReading() {
        InMemoryRecordReader in2 = InMemoryRecordReader.ofResults(List.of(
                ReadResult.error("30"), ReadResult.ok(child(1, 1))));

        int rc = program(records(root(1)), in2).run();

        assertEquals(0, rc);
        assertEquals(1, db.childCount());
        assertTrue(out.contains("ERROR READING CHILD SEG INFILE"));
    }

    @Test
    void insertChildSegInitializesPcbStatusBeforeGu() {
        db.preloadRoot(root(1));
        program(records(root(1)), records(child(1, 1))).run();

        assertTrue(out.contains("ROOT SEGMENT ALREADY IN DB"));
        assertEquals("  ", gw.calls.get(1).statusBefore());
        assertEquals(1, db.childCount());
    }

    @Test
    void insertChildSegSilentlySkipsChildWhenGuFails() {
        int rc = program(records(), records(child(99, 1))).run();

        assertEquals(0, rc);
        assertEquals(1, gw.calls.size());
        assertEquals(0, db.childCount());
        assertEquals(List.of("***************************", "***************************", "CLOSING THE FILE"),
                out.subList(out.size() - 3, out.size()));
    }

    @Test
    void insertChildSegSkipsChildOnForcedGuErrorWithoutAbend() {
        gw.force((f, s) -> f == ImsFuncs.FUNC_GU, "AI");
        int rc = program(records(root(1)), records(child(1, 1))).run();

        assertEquals(0, rc);
        assertEquals(0, db.childCount());
        assertFalse(out.contains("IMS LOAD ABENDING ..."));
    }

    @Test
    void insertImsCallReportsDuplicateChildAndContinues() {
        int rc = program(records(root(1)), records(child(1, 1), child(1, 1))).run();

        assertEquals(0, rc);
        assertEquals(1, db.childCount());
        assertTrue(out.contains("CHILD SEGMENT ALREADY IN DB"));
        assertFalse(out.contains("ROOT GU CALL FAIL:II"));
    }

    @Test
    void insertImsCallAbendsOnAnyOtherStatusAndShowsKeyFeedback() {
        gw.force((f, s) -> s instanceof Ssa.ChildUnqualSsa, "AM");
        InMemoryRecordReader in2 = records(child(1, 1), child(1, 2));

        int rc = program(records(root(1)), in2).run();

        assertEquals(16, rc);
        assertEquals(3, gw.calls.size());
        assertFalse(in2.isClosed());
        String keyfb = new String(pcb.getPautKeyfb(), CS);
        assertEquals(255, keyfb.length());
        assertEquals(List.of("INSERT CALL FAIL FOR CHILD:AM", "KFB AREA IN CHILD:" + keyfb, "IMS LOAD ABENDING ..."),
                out.subList(out.size() - 3, out.size()));
    }

    @Test
    void fileCloseReportsCloseErrorsWithoutChangingReturnCode() {
        int rc = program(records().withCloseStatus("30"), records().withCloseStatus("42")).run();

        assertEquals(0, rc);
        assertEquals(List.of("CLOSING THE FILE", "ERROR IN CLOSING 1ST FILE:30", "ERROR IN CLOSING 2ND FILE:42"),
                out.subList(out.size() - 3, out.size()));
    }

    @Test
    void childSegmentIsStoredByteForByteUnderItsRoot() {
        byte[] detail = TestData.details(123456789L, "TXN1").toBytes(CS);
        program(records(root(4)), records(TestData.childRecord(BigDecimal.valueOf(4), detail))).run();

        List<byte[]> kids = db.childSegmentsByRoot().get("00000000004C");
        assertEquals(1, kids.size());
        assertArrayEquals(detail, kids.get(0));
        assertEquals(new BigDecimal("42.17"), PendingAuthDetails.fromBytes(kids.get(0), CS).paApprovedAmt());
    }
}
