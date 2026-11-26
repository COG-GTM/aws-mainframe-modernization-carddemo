package com.carddemo.programs.cotrtlic;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.programs.cotrtlic.copybook.CardDemoCommarea;
import com.carddemo.programs.cotrtlic.copybook.Dfhaid;
import com.carddemo.programs.cotrtlic.copybook.Dfhbmsca;
import com.carddemo.programs.cotrtlic.support.Cob;
import org.junit.jupiter.api.Test;

/** 0000-MAIN routing, PF keys and paging (8000-READ-FORWARD / 8100-READ-BACKWARDS / 2500 messages). */
class CotrtlicNavigationTest {

    @Test
    void pf8PagesForwardFromTheLookAheadKeyAndPf7PagesBack() {
        Harness h = new Harness().withRows(1, 10);
        CicsResponse.SendMapAndReturn p1 = h.start();
        assertTrue(p1.commarea().thisProg().isNextPageExists());
        assertEquals("08", p1.commarea().thisProg().caLastTrCode, "8th row fetched as look-ahead");

        CicsResponse.SendMapAndReturn p2 = h.pressForMap(Dfhaid.DFHPF8);
        assertEquals("08", h.rowType(1));
        assertEquals("10", h.rowType(3));
        assertTrue(p2.commarea().thisProg().eachRowOutIsLowValues(4));
        assertEquals(Dfhbmsca.DFHBMPRO, p2.map().row(4).trtsel.attribute);
        assertEquals("2  ", p2.map().pageno.data());
        assertTrue(p2.commarea().thisProg().isNextPageNotExists());
        assertEquals("No more pages for these search conditions", h.errmsg());

        CicsResponse.SendMapAndReturn back = h.pressForMap(Dfhaid.DFHPF7);
        for (int i = 1; i <= 7; i++) {
            assertEquals(String.format("%02d", i), h.rowType(i));
        }
        assertEquals("1  ", back.map().pageno.data());
        assertEquals("01", back.commarea().thisProg().caFirstTrCode);
        assertEquals("08", back.commarea().thisProg().caLastTrCode, "8100 copies FIRST key into LAST key");
        assertTrue(back.commarea().thisProg().isNextPageExists());
        assertEquals("No previous pages to display", h.errmsg(),
                "2500 runs after SUBTRACT 1 FROM WS-CA-SCREEN-NUM, so arriving on page 1 via PF7 shows it");
    }

    @Test
    void pf7FromPageThreeLandsOnPageTwoWithoutMessage() {
        Harness h = new Harness().withRows(1, 20);
        h.start();
        h.pressForMap(Dfhaid.DFHPF8);
        h.pressForMap(Dfhaid.DFHPF8);
        assertEquals("15", h.rowType(1));
        CicsResponse.SendMapAndReturn r = h.pressForMap(Dfhaid.DFHPF7);
        assertEquals("08", h.rowType(1));
        assertEquals("14", h.rowType(7));
        assertEquals("2  ", r.map().pageno.data());
        assertTrue(Cob.isSpaces(r.map().errmsg.data()));
    }

    @Test
    void pf19FoldsOntoPf7AndPf20OntoPf8() {
        Harness h = new Harness().withRows(1, 9);
        h.start();
        CicsResponse.SendMapAndReturn r = h.pressForMap(Dfhaid.DFHPF20);
        assertEquals("08", h.rowType(1));
        r = h.pressForMap(Dfhaid.DFHPF19);
        assertEquals("01", h.rowType(1));
        assertEquals(1, r.commarea().thisProg().caScreenNum);
    }

    @Test
    void pf7OnFirstPageSaysNoPreviousPages() {
        Harness h = new Harness().withFixture();
        h.start();
        h.pressForMap(Dfhaid.DFHPF7);
        assertEquals("No previous pages to display", h.errmsg());
        assertEquals("01", h.rowType(1));
    }

    @Test
    void pf8WithoutNextPageMarksLastPageShownThenSaysNoMorePages() {
        Harness h = new Harness().withFixture();
        h.start();
        CicsResponse.SendMapAndReturn r = h.pressForMap(Dfhaid.DFHPF8);
        assertEquals("Type U to update, D to delete any record", h.infomsg());
        assertTrue(r.commarea().thisProg().isLastPageShown());
        h.pressForMap(Dfhaid.DFHPF8);
        assertEquals("No more pages to display", h.errmsg());
    }

    @Test
    void unsupportedKeyIsTreatedAsEnter() {
        Harness h = new Harness().withFixture();
        h.start();
        CicsResponse.SendMapAndReturn r = h.pressForMap(Dfhaid.DFHPF5);
        assertEquals("01", h.rowType(1));
        assertTrue(Cob.isSpaces(r.map().errmsg.data()));
        h.pressForMap(Dfhaid.DFHPA3);
        assertEquals("01", h.rowType(1));
    }

    @Test
    void pf10WithoutPendingActionIsTreatedAsEnter() {
        Harness h = new Harness().withFixture();
        h.start();
        h.pressForMap(Dfhaid.DFHPF10);
        assertEquals(0, h.syncpoint.count());
        assertEquals("Type U to update, D to delete any record", h.infomsg());
    }

    @Test
    void pf3ReturnsToAdminMenuWithSyncpoint() {
        Harness h = new Harness().withFixture();
        h.start();
        CicsResponse.Xctl x = assertInstanceOf(CicsResponse.Xctl.class, h.press(Dfhaid.DFHPF3));
        assertEquals("COADM01C", x.program());
        assertEquals("CA00", x.commarea().toTranid);
        assertEquals("COADM01C", x.commarea().toProgram);
        assertEquals("CTLI", x.commarea().fromTranid);
        assertEquals("COTRTLIC", x.commarea().fromProgram);
        assertTrue(x.commarea().isUsrtypAdmin());
        assertTrue(x.commarea().isPgmEnter());
        assertEquals(1, h.syncpoint.count());
    }

    @Test
    void pf3WithForeignCallerReturnsToThatCaller() {
        Harness h = new Harness().withFixture();
        CardDemoCommarea cd = new CardDemoCommarea();
        cd.fromTranid = "CM00";
        cd.fromProgram = "COMEN01C";
        cd.setPgmReenter();
        CicsResponse resp = h.send(new CicsRequest(Dfhaid.DFHPF3, new ProgramCommarea(cd, new ThisProgCommarea()), null));
        CicsResponse.Xctl x = assertInstanceOf(CicsResponse.Xctl.class, resp);
        assertEquals("COMEN01C", x.program());
        assertEquals("CM00", x.commarea().toTranid);
    }

    @Test
    void pf3ComingBackFromAddScreenRedisplaysListAsEnter() {
        Harness h = new Harness().withFixture();
        CardDemoCommarea cd = new CardDemoCommarea();
        cd.fromTranid = "CTTU";
        cd.fromProgram = "COTRTUPC";
        cd.setPgmReenter();
        CicsResponse resp = h.send(new CicsRequest(Dfhaid.DFHPF3, new ProgramCommarea(cd, new ThisProgCommarea()), null));
        CicsResponse.SendMapAndReturn r = assertInstanceOf(CicsResponse.SendMapAndReturn.class, resp);
        assertEquals("01", h.rowType(1));
        assertEquals("COTRTLIC", r.commarea().cardDemo().fromProgram);
        assertEquals(1, r.commarea().thisProg().caScreenNum);
    }

    @Test
    void pf2TransfersToAddProgramAsUser() {
        Harness h = new Harness().withFixture();
        h.start();
        CicsResponse.Xctl x = assertInstanceOf(CicsResponse.Xctl.class, h.press(Dfhaid.DFHPF2));
        assertEquals("COTRTUPC", x.program());
        assertEquals("COTRTUPC", x.commarea().toProgram);
        assertTrue(x.commarea().isUsrtypUser());
        assertTrue(x.commarea().isPgmEnter());
        assertEquals("CTRTLIA", x.commarea().lastMap);
    }

    @Test
    void entryFromAdminMenuSkipsFilterAttributeSetup() {
        Harness h = new Harness().withFixture();
        CardDemoCommarea cd = new CardDemoCommarea();
        cd.fromTranid = "CA00";
        cd.fromProgram = "COADM01C";
        cd.userId = "ADMIN001";
        cd.setPgmEnter();
        CicsResponse resp = h.send(new CicsRequest(Dfhaid.DFHENTER, new ProgramCommarea(cd, new ThisProgCommarea()), null));
        CicsResponse.SendMapAndReturn r = assertInstanceOf(CicsResponse.SendMapAndReturn.class, resp);
        assertEquals(Cob.LOW_VALUE, r.map().trtype.attribute, "2400 CONTINUE branch");
        assertEquals(-1, r.map().trtype.length);
        assertEquals("ADMIN001", r.commarea().cardDemo().userId);
        assertEquals("01", h.rowType(1));
    }

    @Test
    void reentryFromAnotherProgramResetsCommarea() {
        Harness h = new Harness().withFixture();
        CardDemoCommarea cd = new CardDemoCommarea();
        cd.fromTranid = "CA00";
        cd.fromProgram = "COADM01C";
        cd.userId = "ADMIN001";
        cd.setPgmReenter();
        ThisProgCommarea stale = new ThisProgCommarea();
        stale.caScreenNum = 4;
        CicsResponse resp = h.send(new CicsRequest(Dfhaid.DFHENTER, new ProgramCommarea(cd, stale), null));
        CicsResponse.SendMapAndReturn r = assertInstanceOf(CicsResponse.SendMapAndReturn.class, resp);
        assertTrue(Cob.isSpaces(r.commarea().cardDemo().userId), "INITIALIZE CARDDEMO-COMMAREA");
        assertEquals(1, r.commarea().thisProg().caScreenNum);
        assertTrue(r.commarea().cardDemo().isPgmEnter());
        assertEquals("01", h.rowType(1));
    }

    @Test
    void emptyTableReportsNoRecordsAndSuppressesInfoMessage() {
        Harness h = new Harness();
        CicsResponse.SendMapAndReturn r = h.start();
        assertEquals("No records found for this search condition.", h.errmsg());
        assertTrue(Cob.isSpaces(r.map().infomsg.data()));
        assertEquals(Dfhbmsca.DFHBMDAR, r.map().infomsg.color);
        assertTrue(r.commarea().thisProg().isNextPageNotExists());
        assertTrue(Cob.isSpaces(r.commarea().thisProg().caLastTrCode));
    }
}
