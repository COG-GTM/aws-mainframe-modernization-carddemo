package com.carddemo.programs.cotrtupc;

import com.carddemo.programs.cotrtupc.copybook.CarddemoCommarea;
import com.carddemo.programs.cotrtupc.copybook.CtrtupaMap;
import com.carddemo.programs.cotrtupc.copybook.Dfhaid;
import com.carddemo.programs.cotrtupc.copybook.Dfhbmsca;
import com.carddemo.programs.cotrtupc.support.CobolText;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/** 0000-MAIN entry/exit routing, 0001-CHECK-PFKEYS, COMMON-RETURN, 3100/3250/3391 screen set-up. */
class CotrtupcNavigationTest {

    @Test
    void firstEntryWithoutCommareaSendsBlankSearchScreen() {
        Terminal t = Terminal.withSeedData();
        TranTypeMaintResponse r = t.press(Dfhaid.DFHENTER);

        assertEquals(TranTypeMaintResponse.Outcome.RETURN, r.outcome());
        assertEquals("CTTU", r.transId());
        CtrtupaMap s = r.screen();
        assertEquals("CTTU", s.trnname.data);
        assertEquals("COTRTUPC", s.pgmname.data);
        assertEquals("      AWS Mainframe Modernization       ", s.title01.data);
        assertEquals("              CardDemo                  ", s.title02.data);
        assertEquals("11/26/26", s.curdate.data);
        assertEquals("09:05:07", s.curtime.data);
        // 3250: 39-char prompt centred in WS-STRING-OUT (40) then moved to INFOMSGO X(45)
        assertEquals(CobolText.pic("Enter transaction type to be maintained", 45), s.infomsg.data);
        assertEquals(CobolText.spaces(78), s.errmsg.data);
        assertTrue(CobolText.isLowValues(s.trtypcd.data));
        assertEquals(Dfhbmsca.DFHBMFSE, s.trtypcd.attribute);
        assertEquals(Dfhbmsca.DFHBMPRF, s.trtydsc.attribute);
        assertEquals("TRTYPCD", s.cursorField());
        assertEquals(Dfhbmsca.DFHBMASB, s.infomsg.attribute);
        assertEquals(Dfhbmsca.DFHBMASB, s.fkeys.attribute);
        assertEquals(CobolText.LOW_VALUE, s.fkey04.attribute);
        assertEquals(CobolText.LOW_VALUE, s.fkey05.attribute);
        assertEquals(CobolText.LOW_VALUE, s.fkey12.attribute);
        // function-key legends come from the physical map INITIAL values (attribute left to the map: DRK)
        assertEquals("ENTER=Process F3=Exit", s.fkeys.displayText());
        assertEquals("F4=Delete", s.fkey04.displayText());
        assertEquals("F5=Save ", s.fkey05.displayText());
        assertEquals("F12=Cancel", s.fkey12.displayText());
        assertEquals("11/26/26", s.curdate.displayText());

        assertEquals(Cotrtupc.WS_COMMAREA_LENGTH, r.commarea().length());
        assertTrue(t.carddemo().isPgmReenter());
        assertEquals(0L, t.carddemo().acctId());
        assertEquals(CobolText.LOW_VALUE, t.state());
        assertTrue(CobolText.isSpaces(r.commarea().substring(CarddemoCommarea.LENGTH + TtupProgCommarea.LENGTH)));
    }

    @Test
    void entryFromAdminMenuReinitialisesStateRegardlessOfTrailingBytes() {
        CarddemoCommarea c = new CarddemoCommarea();
        c.initialize();
        c.fromTranid("CA00");
        c.fromProgram("COADM01C");
        c.userId("ADMIN001");
        c.setUsrtypAdmin();
        c.setPgmEnter();
        c.acctId(12345);
        Terminal t = Terminal.withSeedData();
        t.commarea = c.toFixed() + "SGARBAGE";   // admin passes only CARDDEMO-COMMAREA; storage beyond EIBCALEN

        t.press(Dfhaid.DFHENTER);

        assertEquals(CobolText.LOW_VALUE, t.state());
        assertTrue(t.carddemo().isPgmReenter());
        assertEquals(0L, t.carddemo().acctId());
        assertEquals("ADMIN001", t.carddemo().userId());
        assertEquals("Enter transaction type to be maintained", t.info());
    }

    @Test
    void entryFromListProgramShowsSearchScreen() {
        CarddemoCommarea c = new CarddemoCommarea();
        c.initialize();
        c.fromTranid("CTLI");
        c.fromProgram("COTRTLIC");
        c.setPgmEnter();
        Terminal t = Terminal.withSeedData();
        t.commarea = c.toFixed();

        t.press(Dfhaid.DFHENTER);

        assertEquals("Enter transaction type to be maintained", t.info());
        assertEquals("TRTYPCD", t.last.screen().cursorField());
        assertEquals("COTRTLIC", t.carddemo().fromProgram());
    }

    @Test
    void pf3WithoutCallerXctlsToAdminMenu() {
        Terminal t = Terminal.withSeedData().start();
        TranTypeMaintResponse r = t.press(Dfhaid.DFHPF3);

        assertEquals(TranTypeMaintResponse.Outcome.XCTL, r.outcome());
        assertEquals("COADM01C", r.xctlProgram());
        assertEquals(CarddemoCommarea.LENGTH, r.commarea().length());
        CarddemoCommarea c = new CarddemoCommarea();
        c.fromFixed(r.commarea());
        assertEquals("CA00", c.toTranid());
        assertEquals("COADM01C", c.toProgram());
        assertEquals("CTTU", c.fromTranid());
        assertEquals("COTRTUPC", c.fromProgram());
        assertTrue(c.isUsrtypAdmin());
        assertTrue(c.isPgmEnter());
        assertEquals("COTRTUP", c.lastMapset());
        assertEquals("CTRTUPA", c.lastMap());
        assertEquals(1, t.db.syncpoints());
        assertNull(r.screen());
    }

    @Test
    void pf3ReturnsToCallingListProgram() {
        CarddemoCommarea c = new CarddemoCommarea();
        c.initialize();
        c.fromTranid("CTLI");
        c.fromProgram("COTRTLIC");
        c.setPgmEnter();
        Terminal t = Terminal.withSeedData();
        t.commarea = c.toFixed();
        t.press(Dfhaid.DFHENTER);

        TranTypeMaintResponse r = t.press(Dfhaid.DFHPF3);

        assertEquals("COTRTLIC", r.xctlProgram());
        CarddemoCommarea out = new CarddemoCommarea();
        out.fromFixed(r.commarea());
        assertEquals("CTLI", out.toTranid());
    }

    @Test
    void pf15IsTreatedAsPf3() {
        Terminal t = Terminal.withSeedData().start();
        assertEquals(TranTypeMaintResponse.Outcome.XCTL, t.press(Dfhaid.DFHPF15).outcome());
    }

    @Test
    void pf3OnVeryFirstTaskXctlsToAdmin() {
        Terminal t = Terminal.withSeedData();
        TranTypeMaintResponse r = t.press(Dfhaid.DFHPF3);
        assertEquals("COADM01C", r.xctlProgram());
    }

    @Test
    void invalidKeyKeepsStateAndShowsMessage() {
        Terminal t = Terminal.withSeedData().start();
        t.show("01");
        t.press(Dfhaid.DFHPF7, "01", "PURCHASE");

        assertEquals("Invalid key pressed", t.error());
        assertEquals('S', t.state());
        assertEquals("01", t.last.screen().trtypcd.data);
    }

    @Test
    void clearAndPaKeysAreInvalid() {
        Terminal t = Terminal.withSeedData().start();
        t.press(Dfhaid.DFHCLEAR);
        assertEquals("Invalid key pressed", t.error());
        t.press(Dfhaid.DFHPA1);
        assertEquals("Invalid key pressed", t.error());
    }

    @Test
    void pf5WithoutPendingChangesIsInvalid() {
        Terminal t = Terminal.withSeedData().start();
        t.press(Dfhaid.DFHPF5);
        assertEquals("Invalid key pressed", t.error());
    }

    @Test
    void returnedCommareaRoundTripsBetweenTasks() {
        Terminal t = Terminal.withSeedData().start();
        t.show("02");
        TtupProgCommarea p = t.ttup();
        assertEquals("02", p.oldType());
        assertEquals(CobolText.pic("PAYMENT", 50), p.oldDesc());
        assertEquals(CobolText.spaces(52), t.commarea.substring(CarddemoCommarea.LENGTH + 53,
                CarddemoCommarea.LENGTH + 105));
    }
}
