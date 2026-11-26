package com.carddemo.programs.cotrtlic;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.programs.cotrtlic.copybook.Dfhaid;
import com.carddemo.programs.cotrtlic.copybook.Dfhbmsca;
import com.carddemo.programs.cotrtlic.gateway.InMemoryTransactionTypeTable.Statement;
import com.carddemo.programs.cotrtlic.support.Cob;
import org.junit.jupiter.api.Test;

/** 1210/1211/1240 row edits, 9200-UPDATE-RECORD, 9300-DELETE-RECORD and their 2200/2300/2500 effects. */
class CotrtlicActionTest {

    private static Harness listed() {
        Harness h = new Harness().withFixture();
        h.start();
        return h;
    }

    @Test
    void invalidActionCodeIsRejected() {
        Harness h = listed();
        CicsResponse.SendMapAndReturn r = h.pressForMap(Dfhaid.DFHENTER, m -> m.row(3).trtsel.receive("X"));
        assertEquals("Action code selected is invalid", h.errmsg());
        assertEquals(Dfhbmsca.DFHRED, r.map().row(3).trtsel.color);
        assertEquals(-1, r.map().row(3).trtsel.length);
        assertEquals("X", r.map().row(3).trtsel.data());
    }

    @Test
    void moreThanOneActionIsRejected() {
        Harness h = listed();
        CicsResponse.SendMapAndReturn r = h.pressForMap(Dfhaid.DFHENTER, m -> {
            m.row(1).trtsel.receive("D");
            m.row(4).trtsel.receive("U");
        });
        assertEquals("Please select only 1 action", h.errmsg());
        assertEquals(Dfhbmsca.DFHRED, r.map().row(1).trtsel.color);
        assertEquals(Dfhbmsca.DFHRED, r.map().row(4).trtsel.color);
        assertEquals(Dfhbmsca.DFHBLUE, r.map().trtype.color, "filters shown as ASKIP when actions present");
        assertEquals(Dfhbmsca.DFHBMASF, r.map().trtype.attribute);
    }

    @Test
    void deleteAsksForConfirmationThenDeletesOnPf10() {
        Harness h = listed();
        CicsResponse.SendMapAndReturn ask = h.pressForMap(Dfhaid.DFHENTER, m -> m.row(2).trtsel.receive("D"));
        assertEquals("Delete HIGHLIGHTED row ? Press F10 to confirm", h.infomsg());
        assertTrue(ask.commarea().thisProg().isDeleteRequested());
        assertEquals(2, ask.commarea().thisProg().caRowSelected);
        assertEquals(Dfhbmsca.DFHNEUTR, ask.map().row(2).trttyp.color);
        assertEquals(Dfhbmsca.DFHNEUTR, ask.map().row(2).trtypd.color);
        assertEquals(-1, ask.map().row(2).trtsel.length);
        assertEquals("D", ask.map().row(2).trtsel.data());

        CicsResponse.SendMapAndReturn done = h.pressForMap(Dfhaid.DFHPF10);
        assertEquals("HIGHLIGHTED row deleted.Hit Enter to continue", h.infomsg());
        assertFalse(h.table.snapshot().containsKey("02"));
        assertEquals(1, h.syncpoint.count());
        assertEquals("02", h.rowType(2), "screen still shows the old rows (no re-read)");
        assertEquals(" ", done.map().row(2).trtsel.data());
        ThisProgCommarea ca = done.commarea().thisProg();
        assertEquals(1, ca.caScreenNum);
        assertTrue(Cob.isSpaces(ca.caRowTrCodeOut[0]), "commarea INITIALIZEd after success");
        assertTrue(Cob.isSpaces(done.commarea().cardDemo().userType));
        assertEquals("COTRTLIC", done.commarea().cardDemo().fromProgram);

        h.pressForMap(Dfhaid.DFHENTER);
        assertEquals("03", h.rowType(2));
    }

    @Test
    void deleteOfParentRowFailsWithDsntiacMessage() {
        Harness h = listed();
        h.table.addChildReference("02");
        h.pressForMap(Dfhaid.DFHENTER, m -> m.row(2).trtsel.receive("D"));
        CicsResponse.SendMapAndReturn r = h.pressForMap(Dfhaid.DFHPF10);
        assertEquals("Please delete associated child records first: SQLCODE: -532 DSNTIAC CD12", h.errmsg());
        assertTrue(r.commarea().thisProg().isDeleteRequested());
        assertTrue(h.table.snapshot().containsKey("02"));
        assertEquals(0, h.syncpoint.count());
    }

    @Test
    void deleteOtherFailureKeepsRequest() {
        Harness h = listed();
        h.pressForMap(Dfhaid.DFHENTER, m -> m.row(5).trtsel.receive("D"));
        h.table.failNext(Statement.DELETE, -904);
        CicsResponse.SendMapAndReturn r = h.pressForMap(Dfhaid.DFHPF10);
        assertEquals("Delete failed with message: SQLCODE: -904 DSNTIAC CD12", h.errmsg());
        assertTrue(r.commarea().thisProg().isDeleteRequested());
    }

    @Test
    void pf10AfterChangingRowSelectionIsTreatedAsEnter() {
        Harness h = listed();
        h.pressForMap(Dfhaid.DFHENTER, m -> m.row(2).trtsel.receive("D"));
        h.pressForMap(Dfhaid.DFHPF10, m -> {
            m.row(2).trtsel.receive(" ");
            m.row(3).trtsel.receive("D");
        });
        assertEquals("Delete HIGHLIGHTED row ? Press F10 to confirm", h.infomsg());
        assertTrue(h.table.snapshot().containsKey("02"));
        assertTrue(h.table.snapshot().containsKey("03"));
    }

    @Test
    void updateAsksForConfirmationThenSavesOnPf10() {
        Harness h = listed();
        CicsResponse.SendMapAndReturn ask = h.pressForMap(Dfhaid.DFHENTER, m -> {
            m.row(1).trtsel.receive("U");
            m.row(1).trtypd.receive("  Purchase Return");
        });
        assertEquals("Update HIGHLIGHTED row. Press F10 to save", h.infomsg());
        assertEquals("Purchase Return", h.rowDesc(1), "trimmed edited value is shown");
        assertEquals(Dfhbmsca.DFHBMFSE, ask.map().row(1).trtypd.attribute);
        assertEquals(-1, ask.map().row(1).trtypd.length);
        assertEquals(Dfhbmsca.DFHNEUTR, ask.map().row(1).trttyp.color);
        assertTrue(ask.commarea().thisProg().isUpdateRequested());
        assertEquals("Purchase", ask.commarea().thisProg().caRowTrDescOut[0].stripTrailing());

        CicsResponse.SendMapAndReturn done = h.pressForMap(Dfhaid.DFHPF10);
        assertEquals("HIGHLIGHTED row was updated", h.infomsg());
        assertEquals(Cob.move("Purchase Return", 50), h.table.description("01"),
                "DCL-TR-DESCRIPTION-LEN = LENGTH OF WS-ROW-TR-DESC-IN = 50");
        assertEquals(1, h.syncpoint.count());
        assertEquals("Purchase Return", h.rowDesc(1));
        assertEquals(" ", done.map().row(1).trtsel.data());
        assertEquals(-1, done.map().row(1).trtsel.length);
        assertEquals(Dfhbmsca.DFHNEUTR, done.map().row(1).trtypd.color);
        assertTrue(done.commarea().thisProg().isUpdateSucceeded());
    }

    @Test
    void updateWithoutChangeIsReported() {
        Harness h = listed();
        CicsResponse.SendMapAndReturn r = h.pressForMap(Dfhaid.DFHENTER, m -> {
            m.row(2).trtsel.receive("U");
            m.row(2).trtypd.receive("PAYMENT");
        });
        assertEquals("No change detected with respect to database values.", h.errmsg());
        assertEquals("Payment", h.rowDesc(2), "database value redisplayed");
        assertTrue(r.commarea().thisProg().isUpdateRequested());
    }

    @Test
    void blankDescriptionIsRequired() {
        Harness h = listed();
        CicsResponse.SendMapAndReturn r = h.pressForMap(Dfhaid.DFHENTER, m -> {
            m.row(3).trtsel.receive("U");
            m.row(3).trtypd.receive("*");
        });
        assertEquals("Transaction Desc must be supplied.", h.errmsg());
        assertEquals("*", h.rowDesc(3));
        assertEquals(Dfhbmsca.DFHRED, r.map().row(3).trtypd.color);
    }

    @Test
    void descriptionMustBeAlphanumeric() {
        Harness h = listed();
        CicsResponse.SendMapAndReturn r = h.pressForMap(Dfhaid.DFHENTER, m -> {
            m.row(3).trtsel.receive("U");
            m.row(3).trtypd.receive("Credit-Note");
        });
        assertEquals("Transaction Desc can have numbers or alphabets only.", h.errmsg());
        assertEquals("Credit-Note", h.rowDesc(3));
        assertEquals(Dfhbmsca.DFHRED, r.map().row(3).trtypd.color);
    }

    @Test
    void updateDeadlockSetsInputError() {
        Harness h = listed();
        h.pressForMap(Dfhaid.DFHENTER, m -> {
            m.row(1).trtsel.receive("U");
            m.row(1).trtypd.receive("Purchase2");
        });
        h.table.failNext(Statement.UPDATE, -911);
        CicsResponse.SendMapAndReturn r = h.pressForMap(Dfhaid.DFHPF10);
        assertEquals("Deadlock. Someone else updating ? SQLCODE: -911 DSNTIAC CD12", h.errmsg());
        assertTrue(r.commarea().thisProg().isUpdateRequested());
        assertEquals("Purchase", h.table.description("01"));
    }

    @Test
    void updateOfVanishedRowReportsNotFound() {
        Harness h = listed();
        h.pressForMap(Dfhaid.DFHENTER, m -> {
            m.row(1).trtsel.receive("U");
            m.row(1).trtypd.receive("Purchase2");
        });
        h.table.failNext(Statement.UPDATE, 100);
        h.pressForMap(Dfhaid.DFHPF10);
        assertEquals("Record not found. Deleted by others ? SQLCODE:  100 DSNTIAC CD12", h.errmsg());
    }

    @Test
    void updateOtherFailure() {
        Harness h = listed();
        h.pressForMap(Dfhaid.DFHENTER, m -> {
            m.row(1).trtsel.receive("U");
            m.row(1).trtypd.receive("Purchase2");
        });
        h.table.failNext(Statement.UPDATE, -803);
        h.pressForMap(Dfhaid.DFHPF10);
        assertEquals("Update failed with SQLCODE: -803 DSNTIAC CD12", h.errmsg());
    }
}
