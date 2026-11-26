package com.carddemo.programs.cotrtupc;

import com.carddemo.programs.cotrtupc.copybook.CarddemoCommarea;
import com.carddemo.programs.cotrtupc.copybook.Dfhaid;
import com.carddemo.programs.cotrtupc.copybook.Dfhbmsca;
import com.carddemo.programs.cotrtupc.copybook.Sqlca;
import com.carddemo.programs.cotrtupc.gateway.InMemoryTransactionTypeGateway.Statement;
import com.carddemo.programs.cotrtupc.support.CobolText;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/** SQLCODE handling in 9100 / 9600 / 9700 / 9800, ABEND-ROUTINE and HANDLE ABEND. */
class CotrtupcSqlErrorTest {

    private static String errm(Sqlca s) {
        return s.sqlerrmGroup();
    }

    private static Terminal validated() {
        Terminal t = Terminal.withSeedData().start();
        t.show("01");
        t.press(Dfhaid.DFHENTER, "01", "Purchase Retail");
        assertEquals('N', t.state());
        return t;
    }

    @Test
    void selectErrorShowsSqlcodeAndSqlerrm() {
        Terminal t = Terminal.withSeedData().start();
        Sqlca err = Sqlca.of(-805, "DSNXXX.COTRTUPC", "51002");
        t.db.inject(Statement.SELECT, err);
        t.show("01");

        String expected = CobolText.pic("Error accessing: TRANSACTION_TYPE table. SQLCODE: -805:" + errm(err), 78);
        assertEquals(expected, t.errmsgRaw());
        assertEquals('X', t.state());
        assertEquals(Dfhbmsca.DFHRED, t.last.screen().trtypcd.color);
    }

    @Test
    void updateLockTimeoutIsReportedAndResetsOnNextEnter() {
        Terminal t = validated();
        t.db.inject(Statement.UPDATE, Sqlca.of(-911, "00C9008E", "40001"));
        t.pressResending(Dfhaid.DFHPF5);

        assertEquals('L', t.state());
        assertEquals("Could not lock record for update", t.error());
        assertEquals("Changes unsuccessful", t.info());
        assertEquals(0, t.db.syncpoints());
        assertEquals("PURCHASE", t.db.rows().get("01"));

        t.pressResending(Dfhaid.DFHENTER);
        assertEquals(CobolText.LOW_VALUE, t.state());
        assertEquals("Enter transaction type to be maintained", t.info());
    }

    @Test
    void otherUpdateErrorOverwritesFailureMessageSoStateBecomesDone() {
        Terminal t = validated();
        Sqlca err = Sqlca.of(-904, "00C90081", "57011");
        t.db.inject(Statement.UPDATE, err);
        t.pressResending(Dfhaid.DFHPF5);

        // STRING overwrites 'Update of record failed' so TABLE-UPDATE-FAILED is no longer true -> WHEN OTHER
        String expected = CobolText.pic(
                ("Error updating: TRANSACTION_TYPE Table. SQLCODE: -904:" + errm(err)).substring(0, 75), 78);
        assertEquals(expected, t.errmsgRaw());
        assertEquals('C', t.state());
        assertEquals("Changes committed to database", t.info());
        assertEquals(0, t.db.syncpoints());
    }

    @Test
    void insertErrorReportsSqlcodeOfThePrecedingUpdate() {
        Terminal t = Terminal.withSeedData().start();
        t.show("99");
        t.press(Dfhaid.DFHPF5, "99", null);
        t.press(Dfhaid.DFHENTER, "99", "Loyalty Points");
        Sqlca err = Sqlca.of(-803, "1 CARDDEMO.TRANSACTION_TYPE", "23505");
        t.db.inject(Statement.INSERT, err);
        t.pressResending(Dfhaid.DFHPF5);

        String expected = CobolText.pic(
                ("Error inserting record into: TRANSACTION_TYPE Table. SQLCODE:  100:" + errm(err)).substring(0, 75), 78);
        assertEquals(expected, t.errmsgRaw());
        assertEquals('C', t.state());
        assertTrue(!t.db.rows().containsKey("99"));
    }

    @Test
    void deleteWithChildRowsKeepsStartDeleteState() {
        Terminal t = Terminal.withSeedData().start();
        t.db.addCategory(com.carddemo.programs.cotrtupc.copybook.DclTransactionTypeCategory.of("01", "0001",
                "Regular Sales Draft"));
        t.show("01");
        t.press(Dfhaid.DFHPF4, "01", "PURCHASE");
        t.press(Dfhaid.DFHPF4, "01", "PURCHASE");

        Sqlca err = Sqlca.of(-532, "TRC_TYPE_CODE", "23504");
        String expected = CobolText.pic(
                ("Please delete associated child records first:SQLCODE : -532:" + errm(err) + errm(err)).substring(0, 75), 78);
        assertEquals(expected, t.errmsgRaw());
        assertEquals('8', t.state());
        assertEquals("Enter transaction type to be maintained", t.info());
        assertEquals(CobolText.pic("PURCHASE", 50), t.last.screen().trtydsc.data);
        assertTrue(t.db.rows().containsKey("01"));
        assertEquals(0, t.db.syncpoints());
    }

    @Test
    void otherDeleteErrorMarksDeleteFailed() {
        Terminal t = Terminal.withSeedData().start();
        t.show("01");
        t.press(Dfhaid.DFHPF4, "01", "PURCHASE");
        Sqlca err = Sqlca.of(-904, "00C90081", "57011");
        t.db.inject(Statement.DELETE, err);
        t.press(Dfhaid.DFHPF4, "01", "PURCHASE");

        String expected = CobolText.pic(
                ("Delete failed with message:SQLCODE : -904:" + errm(err)).substring(0, 75), 78);
        assertEquals(expected, t.errmsgRaw());
        assertEquals('6', t.state());
        assertEquals("Changes unsuccessful", t.info());
        assertEquals(Dfhbmsca.DFHRED, t.last.screen().trtypcd.color);

        t.press(Dfhaid.DFHENTER);
        assertEquals(CobolText.LOW_VALUE, t.state());
    }

    @Test
    void unexpectedStateAbendsWith9999() {
        // TTUP-REVIEW-NEW-RECORD ('V') is never set by the program; with unchanged data it reaches 2000 WHEN OTHER
        CarddemoCommarea c = new CarddemoCommarea();
        c.initialize();
        c.fromTranid("CTTU");
        c.fromProgram("COTRTUPC");
        c.setPgmReenter();
        TtupProgCommarea p = new TtupProgCommarea();
        p.initialize();
        p.changeAction('V');
        p.oldType("01");
        p.oldDesc("PURCHASE");
        Terminal t = Terminal.withSeedData();
        t.commarea = c.toFixed() + p.toFixed();

        CicsAbendException e = assertThrows(CicsAbendException.class,
                () -> t.press(Dfhaid.DFHENTER, "01", "PURCHASE"));

        assertEquals("9999", e.abendCode());
        assertEquals("COTRTUPC", e.abendData().abendCulprit());
        assertEquals(CobolText.spaces(50), e.abendData().abendReason());
        assertEquals(CobolText.pic("UNEXPECTED DATA SCENARIO", 72), e.abendData().abendMsg());
        assertEquals(134, e.sentText().length());
    }

    @Test
    void failedSyncpointAbendsAndBacksOutTheUpdate() {
        Terminal t = validated();
        t.db.failSyncpoint(new IllegalStateException("commit failed"));

        CicsAbendException e = assertThrows(CicsAbendException.class, () -> t.pressResending(Dfhaid.DFHPF5));

        assertEquals("9999", e.abendCode());
        assertEquals("PURCHASE", t.db.rows().get("01"));
        assertEquals(1, t.db.rollbacks());
        assertEquals(java.util.List.of("SELECT", "UPDATE", "SYNCPOINT", "ROLLBACK"), t.db.log());
    }

    @Test
    void failedSyncpointBacksOutDelete() {
        Terminal t = Terminal.withSeedData().start();
        t.show("06");
        t.press(Dfhaid.DFHPF4, "06", "REVERAL");
        t.db.failSyncpoint(new IllegalStateException("commit failed"));

        assertThrows(CicsAbendException.class, () -> t.press(Dfhaid.DFHPF4, "06", "REVERAL"));

        assertEquals("REVERAL", t.db.rows().get("06"));
    }

    @Test
    void runtimeFailureInsideSqlCallGoesToAbendRoutine() {
        Terminal t = Terminal.withSeedData().start();
        t.db.fail(Statement.SELECT, new IllegalStateException("DB2 connection lost"));

        CicsAbendException e = assertThrows(CicsAbendException.class, () -> t.show("01"));

        assertEquals("9999", e.abendCode());
        assertEquals("COTRTUPC", e.abendData().abendCulprit());
        // ABEND-MSG has VALUE SPACES (not LOW-VALUES), so the default text is not applied
        assertEquals(CobolText.spaces(72), e.abendData().abendMsg());
        assertEquals("DB2 connection lost", e.getCause().getMessage());
    }
}
