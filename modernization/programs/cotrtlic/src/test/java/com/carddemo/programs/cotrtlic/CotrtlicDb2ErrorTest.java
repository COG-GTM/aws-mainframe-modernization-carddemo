package com.carddemo.programs.cotrtlic;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.programs.cotrtlic.copybook.Dfhaid;
import com.carddemo.programs.cotrtlic.gateway.InMemoryTransactionTypeTable.Statement;
import org.junit.jupiter.api.Test;

/** 9998-PRIMING-QUERY, cursor open/fetch/close failures and 9999-FORMAT-DB2-MESSAGE. */
class CotrtlicDb2ErrorTest {

    @Test
    void primingQueryFailureSendsLongTextAndEndsConversation() {
        Harness h = new Harness().withFixture();
        h.table.failNext(Statement.PRIMING, -904);
        CicsResponse resp = h.send(CicsRequest.firstEntry(Dfhaid.DFHENTER));
        CicsResponse.SendTextAndReturn t = assertInstanceOf(CicsResponse.SendTextAndReturn.class, resp);
        assertEquals(800, t.text().length());
        assertEquals("Db2 access failure. SQLCODE: -904 DSNTIAC CD12", t.text().stripTrailing());
    }

    @Test
    void forwardOpenFailureShowsMessageAndNoRows() {
        Harness h = new Harness().withFixture();
        h.table.failNext(Statement.OPEN_FORWARD, -904);
        CicsResponse.SendMapAndReturn r = h.start();
        assertEquals("C-TR-TYPE-FORWARD Open SQLCODE: -904 DSNTIAC CD12", h.errmsg());
        assertTrue(r.commarea().thisProg().eachRowOutIsLowValues(1));
    }

    @Test
    void forwardFetchFailureUsesTheCloseActionText() {
        Harness h = new Harness().withFixture();
        h.table.failNext(Statement.FETCH_FORWARD, -805);
        h.start();
        assertEquals("C-TR-TYPE-FORWARD close SQLCODE: -805 DSNTIAC CD12", h.errmsg());
    }

    @Test
    void lookAheadFetchFailureIsReportedAsFetchWithoutDb2ErrorFlag() {
        Harness h = new Harness().withRows(1, 9);
        h.table.failOnCall(Statement.FETCH_FORWARD, 8, -805);
        CicsResponse.SendMapAndReturn r = h.start();
        assertEquals("C-TR-TYPE-FORWARD fetch SQLCODE: -805 DSNTIAC CD12", h.errmsg());
        assertEquals("07", h.rowType(7));
        assertEquals("07", r.commarea().thisProg().caLastTrCode);
        assertTrue(r.commarea().thisProg().isNextPageExists(), "indicator left at 'Y'");
    }

    @Test
    void forwardCloseFailureIsReported() {
        Harness h = new Harness().withFixture();
        h.table.failNext(Statement.CLOSE_FORWARD, -501);
        h.start();
        assertEquals("C-TR-TYPE-FORWARD close SQLCODE: -501 DSNTIAC CD12", h.errmsg());
        assertEquals("01", h.rowType(1));
    }

    @Test
    void backwardFetchFailureIsReported() {
        Harness h = new Harness().withRows(1, 20);
        h.start();
        h.pressForMap(Dfhaid.DFHPF8);
        h.pressForMap(Dfhaid.DFHPF8);
        h.table.failNext(Statement.FETCH_BACKWARD, -805);
        h.pressForMap(Dfhaid.DFHPF7);
        assertEquals("Error on fetch Cursor C-TR-TYPE-BACKWARD SQLCODE: -805 DSNTIAC CD12", h.errmsg());
    }

    @Test
    void backwardOpenFailureIsReported() {
        Harness h = new Harness().withRows(1, 20);
        h.start();
        h.pressForMap(Dfhaid.DFHPF8);
        h.pressForMap(Dfhaid.DFHPF8);
        h.table.failNext(Statement.OPEN_BACKWARD, -904);
        h.pressForMap(Dfhaid.DFHPF7);
        assertEquals("C-TR-TYPE-BACKWARD Open SQLCODE: -904 DSNTIAC CD12", h.errmsg());
    }

    @Test
    void backwardReadHittingEndOfDataIsTreatedAsError() {
        Harness h = new Harness().withRows(1, 20);
        h.start();
        h.pressForMap(Dfhaid.DFHPF8);
        h.pressForMap(Dfhaid.DFHPF8);
        h.table.snapshot().keySet().stream().filter(k -> k.compareTo("12") < 0).toList()
                .forEach(k -> h.table.deleteByType(k));
        h.pressForMap(Dfhaid.DFHPF7);
        assertEquals("Error on fetch Cursor C-TR-TYPE-BACKWARD SQLCODE:  100 DSNTIAC CD12", h.errmsg());
    }
}
