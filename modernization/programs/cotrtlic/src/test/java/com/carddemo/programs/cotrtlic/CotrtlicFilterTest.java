package com.carddemo.programs.cotrtlic;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.programs.cotrtlic.copybook.Dfhaid;
import com.carddemo.programs.cotrtlic.copybook.Dfhbmsca;
import com.carddemo.programs.cotrtlic.support.Cob;
import org.junit.jupiter.api.Test;

/** 1220-EDIT-TYPECD, 1230-EDIT-DESC, 1290-CROSS-EDITS / 9100-CHECK-FILTERS and 2400 attributes. */
class CotrtlicFilterTest {

    @Test
    void validTypeFilterShowsOnlyThatTypeAndResetsPaging() {
        Harness h = new Harness().withRows(1, 10);
        h.start();
        h.pressForMap(Dfhaid.DFHPF8);
        CicsResponse.SendMapAndReturn r = h.pressForMap(Dfhaid.DFHENTER, m -> m.trtype.receive("03"));
        assertEquals("03", h.rowType(1));
        assertTrue(r.commarea().thisProg().eachRowOutIsLowValues(2));
        assertEquals("03", r.map().trtype.data());
        assertEquals(Dfhbmsca.DFHBMFSE, r.map().trtype.attribute);
        assertEquals("03", r.commarea().thisProg().caTypeCd);
        assertEquals(1, r.commarea().thisProg().caScreenNum, "paging variables re-initialised");
    }

    @Test
    void nonNumericTypeFilterIsRejected() {
        Harness h = new Harness().withFixture();
        h.start();
        CicsResponse.SendMapAndReturn r = h.pressForMap(Dfhaid.DFHENTER, m -> m.trtype.receive("A1"));
        assertEquals("TYPE CODE FILTER,IF SUPPLIED MUST BE A 2 DIGIT NUMBER", h.errmsg());
        assertEquals(Dfhbmsca.DFHRED, r.map().trtype.color);
        assertEquals(-1, r.map().trtype.length);
        assertEquals("A1", r.map().trtype.data());
        assertEquals(Dfhbmsca.DFHBMPRO, r.map().row(1).trtsel.attribute, "select column protected");
        assertEquals("01", h.rowType(1), "rows redisplayed from commarea, no read");
    }

    @Test
    void singleDigitTypeFilterIsNotNumeric() {
        Harness h = new Harness().withFixture();
        h.start();
        h.pressForMap(Dfhaid.DFHENTER, m -> m.trtype.receive("3"));
        assertEquals("TYPE CODE FILTER,IF SUPPLIED MUST BE A 2 DIGIT NUMBER", h.errmsg());
    }

    @Test
    void zeroTypeFilterMeansNoFilter() {
        Harness h = new Harness().withFixture();
        h.start();
        CicsResponse.SendMapAndReturn r = h.pressForMap(Dfhaid.DFHENTER, m -> m.trtype.receive("00"));
        assertEquals("01", h.rowType(1));
        assertEquals("07", h.rowType(7));
        assertTrue(Cob.isLowValues(r.map().trtype.data()));
    }

    @Test
    void typeFilterWithoutMatchesIsRejected() {
        Harness h = new Harness().withFixture();
        h.start();
        CicsResponse.SendMapAndReturn r = h.pressForMap(Dfhaid.DFHENTER, m -> m.trtype.receive("99"));
        assertEquals("No Records found for these filter conditions", h.errmsg());
        assertEquals(Dfhbmsca.DFHRED, r.map().trtype.color);
        assertEquals(Dfhbmsca.DFHBMPRO, r.map().row(1).trtsel.attribute);
    }

    @Test
    void descriptionFilterWithoutMatchesIsRejected() {
        Harness h = new Harness().withFixture();
        h.start();
        CicsResponse.SendMapAndReturn r = h.pressForMap(Dfhaid.DFHENTER, m -> m.trdesc.receive("zzz"));
        assertEquals("No Records found for these filter conditions", h.errmsg());
        assertEquals(Dfhbmsca.DFHRED, r.map().trdesc.color);
        assertEquals(-1, r.map().trdesc.length);
        assertEquals(Dfhbmsca.DFHBMFSE, r.map().trdesc.attribute);
    }

    @Test
    void combinedFiltersAreAnded() {
        Harness h = new Harness().withFixture();
        h.start();
        h.pressForMap(Dfhaid.DFHENTER, m -> {
            m.trtype.receive("02");
            m.trdesc.receive("Pay");
        });
        assertEquals("02", h.rowType(1));
        assertEquals("Payment", h.rowDesc(1));
        assertTrue(Cob.isLowValues(h.rowType(2)));
    }

    @Test
    void countFailureFormatsDb2MessageThenIsOverwrittenByNoRecordsMessage() {
        Harness h = new Harness().withFixture();
        h.start();
        h.table.failNext(com.carddemo.programs.cotrtlic.gateway.InMemoryTransactionTypeTable.Statement.COUNT, -904);
        h.pressForMap(Dfhaid.DFHENTER, m -> m.trtype.receive("01"));
        assertEquals("No Records found for these filter conditions", h.errmsg());
    }

    @Test
    void unchangedDescriptionFilterKeepsPagingAndUpdateConfirmation() {
        Harness h = new Harness().withRows(1, 20);
        h.start();
        h.pressForMap(Dfhaid.DFHENTER, m -> m.trdesc.receive("Type"));
        assertEquals("01", h.rowType(1));
        CicsResponse.SendMapAndReturn p2 = h.pressForMap(Dfhaid.DFHPF8);
        assertEquals("08", h.rowType(1));
        assertEquals(2, p2.commarea().thisProg().caScreenNum);
        h.pressForMap(Dfhaid.DFHENTER, m -> {
            m.row(1).trtsel.receive("U");
            m.row(1).trtypd.receive("Type eight");
        });
        assertEquals("Update HIGHLIGHTED row. Press F10 to save", h.infomsg());
        h.pressForMap(Dfhaid.DFHPF10);
        assertEquals("HIGHLIGHTED row was updated", h.infomsg());
        assertEquals("Type eight", h.table.description("08").stripTrailing());
    }
}
