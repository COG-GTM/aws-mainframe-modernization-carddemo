package com.carddemo.programs.cbpaup0c.ims;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import com.carddemo.programs.cbpaup0c.copybook.CobolCodec;
import com.carddemo.programs.cbpaup0c.copybook.PendingAuthDetails;
import com.carddemo.programs.cbpaup0c.copybook.PendingAuthSummary;

import java.math.BigDecimal;
import org.junit.jupiter.api.Test;

class InMemoryPautDbTest {

    final InMemoryPautDb db = new InMemoryPautDb(CobolCodec.EBCDIC)
            .addSummary(new PendingAuthSummary().accountId(1))
            .addDetail(PendingAuthDetails.of(1, 1, "00", BigDecimal.ONE, BigDecimal.ONE))
            .addDetail(PendingAuthDetails.of(2, 2, "00", BigDecimal.ONE, BigDecimal.ONE))
            .addSummary(new PendingAuthSummary().accountId(2));

    @Test
    void gnpStaysWithinParentAndReportsGe() {
        assertEquals(1, db.getNextSummary().segment().accountId());
        assertEquals(1, db.getNextDetailWithinParent().segment().authDate9c());
        assertEquals(2, db.getNextDetailWithinParent().segment().authDate9c());
        assertEquals("GE", db.getNextDetailWithinParent().status());
        assertEquals("GE", db.getNextDetailWithinParent().status());
        assertEquals(2, db.getNextSummary().segment().accountId());
        assertEquals("GE", db.getNextDetailWithinParent().status());
        DliResult<PendingAuthSummary> end = db.getNextSummary();
        assertEquals("GB", end.status());
        assertNull(end.segment());
    }

    @Test
    void deletingRootCascadesToDependents() {
        db.getNextSummary();
        assertEquals("  ", db.deleteSummary(null));
        assertEquals(1, db.remainingSummaries().size());
        assertEquals(2, db.getNextSummary().segment().accountId());
    }

    @Test
    void deleteWithoutPositionReturnsDj() {
        assertEquals("DJ", db.deleteDetail(null));
        assertEquals("DJ", db.deleteSummary(null));
        db.getNextSummary();
        assertEquals("DJ", db.deleteDetail(null));
    }

    @Test
    void injectedStatusIsReturnedOnTheNthCall() {
        db.injectStatus(DliCall.GN_PAUTSUM0, 2, "AI");
        assertEquals("  ", db.getNextSummary().status());
        assertEquals("AI", db.getNextSummary().status());
        assertEquals(2, db.callCount(DliCall.GN_PAUTSUM0));
    }
}
