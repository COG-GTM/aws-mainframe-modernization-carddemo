package com.carddemo.programs.cotrtlic;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.programs.cotrtlic.copybook.CotrtliMap;
import com.carddemo.programs.cotrtlic.copybook.Dfhbmsca;
import com.carddemo.programs.cotrtlic.copybook.ScreenTitles;
import com.carddemo.programs.cotrtlic.support.Cob;
import org.junit.jupiter.api.Test;

/** First display against the sample data in app/data/ASCII/trantype.txt. */
class CotrtlicFixtureTest {

    @Test
    void firstEntryListsTheSevenSampleTransactionTypes() {
        Harness h = new Harness().withFixture();
        CicsResponse.SendMapAndReturn r = h.start();

        String[][] expected = {
            {"01", "Purchase"}, {"02", "Payment"}, {"03", "Credit"}, {"04", "Authorization"},
            {"05", "Refund"}, {"06", "Reversal"}, {"07", "Adjustment"}};
        for (int i = 1; i <= 7; i++) {
            assertEquals(expected[i - 1][0], h.rowType(i));
            assertEquals(expected[i - 1][1], h.rowDesc(i));
            assertEquals(" ", r.map().row(i).trtsel.data());
            assertEquals(Dfhbmsca.DFHBMFSE, r.map().row(i).trtsel.attribute);
            assertEquals(Dfhbmsca.DFHBMPRF, r.map().row(i).trtypd.attribute);
        }
        assertEquals("CTLI", r.transId());
        assertEquals("CTRTLIA", r.mapName());
        assertEquals("COTRTLI", r.mapset());
        assertEquals("CTLI", r.map().trnname.data());
        assertEquals("COTRTLIC", r.map().pgmname.data());
        assertEquals(ScreenTitles.CCDA_TITLE01, r.map().title01.data());
        assertEquals(ScreenTitles.CCDA_TITLE02, r.map().title02.data());
        assertEquals("03/05/24", r.map().curdate.data());
        assertEquals("14:07:09", r.map().curtime.data());
        assertEquals("1  ", r.map().pageno.data());
        // 2500: centred with WS-STRING-MID = (45 - 40) / 2 + 1 = 3
        assertEquals("  Type U to update, D to delete any record   ", r.map().infomsg.data());
        assertEquals(Dfhbmsca.DFHNEUTR, r.map().infomsg.color);
        assertTrue(Cob.isSpaces(r.map().errmsg.data()));
        assertEquals(-1, r.map().trtype.length, "cursor on type filter");

        ThisProgCommarea ca = r.commarea().thisProg();
        assertEquals("01", ca.caFirstTrCode);
        assertEquals("07", ca.caLastTrCode);
        assertTrue(ca.isNextPageNotExists(), "exactly 7 rows: look-ahead fetch hit +100");
        assertEquals(1, ca.caScreenNum);
        assertEquals("COTRTLIC", r.commarea().cardDemo().fromProgram);
        assertEquals("CTLI", r.commarea().cardDemo().fromTranid);
        assertEquals("CTRTLIA", r.commarea().cardDemo().lastMap);
        assertEquals("COTRTLI", r.commarea().cardDemo().lastMapset);
        assertTrue(r.commarea().cardDemo().isUsrtypAdmin());
        assertTrue(r.commarea().cardDemo().isPgmEnter());
    }

    @Test
    void descriptionFilterOnSampleDataUsesLikeWithPercentWrapping() {
        Harness h = new Harness().withFixture();
        h.start();
        CicsResponse.SendMapAndReturn r = h.pressForMap(com.carddemo.programs.cotrtlic.copybook.Dfhaid.DFHENTER,
                m -> m.trdesc.receive("Re"));
        assertEquals("05", h.rowType(1));
        assertEquals("Refund", h.rowDesc(1));
        assertEquals("06", h.rowType(2));
        assertEquals("Reversal", h.rowDesc(2));
        assertTrue(Cob.isLowValues(h.rowType(3)));
        assertEquals(Dfhbmsca.DFHBMPRO, r.map().row(3).trtsel.attribute);
        assertEquals("Re", r.map().trdesc.data().stripTrailing());
        assertEquals(Dfhbmsca.DFHBMFSE, r.map().trdesc.attribute);
        assertEquals("Re", r.commarea().thisProg().caTypeDesc.stripTrailing());
        assertEquals(CotrtliMap.ROWS, 7);
    }
}
