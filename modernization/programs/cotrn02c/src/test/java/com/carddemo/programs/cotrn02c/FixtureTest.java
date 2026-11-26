package com.carddemo.programs.cotrn02c;

import com.carddemo.programs.cotrn02c.copybook.CardXrefRecord;
import com.carddemo.programs.cotrn02c.copybook.Dfhaid;
import com.carddemo.programs.cotrn02c.copybook.Pic;
import com.carddemo.programs.cotrn02c.copybook.TranRecord;
import com.carddemo.programs.cotrn02c.gateway.InMemoryCardXref;
import com.carddemo.programs.cotrn02c.gateway.InMemoryTransactFile;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

/** Runs COTRN02C against app/data/ASCII/dailytran.txt (as TRANSACT) and cardxref.txt (as CCXREF/CXACAIX). */
class FixtureTest extends TestSupport {

    @BeforeEach
    void loadFixtures() {
        transact = InMemoryTransactFile.load(fixture("dailytran.txt"));
        xref = InMemoryCardXref.load(fixture("cardxref.txt"));
    }

    @Test
    void fixtureRecordsRoundTripByteForByte() {
        List<String> lines = fixture("dailytran.txt").toList();
        assertEquals(300, lines.size());
        lines.forEach(l -> assertEquals(l, TranRecord.parse(l).toRecord()));
        fixture("cardxref.txt").forEach(l -> assertEquals(Pic.x(l, 50), CardXrefRecord.parse(l).toRecord()));
        assertEquals(50, xref.records().size());
    }

    @Test
    void lastFixtureTransactionDecodes() {
        TranRecord last = transact.records().lastEntry().getValue();
        assertEquals("0000000996722787", last.getTranId());
        assertEquals(new BigDecimal("603.22"), last.getAmt());
        assertEquals(800000000L, last.getMerchantId());
        assertEquals("3260763612337560", last.getCardNum());
    }

    @Test
    void addWithFixtureAccountUsesHighestTranIdPlusOne() {
        Cotrn2aResponse r = screen(enter(valid().acctId("00000000027").build()));
        assertEquals("Transaction added successfully.  Your Tran ID is 0000000996722788.", r.message());
        TranRecord w = transact.records().get("0000000996722788");
        assertEquals("0683586198171516", w.getCardNum());
        assertEquals(301, transact.records().size());
    }

    @Test
    void copyLastFixtureTransactionStopsAtEmptyProcDate() {
        Cotrn2aResponse r = screen(service().execute(reenterCommarea("COTRN00C"), Dfhaid.DFHPF5,
                Cotrn2aRequest.builder().cardNum("0500024453765740").build()));
        assertEquals("00000000050", r.acctId());
        assertEquals("01", r.typeCd());
        assertEquals("0001", r.catCd());
        assertEquals("+00000603.22", r.amount());
        assertEquals(Pic.x("Purchase at Kilback LLC", 60), r.desc());
        assertEquals("2022-06-10", r.origDate());
        assertEquals(Pic.spaces(10), r.procDate());
        assertEquals("Proc Date can NOT be empty...", r.message());
    }
}
