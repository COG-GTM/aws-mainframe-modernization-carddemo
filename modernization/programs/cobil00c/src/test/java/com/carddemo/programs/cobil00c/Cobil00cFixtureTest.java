package com.carddemo.programs.cobil00c;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.programs.cobil00c.copybook.AccountRecord;
import com.carddemo.programs.cobil00c.copybook.CardDemoCommarea;
import com.carddemo.programs.cobil00c.copybook.CardXrefRecord;
import com.carddemo.programs.cobil00c.copybook.Dfhaid;
import com.carddemo.programs.cobil00c.copybook.TranRecord;
import com.carddemo.programs.cobil00c.gateway.InMemoryAccountDataGateway;
import com.carddemo.programs.cobil00c.gateway.InMemoryCardXrefAixGateway;
import com.carddemo.programs.cobil00c.gateway.InMemoryTransactGateway;
import com.carddemo.programs.cobil00c.io.FixedWidthRecords;
import java.io.IOException;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/** Runs COBIL00C against the CardDemo sample datasets in app/data/ASCII. */
class Cobil00cFixtureTest {

    private static final Path DATA = Path.of(System.getProperty("carddemo.data.dir", "../../../app/data/ASCII"));
    private static final Clock CLOCK = Clock.fixed(Instant.parse("2022-07-19T23:12:32Z"), ZoneOffset.UTC);

    private List<AccountRecord> accounts;
    private List<CardXrefRecord> xrefs;
    private List<TranRecord> trans;
    private InMemoryAccountDataGateway acctdat;
    private InMemoryTransactGateway transact;
    private Cobil00c program;

    @BeforeEach
    void load() throws IOException {
        accounts = FixedWidthRecords.readAll(DATA.resolve("acctdata.txt"), FixedWidthRecords::decodeAccount);
        xrefs = FixedWidthRecords.readAll(DATA.resolve("cardxref.txt"), FixedWidthRecords::decodeCardXref);
        trans = FixedWidthRecords.readAll(DATA.resolve("dailytran.txt"), FixedWidthRecords::decodeTran);
        acctdat = new InMemoryAccountDataGateway(accounts);
        transact = new InMemoryTransactGateway(trans);
        program = new Cobil00c(acctdat, new InMemoryCardXrefAixGateway(xrefs), transact, CLOCK);
    }

    private static CardDemoCommarea reenter() {
        CardDemoCommarea c = new CardDemoCommarea();
        c.fromProgram("COMEN01C");
        c.pgmContext("1");
        return c;
    }

    @Test
    void sampleDatasetsLoad() {
        assertEquals(50, accounts.size());
        assertEquals(50, xrefs.size());
        assertEquals(300, trans.size());
        assertEquals(new BigDecimal("194.00"), accounts.get(0).acctCurrBal());
    }

    @Test
    void accountAndTransactionCodecsRoundTripTheSampleFiles() throws IOException {
        List<String> acctLines = Files.readAllLines(DATA.resolve("acctdata.txt"), StandardCharsets.UTF_8);
        for (int i = 0; i < accounts.size(); i++) {
            assertEquals(acctLines.get(i), FixedWidthRecords.encodeAccount(accounts.get(i)));
        }
        List<String> tranLines = Files.readAllLines(DATA.resolve("dailytran.txt"), StandardCharsets.UTF_8);
        for (int i = 0; i < trans.size(); i++) {
            assertEquals(tranLines.get(i), FixedWidthRecords.encodeTran(trans.get(i)));
        }
        List<String> xrefLines = Files.readAllLines(DATA.resolve("cardxref.txt"), StandardCharsets.UTF_8);
        for (int i = 0; i < xrefs.size(); i++) {
            assertEquals(xrefLines.get(i).stripTrailing(), FixedWidthRecords.encodeCardXref(xrefs.get(i)).stripTrailing());
        }
    }

    @Test
    void inquiryShowsSampleBalance() {
        BillPayResult r = program.execute(BillPayRequest.of(reenter(), Dfhaid.DFHENTER,
                BillPayScreenInput.of("00000000002", " ")));
        BillPayScreen s = r.finalScreen().orElseThrow();
        assertEquals("+0000000158.00", s.curbal());
        assertEquals("Confirm to make a bill payment...", s.message());
    }

    @Test
    void payingSampleAccountOneAppendsAfterHighestDailyTransaction() {
        String maxId = trans.stream().map(TranRecord::tranId).max(String::compareTo).orElseThrow();
        assertEquals("0000000996722787", maxId);

        BillPayResult r = program.execute(BillPayRequest.of(reenter(), Dfhaid.DFHENTER,
                BillPayScreenInput.of("00000000001", "Y")));

        TranRecord t = transact.find("0000000996722788").orElseThrow();
        assertEquals(new BigDecimal("194.00"), t.tranAmt());
        assertEquals("9680294154603697", t.tranCardNum());
        assertEquals("2022-07-19 23:12:32.000000", t.tranOrigTs());
        assertEquals(new BigDecimal("0.00"), acctdat.find("00000000001").orElseThrow().acctCurrBal());
        assertEquals("Payment successful.  Your Transaction ID is 0000000996722788.",
                r.finalScreen().orElseThrow().message());

        String written = FixedWidthRecords.encodeTran(t);
        assertEquals(TranRecord.RECORD_LENGTH, written.length());
        assertTrue(written.startsWith("000000099672278802" + "0002" + "POS TERM  " + "BILL PAYMENT - ONLINE"));
        assertEquals("00000000000{", FixedWidthRecords.encodeAccount(acctdat.find("00000000001").orElseThrow())
                .substring(12, 24));
    }
}
