package com.carddemo.programs.coactvwc;

import static org.junit.jupiter.api.Assertions.*;

import com.carddemo.programs.coactvwc.CoactvwcResult.SendMapAndReturn;
import com.carddemo.programs.coactvwc.copybook.AccountRecord;
import com.carddemo.programs.coactvwc.copybook.CactvwaInput;
import com.carddemo.programs.coactvwc.copybook.CactvwaOutput;
import com.carddemo.programs.coactvwc.copybook.CardDemoCommarea;
import com.carddemo.programs.coactvwc.copybook.CardXrefRecord;
import com.carddemo.programs.coactvwc.copybook.CustomerRecord;
import com.carddemo.programs.coactvwc.copybook.DfhAid;
import com.carddemo.programs.coactvwc.repository.inmemory.FixedWidthLoader;
import com.carddemo.programs.coactvwc.repository.inmemory.InMemoryAccountRepository;
import com.carddemo.programs.coactvwc.repository.inmemory.InMemoryCardXrefRepository;
import com.carddemo.programs.coactvwc.repository.inmemory.InMemoryCustomerRepository;
import com.carddemo.programs.coactvwc.support.Cobol;
import java.math.BigDecimal;
import java.net.URISyntaxException;
import java.nio.file.Path;
import java.util.List;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

/** Runs the program against the sample extracts copied from app/data/ASCII (acctdata, custdata, cardxref). */
class FixtureDataTest {

    static List<AccountRecord> accounts;
    static List<CustomerRecord> customers;
    static List<CardXrefRecord> xrefs;
    static CoactvwcService service;

    @BeforeAll
    static void load() throws URISyntaxException {
        accounts = FixedWidthLoader.load(fixture("acctdata.txt"), AccountRecord::parse);
        customers = FixedWidthLoader.load(fixture("custdata.txt"), CustomerRecord::parse);
        xrefs = FixedWidthLoader.load(fixture("cardxref.txt"), CardXrefRecord::parse);
        service = new CoactvwcService(new InMemoryCardXrefRepository(xrefs), new InMemoryAccountRepository(accounts),
                new InMemoryCustomerRepository(customers), CoactvwcServiceTest.CLOCK);
    }

    static Path fixture(String name) throws URISyntaxException {
        return Path.of(FixtureDataTest.class.getResource("/fixtures/" + name).toURI());
    }

    static SendMapAndReturn view(String acctId) {
        CardDemoCommarea c = CardDemoCommarea.initialized();
        c.setPgmContext(CardDemoCommarea.PgmContext.REENTER);
        return (SendMapAndReturn) service.process(
                new CoactvwcRequest(DfhAid.DFHENTER, c.toString(), CactvwaInput.ofAcctsid(acctId)));
    }

    @Test
    void fixturesParse() {
        assertEquals(50, accounts.size());
        assertEquals(50, customers.size());
        assertEquals(50, xrefs.size());
        AccountRecord a1 = accounts.get(0);
        assertEquals(1L, a1.acctId());
        assertEquals(new BigDecimal("194.00"), a1.acctCurrBal());
        assertEquals(new BigDecimal("2020.00"), a1.acctCreditLimit());
        assertEquals(new BigDecimal("1020.00"), a1.acctCashCreditLimit());
    }

    @Test
    void viewAccountOneFromSampleData() {
        SendMapAndReturn r = view("00000000001");
        CactvwaOutput s = r.screen();
        assertEquals(Cobol.spaces(78), s.errmsgo());
        assertEquals("Y", s.acsttuso());
        assertEquals("+        194.00", s.acurbalo());
        assertEquals("+      2,020.00", s.acrdlimo());
        assertEquals("+      1,020.00", s.acshlimo());
        assertEquals("+           .00", s.acrcycro());
        assertEquals("+           .00", s.acrcydbo());
        assertEquals("2014-11-20", s.adtopeno());
        assertEquals("2025-05-20", s.aexpdto());
        assertEquals("2025-05-20", s.areisdto());
        assertEquals("000000001", s.acstnumo());
        assertEquals("Immanuel", s.acsfnamo().strip());
        assertEquals("Kessler", s.acslnamo().strip());
        assertEquals("020-97-3888\0", s.acstssno());
        assertEquals("1961-06-08", s.acstdobo());
        assertEquals("274", s.acstfcoo());
        assertEquals("(908)119-8310", s.acsphn1o());
        assertEquals("12546", s.acszipco());
        assertEquals("NC", s.acsstteo());
        assertEquals("USA", s.acsctryo());
        assertEquals("Y", s.acspflgo());
        assertEquals(9680294154603697L, r.carddemoCommarea().cardNum());
    }

    @Test
    void everySampleAccountResolvesWithoutError() {
        for (AccountRecord a : accounts) {
            SendMapAndReturn r = view(Cobol.digits(a.acctId(), 11));
            assertEquals(Cobol.spaces(78), r.screen().errmsgo(), "acct " + a.acctId());
            assertNotNull(r.screen().acstnumo(), "acct " + a.acctId());
        }
    }
}
