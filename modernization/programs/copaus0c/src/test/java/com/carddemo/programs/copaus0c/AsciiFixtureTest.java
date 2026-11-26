package com.carddemo.programs.copaus0c;

import com.carddemo.programs.copaus0c.copybook.CardDemoCommarea;
import com.carddemo.programs.copaus0c.copybook.Copau0aInput;
import com.carddemo.programs.copaus0c.copybook.Copau0aScreen;
import com.carddemo.programs.copaus0c.copybook.Dfh.Aid;
import com.carddemo.programs.copaus0c.inmemory.InMemoryAccountRepository;
import com.carddemo.programs.copaus0c.inmemory.InMemoryCardXrefRepository;
import com.carddemo.programs.copaus0c.inmemory.InMemoryCustomerRepository;
import com.carddemo.programs.copaus0c.inmemory.InMemoryPendingAuthDb;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

/** Runs COPAUS0C against the CardDemo sample files in app/data/ASCII (ACCTDAT, CUSTDAT, CXACAIX). */
class AsciiFixtureTest {

    private static final Path DATA = Path.of("../../../app/data/ASCII");

    @Test
    void account1FromSampleData() throws Exception {
        InMemoryPendingAuthDb ims = new InMemoryPendingAuthDb();
        ims.addSummary(new com.carddemo.programs.copaus0c.copybook.PendingAuthSummary(1, 1, "A",
                List.of("  ", "  ", "  ", "  ", "  "), new BigDecimal("2020.00"), new BigDecimal("1020.00"),
                new BigDecimal("194.00"), BigDecimal.ZERO, 1, 0, new BigDecimal("194.00"), BigDecimal.ZERO));
        ims.addDetail(1, TestData.detail(1, "00", new BigDecimal("194.00")));
        Copaus0cService service = new Copaus0cService(
                InMemoryCardXrefRepository.load(DATA.resolve("cardxref.txt")),
                InMemoryAccountRepository.load(DATA.resolve("acctdata.txt")),
                InMemoryCustomerRepository.load(DATA.resolve("custdata.txt")),
                ims, TestData.CLOCK);

        CardDemoCommarea ca = service.execute(Aid.ENTER, null, null).commarea();
        Copaus0cResult r = service.execute(Aid.ENTER, ca, Copau0aInput.of("00000000001"));
        Copau0aScreen s = r.screen();

        assertEquals(1, r.screens().size());
        assertEquals("000000001", r.commarea().cdemoCustId);
        assertEquals("9680294154603697", r.commarea().cdemoCardNum);
        assertEquals("000000001", s.custid());
        assertEquals("Immanuel M Kessler" + "\0".repeat(7), s.cname());
        assertEquals("618 Deshaun Route,Apt. 80", s.addr001()); // STRING overflows ADDR001O X(25)
        assertEquals("Altenwerthshire,NC,12546" + "\0", s.addr002());
        assertEquals("(908)119-8310", s.phone1());
        assertEquals("     2020.00", s.credlim());
        assertEquals("  1020.00", s.cashlim());
        assertEquals("001", s.apprcnt());
        assertEquals("     194.00", s.row(1).pamt().substring(1));
        assertEquals(String.format("%-78s", ""), s.errmsg());
    }

    @Test
    void everySampleAccountRecordParses() throws Exception {
        InMemoryAccountRepository accounts = InMemoryAccountRepository.load(DATA.resolve("acctdata.txt"));
        assertEquals(new BigDecimal("194.00"), accounts.read("00000000001").record().acctCurrBal());
        InMemoryCustomerRepository customers = InMemoryCustomerRepository.load(DATA.resolve("custdata.txt"));
        assertEquals("Immanuel                 ", customers.read("000000001").record().custFirstName());
    }
}
