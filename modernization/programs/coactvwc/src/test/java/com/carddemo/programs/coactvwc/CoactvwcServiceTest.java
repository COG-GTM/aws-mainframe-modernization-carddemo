package com.carddemo.programs.coactvwc;

import static org.junit.jupiter.api.Assertions.*;

import com.carddemo.programs.coactvwc.CoactvwcResult.Abend;
import com.carddemo.programs.coactvwc.CoactvwcResult.SendMapAndReturn;
import com.carddemo.programs.coactvwc.CoactvwcResult.SendTextAndReturn;
import com.carddemo.programs.coactvwc.CoactvwcResult.Xctl;
import com.carddemo.programs.coactvwc.copybook.AccountRecord;
import com.carddemo.programs.coactvwc.copybook.CactvwaInput;
import com.carddemo.programs.coactvwc.copybook.CactvwaOutput;
import com.carddemo.programs.coactvwc.copybook.CardDemoCommarea;
import com.carddemo.programs.coactvwc.copybook.CardXrefRecord;
import com.carddemo.programs.coactvwc.copybook.CustomerRecord;
import com.carddemo.programs.coactvwc.copybook.DfhAid;
import com.carddemo.programs.coactvwc.copybook.DfhBmsca;
import com.carddemo.programs.coactvwc.repository.CicsResp;
import com.carddemo.programs.coactvwc.repository.FileReadResult;
import com.carddemo.programs.coactvwc.repository.inmemory.InMemoryAccountRepository;
import com.carddemo.programs.coactvwc.repository.inmemory.InMemoryCardXrefRepository;
import com.carddemo.programs.coactvwc.repository.inmemory.InMemoryCustomerRepository;
import com.carddemo.programs.coactvwc.support.Cobol;
import java.math.BigDecimal;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.List;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class CoactvwcServiceTest {

    static final Clock CLOCK = Clock.fixed(Instant.parse("2026-11-26T06:22:33Z"), ZoneOffset.UTC);

    static final AccountRecord ACCT = new AccountRecord(12345678901L, "Y", new BigDecimal("-1234.56"),
            new BigDecimal("5000.00"), new BigDecimal("1500.00"), "2020-01-15", "2027-01-31", "2025-01-31",
            new BigDecimal("100.10"), new BigDecimal("1234567890.12"), "98765", "PLATINUM");
    static final CardXrefRecord XREF = new CardXrefRecord("4111222233334444", 777000111L, 12345678901L);
    static final CustomerRecord CUST = new CustomerRecord(777000111L, "Ada", "Q", "Lovelace", "1 Test Street",
            "Unit 2", "Testville", "NY", "USA", "10001-1234", "(555)010-0000ext", "(555)010-0001", 123456789L,
            "GOV-ID-1", "1970-12-10", "EFT0000001", "Y", 7);

    InMemoryCardXrefRepository xrefs;
    InMemoryAccountRepository accounts;
    InMemoryCustomerRepository customers;
    CoactvwcService service;

    @BeforeEach
    void setUp() {
        xrefs = new InMemoryCardXrefRepository(List.of(XREF));
        accounts = new InMemoryAccountRepository(List.of(ACCT));
        customers = new InMemoryCustomerRepository(List.of(CUST));
        service = new CoactvwcService(xrefs, accounts, customers, CLOCK);
    }

    static String commarea(CardDemoCommarea c, String prog) {
        return c.toString() + Cobol.alnum(prog, 12);
    }

    static CardDemoCommarea reenterCommarea() {
        CardDemoCommarea c = CardDemoCommarea.initialized();
        c.setFromTranid("CM00");
        c.setFromProgram("COMEN01C");
        c.setUserId("USER0001");
        c.setUserType(CardDemoCommarea.UserType.ADMIN);
        c.setPgmContext(CardDemoCommarea.PgmContext.REENTER);
        return c;
    }

    SendMapAndReturn enter(String acctsid) {
        CoactvwcResult r = service.process(new CoactvwcRequest(DfhAid.DFHENTER, commarea(reenterCommarea(), "PROGRAM1TRN1"),
                CactvwaInput.ofAcctsid(acctsid)));
        return assertInstanceOf(SendMapAndReturn.class, r);
    }

    static String err(SendMapAndReturn r) {
        return r.screen().errmsgo().stripTrailing();
    }

    // ---- 0000-MAIN: first entry / context handling ---------------------------------------------------

    @Test
    void firstEntryWithoutCommareaShowsPromptAndReturnsTransid() {
        SendMapAndReturn r = assertInstanceOf(SendMapAndReturn.class,
                service.process(new CoactvwcRequest(DfhAid.DFHENTER, null, null)));
        CactvwaOutput s = r.screen();
        assertEquals("CAVW", r.transId());
        assertEquals("COACTVW", r.mapset());
        assertEquals("CACTVWA", r.map());
        assertEquals("CAVW", s.trnnameo());
        assertEquals("COACTVWC", s.pgmnameo());
        assertEquals("11/26/26", s.curdateo());
        assertEquals("06:22:33", s.curtimeo());
        assertEquals("      AWS Mainframe Modernization       ", s.title01o());
        assertEquals(Cobol.alnum("Enter or update id of account to display", 45), s.infomsgo());
        assertEquals(Cobol.spaces(78), s.errmsgo());
        assertNull(s.acctsido());
        assertEquals(DfhBmsca.DFHBMFSE, s.acctsida());
        assertEquals(DfhBmsca.DFHDFCOL, s.acctsidc());
        assertEquals(DfhBmsca.DFHNEUTR, s.infomsgc());
        assertTrue(s.cursorOnAcctsid());
        assertEquals(CoactvwcService.WS_COMMAREA_LEN, r.commarea().length());
        assertTrue(r.carddemoCommarea().isPgmReenter());
        assertEquals(Cobol.spaces(12), r.thisProgCommarea().toString());
        assertTrue(Cobol.isSpaces(r.commarea().substring(172)));
    }

    @Test
    void enterContextFromMenuKeepsCallerCommareaAndShowsBlankScreen() {
        CardDemoCommarea c = reenterCommarea();
        c.setPgmContext(CardDemoCommarea.PgmContext.ENTER);
        SendMapAndReturn r = assertInstanceOf(SendMapAndReturn.class,
                service.process(new CoactvwcRequest(DfhAid.DFHENTER, commarea(c, "PROGRAM1TRN1"), null)));
        // The "from menu" re-initialise test reads CARDDEMO-COMMAREA before it is loaded, so the caller's data stays.
        assertEquals("USER0001", r.carddemoCommarea().userId());
        assertEquals("PROGRAM1TRN1", r.thisProgCommarea().toString());
        assertTrue(r.carddemoCommarea().isPgmReenter());
        assertNull(r.screen().acctsido());
        assertNull(r.screen().acstnumo());
        assertEquals(DfhBmsca.DFHDFCOL, r.screen().acctsidc());
        assertEquals(Cobol.alnum("Enter or update id of account to display", 45), r.screen().infomsgo());
    }

    @Test
    void unexpectedContextSendsPlainText() {
        String raw = reenterCommarea().toString();
        raw = raw.substring(0, 33) + "7" + raw.substring(34);
        CoactvwcResult r = service.process(new CoactvwcRequest(DfhAid.DFHENTER, raw, null));
        SendTextAndReturn t = assertInstanceOf(SendTextAndReturn.class, r);
        assertEquals(Cobol.alnum("UNEXPECTED DATA SCENARIO", 75), t.text());
    }

    @Test
    void shortCommareaIsPaddedNotRejected() {
        String raw = reenterCommarea().toString().substring(0, 40);
        CoactvwcResult r = service.process(new CoactvwcRequest(DfhAid.DFHENTER, raw, null));
        // Context byte (offset 33) is '1', so this is a re-entry with nothing keyed.
        assertEquals("No input received", err(assertInstanceOf(SendMapAndReturn.class, r)));
    }

    @Test
    void roundTripUsesEachTasksReturnedCommarea() {
        SendMapAndReturn first = assertInstanceOf(SendMapAndReturn.class,
                service.process(new CoactvwcRequest(DfhAid.DFHENTER, null, null)));
        SendMapAndReturn view = assertInstanceOf(SendMapAndReturn.class, service.process(
                new CoactvwcRequest(DfhAid.DFHENTER, first.commarea(), CactvwaInput.ofAcctsid("12345678901"))));
        assertEquals(Cobol.spaces(78), view.screen().errmsgo());
        assertEquals("777000111", view.screen().acstnumo());
        assertEquals(12345678901L, view.carddemoCommarea().acctId());
        Xctl back = assertInstanceOf(Xctl.class,
                service.process(new CoactvwcRequest(DfhAid.DFHPF3, view.commarea(), null)));
        assertEquals("COMEN01C", back.program());
        assertEquals(12345678901L, back.commarea().acctId());
        assertEquals(4111222233334444L, back.commarea().cardNum());
        assertTrue(back.commarea().isPgmEnter());
    }

    // ---- PF keys -------------------------------------------------------------------------------------

    @Test
    void pf3XctlsToCallingProgram() {
        CardDemoCommarea c = reenterCommarea();
        c.setFromTranid("CC00");
        c.setFromProgram("COCRDLIC");
        Xctl x = assertInstanceOf(Xctl.class,
                service.process(new CoactvwcRequest(DfhAid.DFHPF3, commarea(c, ""), null)));
        assertEquals("COCRDLIC", x.program());
        CardDemoCommarea out = x.commarea();
        assertEquals("CC00", out.toTranid());
        assertEquals("COCRDLIC", out.toProgram());
        assertEquals("CAVW", out.fromTranid());
        assertEquals("COACTVWC", out.fromProgram());
        assertTrue(out.isUsrtypUser(), "PF3 forces CDEMO-USRTYP-USER even for an admin");
        assertTrue(out.isPgmEnter());
        assertEquals("COACTVW", out.lastMapset());
        assertEquals("CACTVWA", out.lastMap());
    }

    @Test
    void pf15FoldsToPf3AndBlankCallerFallsBackToMenu() {
        CardDemoCommarea c = reenterCommarea();
        c.setFromTranid("    ");
        c.setFromProgram(Cobol.lowValues(8));
        Xctl x = assertInstanceOf(Xctl.class,
                service.process(new CoactvwcRequest(DfhAid.DFHPF15, commarea(c, ""), null)));
        assertEquals("COMEN01C", x.program());
        assertEquals("CM00", x.commarea().toTranid());
    }

    @Test
    void pf3WithoutCommareaGoesToMenu() {
        Xctl x = assertInstanceOf(Xctl.class, service.process(new CoactvwcRequest(DfhAid.DFHPF3, null, null)));
        assertEquals("COMEN01C", x.program());
        assertEquals("CM00", x.commarea().toTranid());
    }

    @Test
    void invalidKeyIsTreatedAsEnter() {
        SendMapAndReturn r = assertInstanceOf(SendMapAndReturn.class, service.process(new CoactvwcRequest(
                DfhAid.DFHPF7, commarea(reenterCommarea(), ""), CactvwaInput.ofAcctsid("12345678901"))));
        assertEquals("Lovelace", r.screen().acslnamo().strip());
    }

    // ---- 2200 / 2210 input edits ---------------------------------------------------------------------

    @Test
    void blankInputSaysNoInputReceivedAndMarksFieldWithStar() {
        for (String in : new String[] {null, "", "           ", "*"}) {
            SendMapAndReturn r = enter(in);
            assertEquals("No input received", err(r), "input=" + in);
            assertEquals(Cobol.alnum("*", 11), r.screen().acctsido());
            assertEquals(DfhBmsca.DFHRED, r.screen().acctsidc());
            assertEquals(0L, r.carddemoCommarea().acctId());
            assertNull(r.screen().acstnumo());
        }
    }

    @Test
    void nonNumericAccountRejectedWithExactMessage() {
        SendMapAndReturn r = enter("1234567890A");
        assertEquals("Account Filter must  be a non-zero 11 digit number", err(r));
        assertEquals("1234567890A", r.screen().acctsido());
        assertEquals(DfhBmsca.DFHRED, r.screen().acctsidc());
        assertEquals(0L, r.carddemoCommarea().acctId());
    }

    @Test
    void shortOrZeroAccountRejected() {
        assertEquals("Account Filter must  be a non-zero 11 digit number", err(enter("123")));
        assertEquals("Account Filter must  be a non-zero 11 digit number", err(enter("00000000000")));
    }

    // ---- 9000 reads and 1200 screen population ------------------------------------------------------

    @Test
    void foundAccountPopulatesEveryScreenField() {
        SendMapAndReturn r = enter("12345678901");
        CactvwaOutput s = r.screen();
        assertEquals(Cobol.spaces(78), s.errmsgo());
        assertEquals(Cobol.alnum("Enter or update id of account to display", 45), s.infomsgo());
        assertEquals(DfhBmsca.DFHNEUTR, s.infomsgc());
        assertEquals("12345678901", s.acctsido());
        assertEquals(DfhBmsca.DFHDFCOL, s.acctsidc());
        assertEquals("Y", s.acsttuso());
        assertEquals("-      1,234.56", s.acurbalo());
        assertEquals("+      5,000.00", s.acrdlimo());
        assertEquals("+      1,500.00", s.acshlimo());
        assertEquals("+        100.10", s.acrcycro());
        assertEquals("+234,567,890.12", s.acrcydbo());
        assertEquals("2020-01-15", s.adtopeno());
        assertEquals("2027-01-31", s.aexpdto());
        assertEquals("2025-01-31", s.areisdto());
        assertEquals("PLATINUM  ", s.aaddgrpo());
        assertEquals("777000111", s.acstnumo());
        assertEquals("123-45-6789\0", s.acstssno());
        assertEquals("007", s.acstfcoo());
        assertEquals("1970-12-10", s.acstdobo());
        assertEquals("Ada", s.acsfnamo().strip());
        assertEquals("Q", s.acsmnamo().strip());
        assertEquals("Lovelace", s.acslnamo().strip());
        assertEquals(Cobol.alnum("1 Test Street", 50), s.acsadl1o());
        assertEquals(Cobol.alnum("Unit 2", 50), s.acsadl2o());
        assertEquals(Cobol.alnum("Testville", 50), s.acscityo());
        assertEquals("NY", s.acsstteo());
        assertEquals("10001", s.acszipco());
        assertEquals("USA", s.acsctryo());
        assertEquals("(555)010-0000", s.acsphn1o());
        assertEquals("(555)010-0001", s.acsphn2o());
        assertEquals(Cobol.alnum("GOV-ID-1", 20), s.acsgovto());
        assertEquals("EFT0000001", s.acseftco());
        assertEquals("Y", s.acspflgo());
        CardDemoCommarea c = r.carddemoCommarea();
        assertEquals(12345678901L, c.acctId());
        assertEquals(777000111L, c.custId());
        assertEquals(4111222233334444L, c.cardNum());
        assertEquals("USER0001", c.userId());
        assertTrue(c.isPgmReenter());
        assertEquals("PROGRAM1TRN1", r.thisProgCommarea().toString());
    }

    @Test
    void accountNotInXrefGivesResp13Message() {
        SendMapAndReturn r = enter("99999999999");
        assertEquals("Account:99999999999 not found in Cross ref file.  Resp:000000013  Reas:0000", err(r));
        assertEquals(75, r.screen().errmsgo().stripTrailing().length());
        assertEquals("99999999999", r.screen().acctsido());
        assertEquals(DfhBmsca.DFHRED, r.screen().acctsidc());
        assertNull(r.screen().acsttuso());
    }

    @Test
    void xrefDuplicateKeyFallsIntoFileErrorBranch() {
        xrefs.put(new CardXrefRecord("4111222233335555", 777000111L, 12345678901L));
        SendMapAndReturn r = enter("12345678901");
        assertEquals("File Error: READ     on CXACAIX   returned RESP 000000015 ,RESP2 000000140", err(r));
        assertNull(r.screen().acstnumo());
    }

    @Test
    void xrefIoErrorUsesFileErrorMessage() {
        service = new CoactvwcService(id -> FileReadResult.error(CicsResp.NOTOPEN, 60), accounts, customers, CLOCK);
        SendMapAndReturn r = enter("12345678901");
        assertEquals("File Error: READ     on CXACAIX   returned RESP 000000019 ,RESP2 000000060", err(r));
    }

    @Test
    void accountMissingFromMasterStillReadsCustomer() {
        accounts = new InMemoryAccountRepository(List.of());
        service = new CoactvwcService(xrefs, accounts, customers, CLOCK);
        SendMapAndReturn r = enter("12345678901");
        assertEquals("Account:12345678901 not found in Acct Master file.Resp:000000013  Reas:0000", err(r));
        assertEquals(DfhBmsca.DFHRED, r.screen().acctsidc());
        // 9000 only stops on DID-NOT-FIND-ACCT-IN-ACCTDAT, which is never set, so the customer is still shown.
        assertEquals("777000111", r.screen().acstnumo());
        assertEquals("+           .00", r.screen().acurbalo());
        assertNull(r.screen().acsttuso());
        assertNull(r.screen().adtopeno());
        assertNull(r.screen().aexpdto());
        assertNull(r.screen().areisdto());
        assertNull(r.screen().aaddgrpo());
    }

    @Test
    void accountMasterIoError() {
        service = new CoactvwcService(xrefs, id -> FileReadResult.error(CicsResp.DISABLED, 50), customers, CLOCK);
        SendMapAndReturn r = enter("12345678901");
        assertEquals("File Error: READ     on ACCTDAT   returned RESP 000000084 ,RESP2 000000050", err(r));
    }

    @Test
    void customerNotFoundMessage() {
        customers = new InMemoryCustomerRepository(List.of());
        service = new CoactvwcService(xrefs, accounts, customers, CLOCK);
        SendMapAndReturn r = enter("12345678901");
        assertEquals("CustId:777000111 not found in customer master.Resp: 000000013  REAS:0000000", err(r));
        // Account found, so account fields still show; customer fields do not.
        assertEquals("+      5,000.00", r.screen().acrdlimo());
        assertNull(r.screen().acstnumo());
        // WS-EDIT-ACCT-FLAG is still valid, so the account field stays default colour.
        assertEquals(DfhBmsca.DFHDFCOL, r.screen().acctsidc());
    }

    @Test
    void customerNotFoundKeepsEarlierAccountMessage() {
        accounts = new InMemoryAccountRepository(List.of());
        customers = new InMemoryCustomerRepository(List.of());
        service = new CoactvwcService(xrefs, accounts, customers, CLOCK);
        assertEquals("Account:12345678901 not found in Acct Master file.Resp:000000013  Reas:0000", err(enter("12345678901")));
    }

    @Test
    void customerIoErrorOverwritesMessage() {
        accounts = new InMemoryAccountRepository(List.of());
        service = new CoactvwcService(xrefs, accounts, id -> FileReadResult.error(CicsResp.NOTOPEN, 0), CLOCK);
        assertEquals("File Error: READ     on CUSTDAT   returned RESP 000000019 ,RESP2 000000000",
                err(enter("12345678901")));
    }

    // ---- ABEND-ROUTINE --------------------------------------------------------------------------------

    @Test
    void unexpectedFailureAbends9999() {
        service = new CoactvwcService(id -> { throw new IllegalStateException("AFCA"); }, accounts, customers, CLOCK);
        Abend a = assertInstanceOf(Abend.class, service.process(new CoactvwcRequest(DfhAid.DFHENTER,
                commarea(reenterCommarea(), ""), CactvwaInput.ofAcctsid("12345678901"))));
        assertEquals("9999", a.abcode());
        assertEquals("COACTVWC", a.abendData().abendCulprit());
        // ABEND-MSG starts as SPACES (VALUE SPACES), not LOW-VALUES, so the default text is never moved in.
        assertEquals(Cobol.spaces(72), a.abendData().abendMsg());
        assertEquals("AFCA", a.cause().getMessage());
    }
}
