package com.carddemo.programs.cotrn01c;

import com.carddemo.programs.cotrn01c.copybook.Cobol;
import com.carddemo.programs.cotrn01c.copybook.Cotrn1a.Field;
import com.carddemo.programs.cotrn01c.copybook.Cottl01y;
import com.carddemo.programs.cotrn01c.copybook.Csmsg01y;
import com.carddemo.programs.cotrn01c.copybook.Dfhaid;
import com.carddemo.programs.cotrn01c.copybook.TranRecord;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

import java.math.BigDecimal;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class Cotrn01cServiceTest {

    private static final Clock CLOCK = Clock.fixed(Instant.parse("2026-11-26T09:05:07Z"), ZoneOffset.UTC);
    private static final String ID = "0000000000683580";

    private InMemoryTransactFileGateway gateway;
    private List<String> displayed;
    private Cotrn01cService service;

    @BeforeEach
    void setUp() {
        gateway = new InMemoryTransactFileGateway();
        gateway.write(record(ID, new BigDecimal("504.77")));
        displayed = new ArrayList<>();
        service = new Cotrn01cService(gateway, CLOCK, displayed::add);
    }

    private static TranRecord record(String id, BigDecimal amt) {
        return new TranRecord(id, "01", 1, "POS TERM", "Purchase at Abshire-Lowe", amt, 800000000L,
                "Abshire-Lowe", "North Enoshaven", "72112", "4859452612877065", "2022-06-10 19:27:53.000000",
                "2022-06-11 08:00:00.000000");
    }

    private static Cotrn01cCommarea commarea(boolean reenter, String fromProgram, String selected) {
        Cotrn01cCommarea c = new Cotrn01cCommarea();
        c.fromTranid("CT00");
        c.fromProgram(fromProgram);
        c.userId("USER0001");
        c.userType("U");
        if (reenter) {
            c.pgmContext(Cotrn01cCommarea.PgmContext.REENTER);
        }
        c.ct01TrnSelected(selected);
        return c;
    }

    private CicsOutcome.Return reenter(Dfhaid aid, TrnViewRequest input) {
        return reenter(aid, input, "COTRN00C");
    }

    private CicsOutcome.Return reenter(Dfhaid aid, TrnViewRequest input, String fromProgram) {
        String comm = commarea(true, fromProgram, "").toFixedString();
        return assertInstanceOf(CicsOutcome.Return.class, service.execute(new CicsRequest(comm, aid, input)));
    }

    private static String pad(String s, int n) {
        return Cobol.moveAlnum(s, n);
    }

    // MAIN-PARA: EIBCALEN = 0 -> XCTL COSGN00C via RETURN-TO-PREV-SCREEN
    @Test
    void noCommarea_xctlToSignon() {
        CicsOutcome out = service.execute(new CicsRequest(null, Dfhaid.DFHENTER, null));
        CicsOutcome.Xctl x = assertInstanceOf(CicsOutcome.Xctl.class, out);
        assertEquals("COSGN00C", x.program());
        assertEquals("CT01", x.commarea().fromTranid());
        assertEquals("COTRN01C", x.commarea().fromProgram());
        assertEquals("COSGN00C", x.commarea().toProgram());
        assertTrue(x.commarea().isPgmEnter());
        assertTrue(x.commarea().isNextPageNo());
        assertEquals(218, x.commarea().toFixedString().length());
    }

    // MAIN-PARA: first entry, nothing selected -> blank screen, cursor on TRNIDIN, RETURN TRANSID CT01
    @Test
    void firstEntry_noSelection_sendsEmptyScreen() {
        String comm = commarea(false, "COTRN00C", "").toFixedString();
        CicsOutcome.Return r = assertInstanceOf(CicsOutcome.Return.class,
                service.execute(new CicsRequest(comm, Dfhaid.DFHENTER, null)));
        assertEquals("CT01", r.transId());
        assertEquals(1, r.sentScreens().size());
        TrnViewScreen s = r.screen();
        assertEquals(Field.TRNIDIN, s.cursorField());
        assertTrue(Cobol.isLowValues(s.trnidin()));
        assertTrue(Cobol.isLowValues(s.trnid()));
        assertTrue(Cobol.isLowValues(s.trnamt()));
        assertEquals(Cobol.spaces(78), s.errmsg());
        assertTrue(r.commarea().isPgmReenter());
        assertTrue(gateway.readKeys().isEmpty());
    }

    // POPULATE-HEADER-INFO
    @Test
    void header_titlesProgramTransactionDateTime() {
        TrnViewScreen s = reenter(Dfhaid.DFHENTER, TrnViewRequest.ofTranId(ID)).screen();
        assertEquals(Cottl01y.CCDA_TITLE01, s.title01());
        assertEquals(Cottl01y.CCDA_TITLE02, s.title02());
        assertEquals("CT01", s.trnname());
        assertEquals("COTRN01C", s.pgmname());
        assertEquals("11/26/26", s.curdate());
        assertEquals("09:05:07", s.curtime());
    }

    // MAIN-PARA: first entry with CDEMO-CT01-TRN-SELECTED -> PROCESS-ENTER-KEY then SEND again
    @Test
    void firstEntry_withSelection_displaysTransaction_andSendsTwice() {
        String comm = commarea(false, "COTRN00C", ID).toFixedString();
        CicsOutcome.Return r = assertInstanceOf(CicsOutcome.Return.class,
                service.execute(new CicsRequest(comm, Dfhaid.DFHENTER, null)));
        assertEquals(2, r.sentScreens().size());
        assertEquals(r.sentScreens().get(0), r.sentScreens().get(1));
        assertEquals(ID, r.screen().trnidin());
        assertEquals(ID, r.screen().trnid());
        assertEquals("+00000504.77", r.screen().trnamt());
        assertEquals(List.of(ID), gateway.readKeys());
    }

    @Test
    void firstEntry_selectionLowValues_isNotLookedUp() {
        String comm = commarea(false, "COTRN00C", Cobol.lowValues(16)).toFixedString();
        CicsOutcome.Return r = assertInstanceOf(CicsOutcome.Return.class,
                service.execute(new CicsRequest(comm, Dfhaid.DFHENTER, null)));
        assertEquals(1, r.sentScreens().size());
        assertTrue(gateway.readKeys().isEmpty());
    }

    @Test
    void firstEntry_selectionNotFound_showsNotFound() {
        String comm = commarea(false, "COTRN00C", "9999999999999999").toFixedString();
        CicsOutcome.Return r = assertInstanceOf(CicsOutcome.Return.class,
                service.execute(new CicsRequest(comm, Dfhaid.DFHENTER, null)));
        assertEquals(2, r.sentScreens().size());
        assertEquals(pad("Transaction ID NOT found...", 78), r.screen().errmsg());
    }

    // PROCESS-ENTER-KEY: empty TRNIDIN (spaces, low-values, not transmitted)
    @Test
    void enter_emptyTranId_spaces() {
        TrnViewScreen s = reenter(Dfhaid.DFHENTER, TrnViewRequest.ofTranId("    ")).screen();
        assertEquals(pad("Tran ID can NOT be empty...", 78), s.errmsg());
        assertEquals(Field.TRNIDIN, s.cursorField());
        assertTrue(gateway.readKeys().isEmpty());
    }

    @Test
    void enter_emptyTranId_notTransmitted() {
        CicsOutcome.Return r = reenter(Dfhaid.DFHENTER, TrnViewRequest.empty());
        assertEquals(1, r.sentScreens().size());
        assertEquals(pad("Tran ID can NOT be empty...", 78), r.screen().errmsg());
    }

    @Test
    void enter_mixedSpacesAndLowValues_isNotEmpty_andIsLookedUp() {
        CicsOutcome.Return r = reenter(Dfhaid.DFHENTER, TrnViewRequest.ofTranId(" \u0000"));
        assertEquals(pad("Transaction ID NOT found...", 78), r.screen().errmsg());
        assertEquals(List.of(" \u0000" + Cobol.spaces(14)), gateway.readKeys());
    }

    // PROCESS-ENTER-KEY + READ-TRANSACT-FILE NORMAL: every field mapped with MOVE truncation
    @Test
    void enter_found_mapsAllFieldsWithTruncation() {
        String longDesc = "D".repeat(70) + "tail";
        gateway.write(new TranRecord("LONG", "02", 5004, "OPERATOR", longDesc, new BigDecimal("-123456789.12"),
                123L, "N".repeat(45), "C".repeat(40), "ZIP1234567", "1111222233334444",
                "2022-06-10 19:27:53.000000", "2022-06-12 01:02:03.000000"));
        CicsOutcome.Return r = reenter(Dfhaid.DFHENTER, TrnViewRequest.ofTranId("LONG"));
        TrnViewScreen s = r.screen();
        assertEquals(1, r.sentScreens().size());
        assertEquals(pad("LONG", 16), s.trnid());
        assertEquals("1111222233334444", s.cardnum());
        assertEquals("02", s.ttypcd());
        assertEquals("5004", s.tcatcd());
        assertEquals(pad("OPERATOR", 10), s.trnsrc());
        assertEquals("-23456789.12", s.trnamt());
        assertEquals("D".repeat(60), s.tdesc());
        assertEquals("2022-06-10", s.torigdt());
        assertEquals("2022-06-12", s.tprocdt());
        assertEquals("000000123", s.mid());
        assertEquals("N".repeat(30), s.mname());
        assertEquals("C".repeat(25), s.mcity());
        assertEquals("ZIP1234567", s.mzip());
        assertEquals(Cobol.spaces(78), s.errmsg());
        assertEquals(Field.TRNIDIN, s.cursorField());
    }

    @Test
    void enter_tranIdLongerThan16_isTruncatedToKey() {
        reenter(Dfhaid.DFHENTER, TrnViewRequest.ofTranId(ID + "XYZ"));
        assertEquals(List.of(ID), gateway.readKeys());
    }

    // READ-TRANSACT-FILE NOTFND: detail fields already cleared to spaces
    @Test
    void enter_notFound_clearsDetailsAndShowsMessage() {
        Map<Field, String> in = new EnumMap<>(Field.class);
        in.put(Field.TRNIDIN, "NOPE");
        in.put(Field.TRNID, "OLD-VALUE");
        TrnViewScreen s = reenter(Dfhaid.DFHENTER, new TrnViewRequest(in)).screen();
        assertEquals(pad("Transaction ID NOT found...", 78), s.errmsg());
        assertEquals(Cobol.spaces(16), s.trnid());
        assertEquals(Cobol.spaces(12), s.trnamt());
        assertEquals(pad("NOPE", 16), s.trnidin());
        assertEquals(Field.TRNIDIN, s.cursorField());
    }

    // READ-TRANSACT-FILE OTHER: DISPLAY RESP/REAS + message
    @Test
    void enter_otherResp_unableToLookup_andDisplaysCodes() {
        gateway.failWith(CicsResp.NOTOPEN, 0);
        CicsOutcome.Return r = reenter(Dfhaid.DFHENTER, TrnViewRequest.ofTranId(ID));
        assertEquals(pad("Unable to lookup Transaction...", 78), r.screen().errmsg());
        assertEquals(List.of("RESP:+0000000019REAS:+0000000000"), displayed);
        assertEquals(1, r.sentScreens().size());
    }

    // MAIN-PARA PF3: back to CDEMO-FROM-PROGRAM, or COMEN01C if blank
    @Test
    void pf3_returnsToCallingProgram() {
        String comm = commarea(true, "COTRN00C", "").toFixedString();
        CicsOutcome.Xctl x = assertInstanceOf(CicsOutcome.Xctl.class,
                service.execute(new CicsRequest(comm, Dfhaid.DFHPF3, null)));
        assertEquals("COTRN00C", x.program());
        assertEquals("COTRN01C", x.commarea().fromProgram());
        assertEquals("CT01", x.commarea().fromTranid());
        assertTrue(x.commarea().isPgmEnter());
        assertEquals("USER0001", x.commarea().userId());
    }

    @Test
    void pf3_blankFromProgram_goesToMainMenu() {
        String comm = commarea(true, "", "").toFixedString();
        CicsOutcome.Xctl x = assertInstanceOf(CicsOutcome.Xctl.class,
                service.execute(new CicsRequest(comm, Dfhaid.DFHPF3, null)));
        assertEquals("COMEN01C", x.program());
    }

    @Test
    void pf3_lowValueFromProgram_goesToMainMenu() {
        String comm = commarea(true, Cobol.lowValues(8), "").toFixedString();
        CicsOutcome.Xctl x = assertInstanceOf(CicsOutcome.Xctl.class,
                service.execute(new CicsRequest(comm, Dfhaid.DFHPF3, null)));
        assertEquals("COMEN01C", x.program());
    }

    // MAIN-PARA PF4 -> CLEAR-CURRENT-SCREEN / INITIALIZE-ALL-FIELDS
    @Test
    void pf4_clearsAllFields() {
        Map<Field, String> in = new EnumMap<>(Field.class);
        in.put(Field.TRNIDIN, ID);
        in.put(Field.TRNID, ID);
        in.put(Field.TRNAMT, "+00000504.77");
        CicsOutcome.Return r = reenter(Dfhaid.DFHPF4, new TrnViewRequest(in));
        TrnViewScreen s = r.screen();
        assertEquals(Cobol.spaces(16), s.trnidin());
        assertEquals(Cobol.spaces(16), s.trnid());
        assertEquals(Cobol.spaces(12), s.trnamt());
        assertEquals(Cobol.spaces(10), s.mzip());
        assertEquals(Cobol.spaces(78), s.errmsg());
        assertEquals(Field.TRNIDIN, s.cursorField());
        assertTrue(gateway.readKeys().isEmpty());
    }

    // MAIN-PARA PF5 -> COTRN00C
    @Test
    void pf5_goesToTransactionList() {
        String comm = commarea(true, "COMEN01C", "").toFixedString();
        CicsOutcome.Xctl x = assertInstanceOf(CicsOutcome.Xctl.class,
                service.execute(new CicsRequest(comm, Dfhaid.DFHPF5, null)));
        assertEquals("COTRN00C", x.program());
        assertEquals("COTRN01C", x.commarea().fromProgram());
    }

    // MAIN-PARA WHEN OTHER: invalid key
    @ParameterizedTest
    @EnumSource(value = Dfhaid.class, names = {"DFHPF1", "DFHPF2", "DFHPF7", "DFHPF12", "DFHCLEAR", "DFHPA1"})
    void otherKeys_invalidKeyMessage(Dfhaid aid) {
        CicsOutcome.Return r = reenter(aid, TrnViewRequest.ofTranId(ID));
        assertEquals(Csmsg01y.CCDA_MSG_INVALID_KEY + Cobol.spaces(28), r.screen().errmsg());
        assertEquals(pad("Invalid key pressed. Please see below...", 78), r.screen().errmsg());
        assertNull(r.screen().cursorField());
        assertEquals(pad(ID, 16), r.screen().trnidin());
        assertTrue(gateway.readKeys().isEmpty());
        assertEquals("CT01", r.transId());
    }

    // MOVE DFHCOMMAREA(1:EIBCALEN): short commarea is space padded, unknown bytes pass through
    @Test
    void commarea_shortIsPaddedAndPassedThroughUnchanged() {
        Cotrn01cCommarea full = commarea(true, "COTRN00C", "");
        full.custFname("ALICE");
        full.ct01TrnidFirst("FIRST");
        full.ct01PageNum(3);
        String shortComm = full.toFixedString().substring(0, 180);
        CicsOutcome.Return r = assertInstanceOf(CicsOutcome.Return.class,
                service.execute(new CicsRequest(shortComm, Dfhaid.DFHPF4, null)));
        String back = r.commarea().toFixedString();
        assertEquals(218, back.length());
        assertEquals(shortComm, back.substring(0, 180));
        assertEquals(Cobol.spaces(38), back.substring(180));
    }

    @Test
    void errorFlag_isResetEveryTask() {
        reenter(Dfhaid.DFHENTER, TrnViewRequest.ofTranId("NOPE"));
        TrnViewScreen s = reenter(Dfhaid.DFHENTER, TrnViewRequest.ofTranId(ID)).screen();
        assertEquals(Cobol.spaces(78), s.errmsg());
        assertEquals(ID, s.trnid());
    }

    @Test
    void receiveTrnviewScreen_nullFieldValueIsTreatedAsNotTransmitted() {
        Map<Field, String> in = new HashMap<>();
        in.put(Field.TRNIDIN, null);
        TrnViewRequest req = new TrnViewRequest(in);
        assertFalse(req.fields().containsKey(Field.TRNIDIN));
        assertTrue(new TrnViewRequest(null).fields().isEmpty());
    }
}
