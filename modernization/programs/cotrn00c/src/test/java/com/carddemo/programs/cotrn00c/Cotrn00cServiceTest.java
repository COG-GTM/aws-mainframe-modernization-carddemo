package com.carddemo.programs.cotrn00c;

import com.carddemo.programs.cotrn00c.Cotrn00cResponse.Outcome;
import com.carddemo.programs.cotrn00c.copybook.CarddemoCommarea;
import com.carddemo.programs.cotrn00c.copybook.CobolText;
import com.carddemo.programs.cotrn00c.copybook.DfhAid;
import com.carddemo.programs.cotrn00c.copybook.TranRecord;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import static com.carddemo.programs.cotrn00c.Cotrn00cService.MSG_ALREADY_BOTTOM;
import static com.carddemo.programs.cotrn00c.Cotrn00cService.MSG_ALREADY_TOP;
import static com.carddemo.programs.cotrn00c.Cotrn00cService.MSG_INVALID_SELECTION;
import static com.carddemo.programs.cotrn00c.Cotrn00cService.MSG_REACHED_BOTTOM;
import static com.carddemo.programs.cotrn00c.Cotrn00cService.MSG_REACHED_TOP;
import static com.carddemo.programs.cotrn00c.Cotrn00cService.MSG_TOP_OF_PAGE;
import static com.carddemo.programs.cotrn00c.Cotrn00cService.MSG_TRAN_ID_NOT_NUMERIC;
import static com.carddemo.programs.cotrn00c.Cotrn00cService.MSG_UNABLE_TO_LOOKUP;
import static com.carddemo.programs.cotrn00c.Fixtures.id;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class Cotrn00cServiceTest {

    private InMemoryTransactFile file;
    private Cotrn00cService service;

    private void given(List<TranRecord> records) {
        file = new InMemoryTransactFile(records);
        service = new Cotrn00cService(file, Fixtures.CLOCK);
    }

    /** COMMAREA as sent by the menu (COMEN01C): 160 bytes, CDEMO-PGM-CONTEXT = 0. */
    private static Cotrn00cCommarea fromMenu() {
        CarddemoCommarea g = CarddemoCommarea.blank();
        g.setFromTranid("CM00");
        g.setFromProgram("COMEN01C");
        g.setUserId("USER0001");
        g.setUserType("U");
        g.setPgmContext(0);
        return Cotrn00cCommarea.fromBytes(g.encode());
    }

    private Cotrn00cResponse firstEntry() {
        return service.execute(new Cotrn00cRequest(DfhAid.DFHENTER, fromMenu(), null));
    }

    private Cotrn00cResponse next(Cotrn00cResponse prev, DfhAid aid, ScreenInput input) {
        return service.execute(new Cotrn00cRequest(aid, prev.commarea(), input));
    }

    private Cotrn00cResponse next(Cotrn00cResponse prev, DfhAid aid) {
        return next(prev, aid, ScreenInput.echo(prev.lastScreen()));
    }

    private static String msg(Cotrn00cResponse r) {
        return r.lastScreen().errmsg().stripTrailing();
    }

    private static List<String> ids(ScreenImage s) {
        List<String> out = new ArrayList<>();
        for (ScreenImage.Row r : s.rows()) {
            out.add(r.trnid());
        }
        return out;
    }

    private static List<String> idRange(int from, int to) {
        List<String> out = new ArrayList<>();
        for (int i = from; i <= to; i++) {
            out.add(id(i));
        }
        return out;
    }

    @Nested
    class MainPara {

        @Test
        void noCommareaXctlsToSignon() {
            given(Fixtures.trans(3));
            Cotrn00cResponse r = service.execute(new Cotrn00cRequest(DfhAid.DFHENTER, null, null));
            assertEquals(Outcome.XCTL, r.outcome());
            assertEquals("COSGN00C", r.xctlProgram());
            assertEquals("CT00", r.commarea().general().fromTranid());
            assertEquals("COTRN00C", r.commarea().general().fromProgram());
            assertTrue(r.commarea().general().isPgmEnter());
            assertTrue(r.screens().isEmpty());
            assertTrue(file.log().isEmpty());
        }

        @Test
        void firstEntryListsFirstPageAndReturnsWithCt00() {
            given(Fixtures.trans(25));
            Cotrn00cResponse r = firstEntry();
            assertEquals(Outcome.RETURN, r.outcome());
            assertEquals("CT00", r.nextTransid());
            assertTrue(r.commarea().general().isPgmReenter());
            assertEquals("USER0001", r.commarea().general().userId());
            // PROCESS-PAGE-FORWARD sends, then MAIN-PARA sends again.
            assertEquals(2, r.screens().size());
            ScreenImage s = r.lastScreen();
            assertEquals(idRange(1, 10), ids(s));
            assertEquals("00000001", s.pagenum());
            assertEquals(" ".repeat(16), s.trnidin());
            assertEquals("", msg(r));
            assertTrue(s.erase());
            assertEquals("TRNIDIN", s.cursorField());
            Ct00Info ct00 = r.commarea().ct00();
            assertEquals(1, ct00.pageNum());
            assertTrue(ct00.isNextPageYes());
            assertEquals(id(1), ct00.trnidFirst());
            assertEquals(id(10), ct00.trnidLast());
            assertEquals(" ", ct00.trnSelFlg());
            assertEquals(" ".repeat(16), ct00.trnSelected());
            assertEquals(List.of("STARTBR NORMAL", "READNEXT NORMAL " + id(1)), file.log().subList(0, 2));
            assertEquals("ENDBR NORMAL", file.log().get(file.log().size() - 1));
            assertFalse(file.isBrowsing());
        }

        @Test
        void headerInfoFromCurrentDate() {
            given(Fixtures.trans(1));
            ScreenImage s = firstEntry().lastScreen();
            assertEquals("CT00", s.trnname());
            assertEquals("COTRN00C", s.pgmname());
            assertEquals("03/07/26", s.curdate());
            assertEquals("09:05:04", s.curtime());
            assertEquals("      AWS Mainframe Modernization       ", s.title01());
            assertEquals("              CardDemo                  ", s.title02());
        }

        @Test
        void pf3XctlsToMenu() {
            given(Fixtures.trans(3));
            Cotrn00cResponse r = next(firstEntry(), DfhAid.DFHPF3);
            assertEquals(Outcome.XCTL, r.outcome());
            assertEquals("COMEN01C", r.xctlProgram());
            assertEquals("COTRN00C", r.commarea().general().fromProgram());
            assertEquals(0, r.commarea().general().pgmContext());
        }

        @Test
        void otherKeyShowsInvalidKeyMessage() {
            given(Fixtures.trans(3));
            Cotrn00cResponse first = firstEntry();
            Cotrn00cResponse r = next(first, DfhAid.DFHPF5);
            assertEquals(Outcome.RETURN, r.outcome());
            assertEquals(1, r.screens().size());
            assertEquals("Invalid key pressed. Please see below...", msg(r));
            assertEquals(78, r.lastScreen().errmsg().length());
            assertEquals(ids(first.lastScreen()), ids(r.lastScreen()));
            assertEquals("TRNIDIN", r.lastScreen().cursorField());
            assertTrue(r.lastScreen().erase());
        }

        @Test
        void shortMenuCommareaIsSpacePadded() {
            Cotrn00cCommarea c = fromMenu();
            assertEquals(Cotrn00cCommarea.LENGTH, c.toBytes().length());
            assertEquals(218, Cotrn00cCommarea.LENGTH);
            assertEquals(" ".repeat(16), c.ct00().trnidFirst());
            assertEquals(0, c.ct00().pageNum());
            assertEquals(" ", c.ct00().nextPageFlg());
            assertFalse(c.ct00().isNextPageYes());
        }
    }

    @Nested
    class PopulateTranData {

        @Test
        void rowFieldsAreEditedLikeTheMap() {
            TranRecord big = new TranRecord(id(1), "01", 1, "POS", "A".repeat(30) + "tail",
                    new BigDecimal("123456789.12"), 1, "", "", "", "", "2019-12-31 23:59:59.999999", "");
            TranRecord neg = Fixtures.tran(2, "-919.00");
            given(List.of(big, neg));
            ScreenImage s = firstEntry().lastScreen();
            assertEquals("12/31/19", s.row(1).tdate());
            assertEquals("A".repeat(26), s.row(1).tdesc());
            assertEquals("+23456789.12", s.row(1).tamt());
            assertEquals("-00000919.00", s.row(2).tamt());
        }

        @Test
        void amountEditPicture() {
            assertEquals("+00000000.00", Cotrn00cService.formatTranAmt(BigDecimal.ZERO));
            assertEquals("-00000000.01", Cotrn00cService.formatTranAmt(new BigDecimal("-0.01")));
            assertEquals("+99999999.99", Cotrn00cService.formatTranAmt(new BigDecimal("999999999.99")));
        }

        @Test
        void fixtureRowsFromDailytran() {
            given(Fixtures.dailyTran());
            ScreenImage s = firstEntry().lastScreen();
            assertEquals(new ScreenImage.Row("\u0000", "0000000000683580", "06/10/22",
                    "Purchase at Abshire-Lowe  ", "+00000504.77"), s.row(1));
            assertEquals("0000000001774260", s.row(2).trnid());
            assertEquals("Return item at Nitzsche, N", s.row(2).tdesc());
            assertEquals("-00000919.00", s.row(2).tamt());
        }
    }

    @Nested
    class ProcessEnterKey {

        @Test
        void selectionSXctlsToCotrn01cWithSelectedId() {
            given(Fixtures.trans(12));
            Cotrn00cResponse first = firstEntry();
            Cotrn00cResponse r = next(first, DfhAid.DFHENTER, ScreenInput.echo(first.lastScreen()).withSelection(3, "S"));
            assertEquals(Outcome.XCTL, r.outcome());
            assertEquals("COTRN01C", r.xctlProgram());
            assertEquals("S", r.commarea().ct00().trnSelFlg());
            assertEquals(id(3), r.commarea().ct00().trnSelected());
            assertEquals("CT00", r.commarea().general().fromTranid());
            assertEquals("COTRN00C", r.commarea().general().fromProgram());
            assertTrue(r.commarea().general().isPgmEnter());
            assertTrue(r.screens().isEmpty());
        }

        @Test
        void lowerCaseSAlsoSelectsAndFirstMarkedRowWins() {
            given(Fixtures.trans(12));
            Cotrn00cResponse first = firstEntry();
            ScreenInput in = ScreenInput.echo(first.lastScreen()).withSelection(5, "s").withSelection(7, "S");
            Cotrn00cResponse r = next(first, DfhAid.DFHENTER, in);
            assertEquals("COTRN01C", r.xctlProgram());
            assertEquals(id(5), r.commarea().ct00().trnSelected());
        }

        @Test
        void otherSelectionCodeShowsMessageOnRefreshedList() {
            given(Fixtures.trans(12));
            Cotrn00cResponse first = firstEntry();
            Cotrn00cResponse r = next(first, DfhAid.DFHENTER, ScreenInput.echo(first.lastScreen()).withSelection(2, "X"));
            assertEquals(Outcome.RETURN, r.outcome());
            assertEquals(MSG_INVALID_SELECTION, msg(r));
            assertEquals("X", r.commarea().ct00().trnSelFlg());
            assertEquals(idRange(1, 10), ids(r.lastScreen()));
            assertEquals("00000001", r.lastScreen().pagenum());
        }

        @Test
        void selectionOnBlankRowIsIgnored() {
            given(Fixtures.trans(3));
            Cotrn00cResponse first = firstEntry();
            Cotrn00cResponse r = next(first, DfhAid.DFHENTER, ScreenInput.echo(first.lastScreen()).withSelection(6, "S"));
            assertEquals(Outcome.RETURN, r.outcome());
            assertEquals("S", r.commarea().ct00().trnSelFlg());
            assertEquals(" ".repeat(16), r.commarea().ct00().trnSelected());
        }

        @Test
        void nonNumericTranIdIsRejected() {
            given(Fixtures.trans(12));
            Cotrn00cResponse first = firstEntry();
            Cotrn00cResponse r = next(first, DfhAid.DFHENTER, ScreenInput.echo(first.lastScreen()).withTrnidin("ABC"));
            assertEquals(Outcome.RETURN, r.outcome());
            assertEquals(1, r.screens().size());
            assertEquals(MSG_TRAN_ID_NOT_NUMERIC, msg(r));
            assertEquals("ABC" + " ".repeat(13), r.lastScreen().trnidin());
            assertEquals("TRNIDIN", r.lastScreen().cursorField());
            // STARTBR was still issued (PROCESS-PAGE-FORWARD runs) but the page was not re-read or ENDBR'd.
            assertEquals(List.of("STARTBR NORMAL"), file.log().subList(file.log().size() - 1, file.log().size()));
        }

        @Test
        void shortNumericTranIdIsNotNumericBecauseOfSpacePadding() {
            given(Fixtures.trans(12));
            Cotrn00cResponse first = firstEntry();
            Cotrn00cResponse r = next(first, DfhAid.DFHENTER, ScreenInput.echo(first.lastScreen()).withTrnidin("5"));
            assertEquals(MSG_TRAN_ID_NOT_NUMERIC, msg(r));
        }

        @Test
        void sixteenDigitTranIdStartsListAtThatKeyOrNext() {
            given(Fixtures.trans(30));
            file.remove(id(5));
            Cotrn00cResponse first = firstEntry();
            Cotrn00cResponse r = next(first, DfhAid.DFHENTER, ScreenInput.echo(first.lastScreen()).withTrnidin(id(5)));
            assertEquals(idRange(6, 15), ids(r.lastScreen()));
            assertEquals("00000001", r.lastScreen().pagenum());
            assertEquals(" ".repeat(16), r.lastScreen().trnidin());
            assertEquals("", msg(r));
        }

        @Test
        void tranIdBeyondLastRecordShowsTopOfPageThenAbendsOnEndbr() {
            given(Fixtures.trans(3));
            Cotrn00cResponse first = firstEntry();
            Cotrn00cResponse r = next(first, DfhAid.DFHENTER, ScreenInput.echo(first.lastScreen()).withTrnidin(id(99)));
            assertEquals(Outcome.ABEND, r.outcome());
            assertEquals("AEIP", r.abendCode());
            assertEquals(MSG_TOP_OF_PAGE, msg(r));
        }
    }

    @Nested
    class Paging {

        @Test
        void pf8ShowsNextPageAndPf7GoesBack() {
            given(Fixtures.trans(35));
            Cotrn00cResponse p1 = firstEntry();
            Cotrn00cResponse p2 = next(p1, DfhAid.DFHPF8);
            assertEquals(idRange(11, 20), ids(p2.lastScreen()));
            assertEquals("00000002", p2.lastScreen().pagenum());
            assertTrue(p2.commarea().ct00().isNextPageYes());
            Cotrn00cResponse p3 = next(p2, DfhAid.DFHPF8);
            assertEquals(idRange(21, 30), ids(p3.lastScreen()));
            Cotrn00cResponse back = next(p3, DfhAid.DFHPF7);
            assertEquals(idRange(11, 20), ids(back.lastScreen()));
            assertEquals("00000002", back.lastScreen().pagenum());
            assertEquals(id(11), back.commarea().ct00().trnidFirst());
            assertEquals(id(20), back.commarea().ct00().trnidLast());
            assertEquals(1, back.screens().size());
            assertEquals("", msg(back));
        }

        @Test
        void pf7BackToFirstPageReachesTopOfFile() {
            given(Fixtures.trans(25));
            Cotrn00cResponse p2 = next(firstEntry(), DfhAid.DFHPF8);
            Cotrn00cResponse r = next(p2, DfhAid.DFHPF7);
            assertEquals(idRange(1, 10), ids(r.lastScreen()));
            assertEquals(MSG_REACHED_TOP, msg(r));
            assertEquals("00000001", r.lastScreen().pagenum());
            assertEquals(2, r.screens().size());
        }

        @Test
        void pf7OnFirstPageIsAlreadyAtTopWithoutErase() {
            given(Fixtures.trans(25));
            Cotrn00cResponse r = next(firstEntry(), DfhAid.DFHPF7);
            assertEquals(MSG_ALREADY_TOP, msg(r));
            assertFalse(r.lastScreen().erase());
            assertEquals(1, r.screens().size());
            assertTrue(r.commarea().ct00().isNextPageYes());
        }

        @Test
        void pf8WithoutNextPageIsAlreadyAtBottomWithoutErase() {
            given(Fixtures.trans(4));
            Cotrn00cResponse first = firstEntry();
            assertTrue(first.commarea().ct00().isNextPageNo());
            Cotrn00cResponse r = next(first, DfhAid.DFHPF8);
            assertEquals(MSG_ALREADY_BOTTOM, msg(r));
            assertFalse(r.lastScreen().erase());
            assertEquals(1, r.screens().size());
        }

        @Test
        void exactlyTenRecordsShowsBottomMessageAndNoNextPage() {
            given(Fixtures.trans(10));
            Cotrn00cResponse r = firstEntry();
            assertEquals(idRange(1, 10), ids(r.lastScreen()));
            assertEquals(MSG_REACHED_BOTTOM, msg(r));
            assertTrue(r.commarea().ct00().isNextPageNo());
            assertEquals("00000001", r.lastScreen().pagenum());
            // ENDFILE send, PAGE-FORWARD send, MAIN-PARA send.
            assertEquals(3, r.screens().size());
        }

        @Test
        void partialPageBlanksRemainingRowsAndKeepsOldLastId() {
            given(Fixtures.trans(13));
            Cotrn00cResponse p1 = firstEntry();
            Cotrn00cResponse p2 = next(p1, DfhAid.DFHPF8);
            ScreenImage s = p2.lastScreen();
            assertEquals(idRange(11, 13), ids(s).subList(0, 3));
            for (int i = 4; i <= 10; i++) {
                assertEquals(" ".repeat(16), s.row(i).trnid());
                assertEquals(" ".repeat(12), s.row(i).tamt());
            }
            assertEquals(MSG_REACHED_BOTTOM, msg(p2));
            assertEquals("00000002", s.pagenum());
            assertTrue(p2.commarea().ct00().isNextPageNo());
            // TRNID-LAST is only set from row 10, so it still points at page 1's last row.
            assertEquals(id(10), p2.commarea().ct00().trnidLast());
            assertEquals(id(11), p2.commarea().ct00().trnidFirst());
        }

        @Test
        void pf8WithBlankLastIdUsesHighValues() {
            given(Fixtures.trans(5));
            Cotrn00cCommarea c = fromMenu();
            c.general().setPgmContext(CarddemoCommarea.PgmContext.REENTER);
            c.ct00().setNextPageYes();
            c.ct00().setPageNum(3);
            Cotrn00cResponse r = service.execute(new Cotrn00cRequest(DfhAid.DFHPF8, c, ScreenInput.empty()));
            assertEquals(Outcome.RETURN, r.outcome());
            assertEquals(MSG_REACHED_BOTTOM, msg(r));
            assertEquals("00000003", r.lastScreen().pagenum());
            assertTrue(r.commarea().ct00().isNextPageNo());
        }

        @Test
        void backwardHittingStartOfFileMidPageKeepsPageNumber() {
            given(Fixtures.trans(20));
            Cotrn00cCommarea c = fromMenu();
            c.general().setPgmContext(CarddemoCommarea.PgmContext.REENTER);
            c.ct00().setPageNum(2);
            c.ct00().setTrnidFirst(id(4));
            c.ct00().setTrnidLast(id(13));
            Cotrn00cResponse r = service.execute(new Cotrn00cRequest(DfhAid.DFHPF7, c, ScreenInput.empty()));
            ScreenImage s = r.lastScreen();
            assertEquals(MSG_REACHED_TOP, msg(r));
            assertEquals(id(3), s.row(10).trnid());
            assertEquals(id(1), s.row(8).trnid());
            assertEquals(" ".repeat(16), s.row(7).trnid());
            assertEquals("00000002", s.pagenum());
            assertEquals(id(4), r.commarea().ct00().trnidFirst());
            assertEquals(id(3), r.commarea().ct00().trnidLast());
        }

        @Test
        void fixturePagesThroughAllDailytranRecords() {
            List<TranRecord> all = Fixtures.dailyTran();
            given(all);
            List<String> seen = new ArrayList<>();
            Cotrn00cResponse r = firstEntry();
            seen.addAll(ids(r.lastScreen()));
            int pages = 1;
            while (r.commarea().ct00().isNextPageYes()) {
                r = next(r, DfhAid.DFHPF8);
                seen.addAll(ids(r.lastScreen()));
                pages++;
            }
            assertEquals(30, pages);
            assertEquals(all.stream().map(TranRecord::tranId).sorted().toList(), seen);
            assertEquals("00000030", r.lastScreen().pagenum());
            assertEquals(MSG_REACHED_BOTTOM, msg(r));
            Cotrn00cResponse again = next(r, DfhAid.DFHPF8);
            assertEquals(MSG_ALREADY_BOTTOM, msg(again));
        }
    }

    @Nested
    class FileErrors {

        @Test
        void startbrFailureShowsUnableToLookupAndDisplaysResp() {
            given(Fixtures.trans(5));
            file.injectFailure(InMemoryTransactFile.Op.STARTBR, new CicsResponse(CicsResp.NOTOPEN, 60));
            Cotrn00cResponse r = firstEntry();
            assertEquals(Outcome.RETURN, r.outcome());
            assertEquals(MSG_UNABLE_TO_LOOKUP, msg(r));
            assertEquals(List.of("RESP:000000019REAS:000000060"), r.displayLines());
            assertEquals(2, r.screens().size());
            assertEquals(List.of("STARTBR NOTOPEN"), file.log());
        }

        @Test
        void readnextFailureMidPageStopsAndEndsBrowse() {
            given(Fixtures.trans(15));
            file.injectFailureOnCall(InMemoryTransactFile.Op.READNEXT, 4, CicsResponse.of(CicsResp.IOERR));
            Cotrn00cResponse r = firstEntry();
            assertEquals(Outcome.RETURN, r.outcome());
            assertEquals(MSG_UNABLE_TO_LOOKUP, msg(r));
            assertEquals(List.of("RESP:000000017REAS:000000000"), r.displayLines());
            assertEquals(idRange(1, 3), ids(r.lastScreen()).subList(0, 3));
            assertEquals(" ".repeat(16), r.lastScreen().row(4).trnid());
            assertEquals("ENDBR NORMAL", file.log().get(file.log().size() - 1));
            assertTrue(r.commarea().ct00().isNextPageNo());
            assertEquals(1, r.commarea().ct00().pageNum());
        }

        @Test
        void readprevNotfndWhenFirstIdWasDeleted() {
            given(Fixtures.trans(30));
            Cotrn00cResponse p2 = next(firstEntry(), DfhAid.DFHPF8);
            file.remove(id(11));
            Cotrn00cResponse r = next(p2, DfhAid.DFHPF7);
            assertEquals(Outcome.RETURN, r.outcome());
            assertEquals(MSG_UNABLE_TO_LOOKUP, msg(r));
            assertEquals(List.of("RESP:000000013REAS:000000000"), r.displayLines());
            assertEquals("ENDBR NORMAL", file.log().get(file.log().size() - 1));
        }

        @Test
        void emptyFileOnFirstEntryShowsTopOfPageThenAbends() {
            given(List.of());
            Cotrn00cResponse r = firstEntry();
            assertEquals(Outcome.ABEND, r.outcome());
            assertEquals("AEIP", r.abendCode());
            assertNull(r.nextTransid());
            assertEquals(MSG_TOP_OF_PAGE, msg(r));
            assertEquals(1, r.screens().size());
            assertEquals(CobolText.lowValues(16), r.lastScreen().row(1).trnid());
        }
    }
}
