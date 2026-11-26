package com.carddemo.programs.cousr00c;

import static com.carddemo.programs.cousr00c.Fixtures.firstEntry;
import static com.carddemo.programs.cousr00c.Fixtures.next;
import static com.carddemo.programs.cousr00c.Fixtures.trim;
import static com.carddemo.programs.cousr00c.Fixtures.userId;
import static com.carddemo.programs.cousr00c.Fixtures.users;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.programs.cousr00c.InMemoryUserSecurityFile.Op;
import com.carddemo.programs.cousr00c.copybook.CommonMessages;
import com.carddemo.programs.cousr00c.copybook.Dfhaid;
import com.carddemo.programs.cousr00c.copybook.ScreenTitle;
import com.carddemo.programs.cousr00c.copybook.SecUserData;
import java.util.List;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.ValueSource;

class Cousr00cServiceTest {

    private InMemoryUserSecurityFile file;
    private Cousr00cService service;

    private Cousr00cService with(List<SecUserData> data) {
        file = new InMemoryUserSecurityFile(data);
        service = new Cousr00cService(file, Fixtures.CLOCK);
        return service;
    }

    private static void assertRows(Cousr0aScreen screen, int fromUser, int count) {
        for (int i = 0; i < 10; i++) {
            Cousr0aScreen.Row row = screen.rows().get(i);
            if (i < count) {
                assertEquals(userId(fromUser + i), row.usrid(), "row " + (i + 1));
                assertEquals(SecUserData.of("", "First" + (fromUser + i), "", "", "").secUsrFname(), row.fname());
            } else {
                assertEquals("        ", row.usrid(), "row " + (i + 1) + " cleared");
                assertEquals(" ".repeat(20), row.fname());
            }
        }
    }

    @Nested
    class MainPara {

        @Test
        void eibcalenZeroTransfersToSignon() {
            Cousr00cResponse r = with(users(3)).execute(new Cousr00cRequest(Dfhaid.DFHENTER, null, null));
            assertEquals(Cousr00cResponse.Kind.XCTL, r.kind());
            assertEquals("COSGN00C", r.target());
            assertEquals("CU00", r.commarea().general().getFromTranid());
            assertEquals("COUSR00C", r.commarea().general().getFromProgram());
            assertTrue(r.commarea().general().isPgmEnter());
            assertTrue(r.sends().isEmpty());
            assertTrue(file.calls().isEmpty());
        }

        @Test
        void firstEntryListsFirstPageAndSetsReenter() {
            Cousr00cResponse r = with(users(25)).execute(firstEntry());
            assertEquals(Cousr00cResponse.Kind.RETURN, r.kind());
            assertEquals("CU00", r.target());
            assertTrue(r.commarea().general().isPgmReenter());
            assertEquals(2, r.sends().size(), "PROCESS-PAGE-FORWARD send + MAIN-PARA send");
            Cousr0aScreen s = r.screen();
            assertRows(s, 1, 10);
            assertEquals("00000001", s.pagenum());
            assertEquals("        ", s.usridin());
            assertEquals(" ".repeat(78), s.errmsg());
            assertEquals("USRIDIN", s.cursorField());
            assertTrue(r.lastSend().erase());
            assertEquals(1, r.commarea().cu00().getPageNum());
            assertTrue(r.commarea().cu00().isNextPageYes());
            assertEquals(userId(1), r.commarea().cu00().getUsridFirst());
            assertEquals(userId(10), r.commarea().cu00().getUsridLast());
            assertEquals(List.of(Op.STARTBR, Op.READNEXT, Op.READNEXT, Op.READNEXT, Op.READNEXT, Op.READNEXT,
                    Op.READNEXT, Op.READNEXT, Op.READNEXT, Op.READNEXT, Op.READNEXT, Op.READNEXT, Op.ENDBR),
                    file.calls(), "10 rows + 1 look-ahead READNEXT");
        }

        @Test
        void headerInfoPopulatedFromCurrentDate() {
            Cousr0aScreen s = with(users(1)).execute(firstEntry()).screen();
            assertEquals("CU00", s.trnname());
            assertEquals("COUSR00C", s.pgmname());
            assertEquals(ScreenTitle.CCDA_TITLE01, s.title01());
            assertEquals(ScreenTitle.CCDA_TITLE02, s.title02());
            assertEquals("11/26/26", s.curdate());
            assertEquals("17:22:05", s.curtime());
        }

        @Test
        void pf3ReturnsToAdminMenu() {
            with(users(25));
            Cousr00cResponse first = service.execute(firstEntry());
            Cousr00cResponse r = service.execute(next(first, Dfhaid.DFHPF3));
            assertEquals(Cousr00cResponse.Kind.XCTL, r.kind());
            assertEquals("COADM01C", r.target());
            assertEquals("CU00", r.commarea().general().getFromTranid());
            assertEquals("COUSR00C", r.commarea().general().getFromProgram());
            assertEquals(0, r.commarea().general().getPgmContext());
            assertTrue(r.sends().isEmpty());
        }

        @ParameterizedTest
        @EnumSource(value = Dfhaid.class, names = {"DFHPF1", "DFHPF5", "DFHPF12", "DFHPA1", "DFHCLEAR"})
        void otherKeysShowInvalidKeyMessage(Dfhaid aid) {
            with(users(25));
            Cousr00cResponse first = service.execute(firstEntry());
            Cousr00cResponse r = service.execute(next(first, aid));
            assertEquals(Cousr00cResponse.Kind.RETURN, r.kind());
            assertEquals(1, r.sends().size());
            assertEquals(CommonMessages.CCDA_MSG_INVALID_KEY + " ".repeat(28), r.screen().errmsg());
            assertEquals("Invalid key pressed. Please see below...", trim(r.screen().errmsg()));
            assertEquals("USRIDIN", r.screen().cursorField());
            assertTrue(r.lastSend().erase());
            assertEquals(first.commarea().toFixedWidth(), r.commarea().toFixedWidth());
        }

        @Test
        void clearKeyGetsMapfailSoNoFieldDataIsEchoed() {
            with(users(25));
            Cousr00cResponse first = service.execute(firstEntry());
            Cousr0aScreen s = service.execute(next(first, Dfhaid.DFHCLEAR)).screen();
            assertEquals("\u0000".repeat(8), s.rows().get(0).usrid());
            assertEquals("\u0000".repeat(8), s.pagenum());
        }

        @Test
        void pf5KeepsEchoedListOnScreen() {
            with(users(25));
            Cousr00cResponse first = service.execute(firstEntry());
            Cousr0aScreen s = service.execute(next(first, Dfhaid.DFHPF5)).screen();
            assertRows(s, 1, 10);
            assertEquals("00000001", s.pagenum());
        }
    }

    @Nested
    class ProcessEnterKey {

        @ParameterizedTest
        @ValueSource(strings = {"U", "u"})
        void updateSelectionTransfersToCousr02c(String code) {
            with(users(25));
            Cousr00cResponse first = service.execute(firstEntry());
            Cousr00cResponse r = service.execute(next(first, Dfhaid.DFHENTER,
                    Cousr0aInput.echo(first.screen()).withSelection(3, code)));
            assertEquals(Cousr00cResponse.Kind.XCTL, r.kind());
            assertEquals("COUSR02C", r.target());
            assertEquals("COUSR02C", r.commarea().general().getToProgram());
            assertEquals(code.charAt(0), r.commarea().cu00().getUsrSelFlg());
            assertEquals(userId(3), r.commarea().cu00().getUsrSelected());
            assertEquals("CU00", r.commarea().general().getFromTranid());
            assertEquals("COUSR00C", r.commarea().general().getFromProgram());
            assertTrue(r.commarea().general().isPgmEnter());
            assertTrue(r.sends().isEmpty());
        }

        @ParameterizedTest
        @ValueSource(strings = {"D", "d"})
        void deleteSelectionTransfersToCousr03c(String code) {
            with(users(25));
            Cousr00cResponse first = service.execute(firstEntry());
            Cousr00cResponse r = service.execute(next(first, Dfhaid.DFHENTER,
                    Cousr0aInput.echo(first.screen()).withSelection(10, code)));
            assertEquals("COUSR03C", r.target());
            assertEquals(userId(10), r.commarea().cu00().getUsrSelected());
        }

        @Test
        void firstNonBlankSelectionWins() {
            with(users(25));
            Cousr00cResponse first = service.execute(firstEntry());
            Cousr00cResponse r = service.execute(next(first, Dfhaid.DFHENTER,
                    Cousr0aInput.echo(first.screen()).withSelection(5, "D").withSelection(2, "U")));
            assertEquals("COUSR02C", r.target());
            assertEquals(userId(2), r.commarea().cu00().getUsrSelected());
        }

        @Test
        void invalidSelectionShowsMessageAndRelists() {
            with(users(25));
            Cousr00cResponse first = service.execute(firstEntry());
            Cousr00cResponse page2 = service.execute(next(first, Dfhaid.DFHPF8));
            Cousr00cResponse r = service.execute(next(page2, Dfhaid.DFHENTER,
                    Cousr0aInput.echo(page2.screen()).withSelection(1, "X")));
            assertEquals(Cousr00cResponse.Kind.RETURN, r.kind());
            assertEquals("Invalid selection. Valid values are U and D", trim(r.screen().errmsg()));
            assertEquals('X', r.commarea().cu00().getUsrSelFlg());
            assertEquals(userId(11), r.commarea().cu00().getUsrSelected());
            assertRows(r.screen(), 1, 10);
            assertEquals("00000001", r.screen().pagenum(), "ENTER restarts the list at page 1");
        }

        @Test
        void selectionOnEmptyRowIsIgnored() {
            with(users(3));
            Cousr00cResponse first = service.execute(firstEntry());
            Cousr00cResponse r = service.execute(next(first, Dfhaid.DFHENTER,
                    Cousr0aInput.echo(first.screen()).withSelection(7, "U")));
            assertEquals(Cousr00cResponse.Kind.RETURN, r.kind());
            assertEquals('U', r.commarea().cu00().getUsrSelFlg());
            assertEquals("        ", r.commarea().cu00().getUsrSelected());
            assertEquals("You have reached the bottom of the page...", trim(r.screen().errmsg()));
        }

        @Test
        void noSelectionClearsSelectionFields() {
            with(users(25));
            Cousr00cResponse r = service.execute(firstEntry());
            assertEquals(' ', r.commarea().cu00().getUsrSelFlg());
            assertEquals("        ", r.commarea().cu00().getUsrSelected());
        }

        @Test
        void searchUserIdStartsListAtKeyGteq() {
            with(users(25));
            Cousr00cResponse first = service.execute(firstEntry());
            Cousr00cResponse r = service.execute(next(first, Dfhaid.DFHENTER,
                    Cousr0aInput.echo(first.screen()).withUsridin("USER0012")));
            assertRows(r.screen(), 12, 10);
            assertEquals("00000001", r.screen().pagenum());
            assertEquals("        ", r.screen().usridin(), "search field blanked after paging");
            assertTrue(r.commarea().cu00().isNextPageYes());
        }

        @Test
        void partialSearchKeyIsSpacePaddedBeforeStartbr() {
            with(users(25));
            Cousr00cResponse first = service.execute(firstEntry());
            Cousr00cResponse r = service.execute(next(first, Dfhaid.DFHENTER,
                    Cousr0aInput.echo(first.screen()).withUsridin("USER002")));
            assertEquals(userId(20), r.screen().rows().get(0).usrid(), "'USER002 ' < 'USER0020' in EBCDIC (X'40' < X'F0')");
        }

        @Test
        void searchBeyondLastKeyAbendsOnEndbrAfterNotfnd() {
            with(users(25));
            Cousr00cResponse first = service.execute(firstEntry());
            CicsAbendException e = assertThrows(CicsAbendException.class, () -> service.execute(next(first,
                    Dfhaid.DFHENTER, Cousr0aInput.echo(first.screen()).withUsridin("ZZZZZZZZ"))));
            assertEquals("AEIP", e.abendCode());
            assertEquals(CicsResp.INVREQ, e.response().resp());
            assertEquals(1, e.sendsBeforeAbend().size());
            assertEquals("You are at the top of the page...", trim(e.sendsBeforeAbend().get(0).screen().errmsg()));
        }
    }

    @Nested
    class PageForward {

        @Test
        void exactlyTenUsersReportsBottomOnFirstPage() {
            Cousr00cResponse r = with(users(10)).execute(firstEntry());
            assertEquals(3, r.sends().size(), "ENDFILE send + page send + MAIN-PARA send");
            assertRows(r.screen(), 1, 10);
            assertEquals("You have reached the bottom of the page...", trim(r.screen().errmsg()));
            assertEquals("00000001", r.screen().pagenum());
            assertTrue(r.commarea().cu00().isNextPageNo());
        }

        @Test
        void partialPageClearsRemainingRows() {
            Cousr00cResponse r = with(users(3)).execute(firstEntry());
            assertRows(r.screen(), 1, 3);
            assertEquals("00000001", r.screen().pagenum());
            assertEquals(userId(1), r.commarea().cu00().getUsridFirst());
            assertEquals("        ", r.commarea().cu00().getUsridLast(),
                    "160-byte COADM01C commarea moved into 194-byte area: CU00-INFO space filled, LAST only set by row 10");
        }

        @Test
        void emptyFileShowsTopMessageAndAbendsOnEndbr() {
            with(List.of());
            CicsAbendException e = assertThrows(CicsAbendException.class, () -> service.execute(firstEntry()));
            assertEquals("You are at the top of the page...", trim(e.sendsBeforeAbend().get(0).screen().errmsg()));
        }

        @Test
        void pf8PagesForwardSkippingLastKey() {
            with(users(25));
            Cousr00cResponse p1 = service.execute(firstEntry());
            Cousr00cResponse p2 = service.execute(next(p1, Dfhaid.DFHPF8));
            assertEquals(1, p2.sends().size());
            assertRows(p2.screen(), 11, 10);
            assertEquals("00000002", p2.screen().pagenum());
            assertEquals(userId(11), p2.commarea().cu00().getUsridFirst());
            assertEquals(userId(20), p2.commarea().cu00().getUsridLast());
            assertTrue(p2.commarea().cu00().isNextPageYes());

            Cousr00cResponse p3 = service.execute(next(p2, Dfhaid.DFHPF8));
            assertRows(p3.screen(), 21, 5);
            assertEquals("00000003", p3.screen().pagenum());
            assertEquals("You have reached the bottom of the page...", trim(p3.screen().errmsg()));
            assertTrue(p3.commarea().cu00().isNextPageNo());
            assertEquals(userId(20), p3.commarea().cu00().getUsridLast(), "unchanged when row 10 not filled");
        }

        @Test
        void pf8AtBottomShowsMessageWithoutErase() {
            with(users(25));
            Cousr00cResponse p1 = service.execute(firstEntry());
            Cousr00cResponse p3 = service.execute(next(service.execute(next(p1, Dfhaid.DFHPF8)), Dfhaid.DFHPF8));
            Cousr00cResponse r = service.execute(next(p3, Dfhaid.DFHPF8));
            assertEquals(1, r.sends().size());
            assertFalse(r.lastSend().erase());
            assertEquals("You are already at the bottom of the page...", trim(r.screen().errmsg()));
            assertRows(r.screen(), 21, 5);
            assertEquals(3, r.commarea().cu00().getPageNum());
        }

        @Test
        void pf8WhenNextPageHasExactlyTenRecords() {
            with(users(20));
            Cousr00cResponse p1 = service.execute(firstEntry());
            Cousr00cResponse p2 = service.execute(next(p1, Dfhaid.DFHPF8));
            assertRows(p2.screen(), 11, 10);
            assertEquals("00000002", p2.screen().pagenum());
            assertEquals("You have reached the bottom of the page...", trim(p2.screen().errmsg()));
            assertTrue(p2.commarea().cu00().isNextPageNo());
        }

        @Test
        void pf8WhenLastKeyDeletedAndNothingFollowsKeepsOldRows() {
            with(users(20));
            Cousr00cResponse p1 = service.execute(firstEntry());
            for (int i = 10; i <= 20; i++) {
                file.delete(userId(i));
            }
            CicsAbendException e = assertThrows(CicsAbendException.class,
                    () -> service.execute(next(p1, Dfhaid.DFHPF8)));
            assertEquals("AEIP", e.abendCode());
        }

        @Test
        void startbrErrorShowsUnableToLookupAndSkipsEndbr() {
            with(users(25));
            file.injectFailure(Op.STARTBR, new CicsResponse(CicsResp.NOTOPEN, 0));
            Cousr00cResponse r = service.execute(firstEntry());
            assertEquals(Cousr00cResponse.Kind.RETURN, r.kind());
            assertEquals(2, r.sends().size());
            assertEquals("Unable to lookup User...", trim(r.screen().errmsg()));
            assertEquals(List.of("RESP:000000019REAS:000000000"), r.displays());
            assertEquals(List.of(Op.STARTBR), file.calls());
            assertEquals("\u0000".repeat(8), r.screen().usridin(), "USRIDIN not blanked when ERR-FLG-ON");
            assertEquals("\u0000".repeat(8), r.screen().pagenum());
        }

        @Test
        void readnextIoErrorMidPage() {
            with(users(25));
            Cousr00cResponse p1 = service.execute(firstEntry());
            InMemoryUserSecurityFile real = file;
            UserSecurityFile flaky = new UserSecurityFile() {
                private int reads;

                @Override
                public CicsResponse startBr(String ridfld) {
                    return real.startBr(ridfld);
                }

                @Override
                public BrowseRead readNext() {
                    return ++reads == 4 ? new BrowseRead(new CicsResponse(CicsResp.IOERR, 7), null) : real.readNext();
                }

                @Override
                public BrowseRead readPrev() {
                    return real.readPrev();
                }

                @Override
                public CicsResponse endBr() {
                    return real.endBr();
                }
            };
            Cousr00cResponse r = new Cousr00cService(flaky, Fixtures.CLOCK).execute(next(p1, Dfhaid.DFHPF8));
            assertEquals("Unable to lookup User...", trim(r.screen().errmsg()));
            assertEquals(List.of("RESP:000000017REAS:000000007"), r.displays());
            assertEquals(userId(11), r.screen().rows().get(0).usrid());
            assertEquals(userId(12), r.screen().rows().get(1).usrid());
            assertEquals("        ", r.screen().rows().get(2).usrid());
            assertEquals("00000002", r.screen().pagenum(), "page still advanced because WS-IDX > 1");
            assertTrue(r.commarea().cu00().isNextPageNo());
            assertFalse(real.browseActive(), "ENDBR still issued");
            assertEquals(2, r.sends().size());
        }
    }

    @Nested
    class PageBackward {

        @Test
        void pf7AtFirstPageShowsMessageWithoutErase() {
            with(users(25));
            Cousr00cResponse p1 = service.execute(firstEntry());
            Cousr00cResponse r = service.execute(next(p1, Dfhaid.DFHPF7));
            assertEquals(1, r.sends().size());
            assertFalse(r.lastSend().erase());
            assertEquals("You are already at the top of the page...", trim(r.screen().errmsg()));
            assertRows(r.screen(), 1, 10);
            assertTrue(r.commarea().cu00().isNextPageYes());
        }

        @Test
        void pf7FromPageThreeToPageTwo() {
            with(users(25));
            Cousr00cResponse p1 = service.execute(firstEntry());
            Cousr00cResponse p2 = service.execute(next(p1, Dfhaid.DFHPF8));
            Cousr00cResponse p3 = service.execute(next(p2, Dfhaid.DFHPF8));
            Cousr00cResponse r = service.execute(next(p3, Dfhaid.DFHPF7));
            assertEquals(1, r.sends().size());
            assertRows(r.screen(), 11, 10);
            assertEquals("00000002", r.screen().pagenum());
            assertEquals(" ".repeat(78), r.screen().errmsg());
            assertEquals(userId(11), r.commarea().cu00().getUsridFirst());
            assertEquals(userId(20), r.commarea().cu00().getUsridLast());
            assertTrue(r.commarea().cu00().isNextPageYes());
            assertEquals("        ", r.screen().usridin());
        }

        @Test
        void pf7BackToFirstPageHitsTopOnLookBehind() {
            with(users(25));
            Cousr00cResponse p1 = service.execute(firstEntry());
            Cousr00cResponse p2 = service.execute(next(p1, Dfhaid.DFHPF8));
            Cousr00cResponse r = service.execute(next(p2, Dfhaid.DFHPF7));
            assertEquals(2, r.sends().size(), "ENDFILE send on the look-behind READPREV + page send");
            assertRows(r.screen(), 1, 10);
            assertEquals("00000001", r.screen().pagenum());
            assertEquals("You have reached the top of the page...", trim(r.screen().errmsg()));
            assertEquals(List.of(Op.STARTBR, Op.READPREV, Op.READPREV, Op.READPREV, Op.READPREV, Op.READPREV,
                    Op.READPREV, Op.READPREV, Op.READPREV, Op.READPREV, Op.READPREV, Op.READPREV, Op.READPREV,
                    Op.ENDBR), file.calls().subList(file.calls().size() - 14, file.calls().size()));
        }

        @Test
        void pf7WithFewerThanTenPriorRecordsLeavesPageNumberAndBlanksTopRows() {
            with(users(25));
            Cousr00cResponse p1 = service.execute(firstEntry());
            Cousr00cResponse p2 = service.execute(next(p1, Dfhaid.DFHPF8));
            for (int i = 1; i <= 7; i++) {
                file.delete(userId(i));
            }
            Cousr00cResponse r = service.execute(next(p2, Dfhaid.DFHPF7));
            assertEquals("You have reached the top of the page...", trim(r.screen().errmsg()));
            assertEquals("00000002", r.screen().pagenum(), "page not adjusted when EOF hit inside the loop");
            for (int i = 0; i < 7; i++) {
                assertEquals("        ", r.screen().rows().get(i).usrid());
            }
            assertEquals(userId(8), r.screen().rows().get(7).usrid());
            assertEquals(userId(10), r.screen().rows().get(9).usrid());
            assertEquals(userId(10), r.commarea().cu00().getUsridLast());
        }

        @Test
        void pf7WhenFirstKeyWasDeletedReadprevNotfndIsALookupError() {
            with(users(25));
            Cousr00cResponse p1 = service.execute(firstEntry());
            Cousr00cResponse p2 = service.execute(next(p1, Dfhaid.DFHPF8));
            file.delete(userId(11));
            Cousr00cResponse r = service.execute(next(p2, Dfhaid.DFHPF7));
            assertEquals("Unable to lookup User...", trim(r.screen().errmsg()));
            assertEquals(List.of("RESP:000000013REAS:000000000"), r.displays());
            assertRows(r.screen(), 11, 10);
        }
    }

    @Test
    void endbrIoErrorAbendsWithAeiq() {
        with(users(25));
        file.injectFailure(Op.ENDBR, new CicsResponse(CicsResp.IOERR, 0));
        CicsAbendException e = assertThrows(CicsAbendException.class, () -> service.execute(firstEntry()));
        assertEquals("AEIQ", e.abendCode());
        assertEquals(CicsResp.IOERR, e.response().resp());
    }

    @Test
    void displaysAreEmptyOnHappyPath() {
        assertTrue(with(users(5)).execute(firstEntry()).displays().isEmpty());
    }

    @Test
    void cursorAlwaysOnSearchField() {
        with(users(25));
        Cousr00cResponse p1 = service.execute(firstEntry());
        for (Dfhaid aid : List.of(Dfhaid.DFHPF7, Dfhaid.DFHPF8, Dfhaid.DFHENTER, Dfhaid.DFHPF2)) {
            Cousr00cResponse r = service.execute(next(p1, aid));
            r.sends().forEach(s -> assertEquals("USRIDIN", s.screen().cursorField()));
        }
        assertNull(new Cousr00cService(file, Fixtures.CLOCK)
                .execute(new Cousr00cRequest(Dfhaid.DFHENTER, null, null)).screen());
    }
}
