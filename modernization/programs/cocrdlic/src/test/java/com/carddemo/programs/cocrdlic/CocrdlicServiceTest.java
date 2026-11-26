package com.carddemo.programs.cocrdlic;

import static com.carddemo.programs.cocrdlic.CocrdlicService.*;
import static org.junit.jupiter.api.Assertions.*;

import com.carddemo.programs.cocrdlic.CardListResponse.SendMapAndReturn;
import com.carddemo.programs.cocrdlic.CardListResponse.Xctl;
import com.carddemo.programs.cocrdlic.copybook.BmsAttributes;
import com.carddemo.programs.cocrdlic.copybook.CardDemoCommarea;
import com.carddemo.programs.cocrdlic.copybook.CardListMap;
import com.carddemo.programs.cocrdlic.copybook.CardRecord;
import com.carddemo.programs.cocrdlic.copybook.EibAid;
import com.carddemo.programs.cocrdlic.copybook.Pic;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;
import java.util.function.IntToLongFunction;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

class CocrdlicServiceTest {

    static final Clock CLOCK = Clock.fixed(Instant.parse("2026-11-26T10:15:30Z"), ZoneOffset.UTC);

    InMemoryCardFileGateway gateway;
    CocrdlicService service;

    static String num(int i) {
        return String.format("%016d", 4000000000000000L + i);
    }

    static List<CardRecord> cards(int n, IntToLongFunction acct) {
        List<CardRecord> list = new ArrayList<>();
        for (int i = 1; i <= n; i++) {
            list.add(CardRecord.of(num(i), acct.applyAsLong(i), i % 2 == 0 ? 'N' : 'Y'));
        }
        return list;
    }

    void givenCards(int n) {
        givenCards(cards(n, i -> 10_000_000_000L + i));
    }

    void givenCards(List<CardRecord> list) {
        gateway = new InMemoryCardFileGateway(list);
        service = new CocrdlicService(gateway, CLOCK);
    }

    static SendMapAndReturn screen(CardListResponse r) {
        return assertInstanceOf(SendMapAndReturn.class, r);
    }

    SendMapAndReturn first() {
        return screen(service.execute(CardListRequest.initial()));
    }

    CardListResponse send(SendMapAndReturn prev, EibAid aid, CardListScreenInput in) {
        return service.execute(CardListRequest.reenter(prev.commareaImage(), aid, in));
    }

    SendMapAndReturn next(SendMapAndReturn prev, EibAid aid) {
        return screen(send(prev, aid, CardListScreenInput.empty()));
    }

    SendMapAndReturn next(SendMapAndReturn prev, EibAid aid, CardListScreenInput in) {
        return screen(send(prev, aid, in));
    }

    static List<String> rowCards(CardListMap map) {
        List<String> out = new ArrayList<>();
        for (int r = 1; r <= CardListMap.ROWS; r++) {
            if (!map.crdnum(r).text().isEmpty()) {
                out.add(map.crdnum(r).text());
            }
        }
        return out;
    }

    static List<String> nums(int from, int to) {
        List<String> out = new ArrayList<>();
        for (int i = from; i <= to; i++) {
            out.add(num(i));
        }
        return out;
    }

    static CardDemoCommarea commareaFrom(String program, boolean reenter) {
        CardDemoCommarea c = new CardDemoCommarea();
        c.setFromTranid("XXXX");
        c.setFromProgram(program);
        c.setUserId("USER0001");
        c.setUserType(CardDemoCommarea.UserType.ADMIN);
        c.setPgmContext(reenter ? 1 : 0);
        c.setAcctId(12345678901L);
        c.setCardNum(4000000000000003L);
        return c;
    }

    @Nested
    class MainEntry {
        @Test
        void firstEntryWithNoCommareaListsFirstSevenCardsFromStartOfFile() {
            givenCards(20);
            SendMapAndReturn r = first();

            assertEquals(LIT_THISTRANID, r.transid());
            assertEquals(nums(1, 7), rowCards(r.screen()));
            assertEquals("10000000001", r.screen().acctno(1).text());
            assertEquals("Y", r.screen().crdsts(1).text());
            assertEquals("N", r.screen().crdsts(2).text());
            assertEquals(MSG_INFORM_REC_ACTIONS, r.screen().infomsg.text());
            assertEquals(BmsAttributes.DFHNEUTR, r.screen().infomsg.color());
            assertEquals("", r.screen().errmsg.text());
            assertEquals("ACCTSID", r.screen().cursorField());
            assertEquals(List.of("STARTBR CARDDAT GTEQ '                '"), gateway.commandLog().subList(0, 1));
        }

        @Test
        void screenInitPopulatesHeaderTitlesDateTimeAndPage() {
            givenCards(3);
            CardListMap m = first().screen();
            assertEquals("CCLI", m.trnname.text());
            assertEquals("COCRDLIC", m.pgmname.text());
            assertEquals("AWS Mainframe Modernization", m.title01.text().strip());
            assertEquals("CardDemo", m.title02.text().strip());
            assertEquals("11/26/26", m.curdate.text());
            assertEquals("10:15:30", m.curtime.text());
            assertEquals("1  ", m.pageno.data());
        }

        @Test
        void returnedCommareaIs2000BytesWithCarddemoAndProgramState() {
            givenCards(20);
            SendMapAndReturn r = first();
            assertEquals(2000, r.commareaImage().length());
            CardDemoCommarea c = r.commarea();
            assertEquals("CCLI", c.getFromTranid());
            assertEquals("COCRDLIC", c.getFromProgram());
            assertTrue(c.isUserTypeUser());
            assertTrue(c.isPgmEnter());
            assertEquals("CCRDLIA", c.getLastMap());
            assertEquals("COCRDLI", c.getLastMapset());
            ThisProgCommarea s = r.programState();
            assertEquals(1, s.screenNum());
            assertEquals(num(1), s.firstCardNum());
            assertEquals(num(8), s.lastCardNum(), "last key is the look-ahead record");
            assertTrue(s.isNextPageExists());
            assertTrue(s.isLastPageNotShown());
        }

        @Test
        void entryFromMenuSkipsFilterFieldSetupAndResetsPaging() {
            givenCards(20);
            CardDemoCommarea menu = commareaFrom(LIT_MENUPGM, false);
            SendMapAndReturn r = screen(service.execute(CardListRequest.fromProgram(menu, EibAid.ENTER)));
            assertTrue(Pic.isLowValues(r.screen().acctsid.data()));
            assertEquals(Pic.LOW_VALUE, r.screen().acctsid.attribute());
            assertEquals(nums(1, 7), rowCards(r.screen()));
            assertEquals("USER0001", r.commarea().getUserId());
            assertTrue(r.commarea().isUserTypeAdmin(), "user type is only forced to U on specific paths");
        }

        @Test
        void entryFromOtherProgramWithAccountContextShowsItButDoesNotFilter() {
            givenCards(20);
            CardDemoCommarea other = commareaFrom(LIT_CARDDTLPGM, false);
            SendMapAndReturn r = screen(service.execute(CardListRequest.fromProgram(other, EibAid.ENTER)));
            assertEquals("12345678901", r.screen().acctsid.text());
            assertEquals(BmsAttributes.DFHBMFSE, r.screen().acctsid.attribute());
            assertEquals("4000000000000003", r.screen().cardsid.text());
            assertEquals(nums(1, 7), rowCards(r.screen()));
        }

        @Test
        void reenterFromAnotherProgramReinitialisesCommarea() {
            givenCards(20);
            CardDemoCommarea back = commareaFrom(LIT_CARDDTLPGM, true);
            SendMapAndReturn r = screen(service.execute(CardListRequest.fromProgram(back, EibAid.ENTER)));
            assertEquals(nums(1, 7), rowCards(r.screen()));
            assertEquals(0, r.commarea().getAcctId());
            assertEquals("        ", r.commarea().getUserId());
            assertTrue(r.commarea().isUserTypeUser());
            assertTrue(Pic.isLowValues(r.screen().acctsid.data()));
        }

        @Test
        void invalidAidKeyIsTreatedAsEnterAndRedisplaysCurrentPage() {
            givenCards(20);
            SendMapAndReturn p2 = next(first(), EibAid.PF8);
            SendMapAndReturn again = next(p2, EibAid.PF5);
            assertEquals(rowCards(p2.screen()), rowCards(again.screen()));
            assertEquals("2  ", again.screen().pageno.data());
        }

        @Test
        void pf3FromThisProgramTransfersToMenu() {
            givenCards(20);
            Xctl x = assertInstanceOf(Xctl.class, send(first(), EibAid.PF3, CardListScreenInput.empty()));
            assertEquals(LIT_MENUPGM, x.program());
            assertEquals("COMEN01C", x.commarea().getToProgram());
            assertEquals("COCRDLIC", x.commarea().getFromProgram());
            assertEquals("CCLI", x.commarea().getFromTranid());
            assertTrue(x.commarea().isPgmEnter());
            assertTrue(x.commarea().isUserTypeUser());
            assertEquals("COCRDLI", x.commarea().getLastMapset());
            assertEquals("CCRDLIA", x.commarea().getLastMap());
        }

        @Test
        void pf15IsFoldedOntoPf3() {
            givenCards(3);
            Xctl x = assertInstanceOf(Xctl.class, send(first(), EibAid.PF15, CardListScreenInput.empty()));
            assertEquals(LIT_MENUPGM, x.program());
        }

        @Test
        void pf3WhenCalledFromAnotherProgramRestartsTheList() {
            givenCards(20);
            CardDemoCommarea menu = commareaFrom(LIT_MENUPGM, false);
            SendMapAndReturn r = screen(service.execute(CardListRequest.fromProgram(menu, EibAid.PF3)));
            assertEquals(nums(1, 7), rowCards(r.screen()));
            assertEquals(0, r.commarea().getAcctId());
        }
    }

    @Nested
    class Selection {
        @Test
        void sOnRowTransfersToCardDetailWithRowKeys() {
            givenCards(20);
            SendMapAndReturn p1 = first();
            Xctl x = assertInstanceOf(Xctl.class,
                    send(p1, EibAid.ENTER, CardListScreenInput.empty().withSelection(3, "S")));
            assertEquals(LIT_CARDDTLPGM, x.program());
            assertEquals(10_000_000_003L, x.commarea().getAcctId());
            assertEquals(4000000000000003L, x.commarea().getCardNum());
            assertEquals("COCRDLIC", x.commarea().getFromProgram());
            assertEquals("CCLI", x.commarea().getFromTranid());
            assertTrue(x.commarea().isPgmEnter());
            assertTrue(x.commarea().isUserTypeUser());
            assertEquals("CCRDLIA", x.commarea().getLastMap());
        }

        @Test
        void uOnRowTransfersToCardUpdate() {
            givenCards(20);
            Xctl x = assertInstanceOf(Xctl.class,
                    send(first(), EibAid.ENTER, CardListScreenInput.empty().withSelection(7, "U")));
            assertEquals(LIT_CARDUPDPGM, x.program());
            assertEquals(4000000000000007L, x.commarea().getCardNum());
        }

        @Test
        void moreThanOneSelectionIsRejectedAndRowsFlagged() {
            givenCards(20);
            SendMapAndReturn p2 = next(first(), EibAid.PF8);
            SendMapAndReturn r = next(p2, EibAid.ENTER,
                    CardListScreenInput.empty().withSelection(1, "S").withSelection(4, "U"));
            CardListMap m = r.screen();
            assertEquals(MSG_MORE_THAN_1_ACTION, m.errmsg.text());
            assertEquals(BmsAttributes.DFHRED, m.crdsel(1).color());
            assertEquals(BmsAttributes.DFHRED, m.crdsel(4).color());
            assertEquals(Pic.LOW_VALUE, m.crdsel(2).color());
            assertEquals("S", m.crdsel(1).text());
            assertEquals("U", m.crdsel(4).text());
            assertEquals("CRDSEL4", m.cursorField(), "row 1 never gets the cursor; input error suppresses ACCTSID");
            // the edit-error path re-reads from the start of the file but keeps the page number
            assertEquals(nums(1, 7), rowCards(m));
            assertEquals("2  ", m.pageno.data());
        }

        @Test
        void invalidActionCodeIsRejected() {
            givenCards(20);
            SendMapAndReturn r = next(first(), EibAid.ENTER, CardListScreenInput.empty().withSelection(2, "X"));
            assertEquals(MSG_INVALID_ACTION_CODE, r.screen().errmsg.text());
            assertEquals(BmsAttributes.DFHRED, r.screen().crdsel(2).color());
            assertEquals("CRDSEL2", r.screen().cursorField());
            assertEquals(BmsAttributes.DFHBMFSE, r.screen().crdsel(2).attribute());
        }

        @Test
        void invalidActionOnRowOneIsRedWithoutCursor() {
            givenCards(20);
            SendMapAndReturn r = next(first(), EibAid.ENTER, CardListScreenInput.empty().withSelection(1, "x"));
            assertEquals(MSG_INVALID_ACTION_CODE, r.screen().errmsg.text());
            assertEquals(BmsAttributes.DFHRED, r.screen().crdsel(1).color());
            assertNull(r.screen().cursorField());
        }

        @Test
        void multipleSelectionMessageWinsOverInvalidActionCode() {
            givenCards(20);
            SendMapAndReturn r = next(first(), EibAid.ENTER, CardListScreenInput.empty()
                    .withSelection(2, "S").withSelection(3, "S").withSelection(5, "Q"));
            assertEquals(MSG_MORE_THAN_1_ACTION, r.screen().errmsg.text());
            assertEquals(BmsAttributes.DFHRED, r.screen().crdsel(5).color());
        }

        @Test
        void blankSelectionsWithEnterJustRedisplay() {
            givenCards(20);
            SendMapAndReturn r = next(first(), EibAid.ENTER,
                    CardListScreenInput.empty().withSelection(1, " ").withSelection(2, ""));
            assertEquals(nums(1, 7), rowCards(r.screen()));
            assertEquals("", r.screen().errmsg.text());
        }

        @Test
        void selectionOnFirstEntryIsIgnoredBecauseMapIsNotReceived() {
            givenCards(20);
            CardListRequest req = new CardListRequest(commareaFrom(LIT_MENUPGM, false).toFixedWidth(),
                    EibAid.ENTER, CardListScreenInput.empty().withSelection(1, "S"));
            SendMapAndReturn r = screen(service.execute(req));
            assertEquals(nums(1, 7), rowCards(r.screen()));
            assertEquals(" ", r.screen().crdsel(1).data());
        }

        @Test
        void rowAttributesProtectEmptyRows() {
            givenCards(3);
            CardListMap m = first().screen();
            assertEquals(BmsAttributes.DFHBMFSE, m.crdsel(1).attribute());
            assertEquals(BmsAttributes.DFHBMFSE, m.crdsel(3).attribute());
            assertEquals(BmsAttributes.DFHBMPRO, m.crdsel(4).attribute());
            assertEquals(BmsAttributes.DFHBMPRO, m.crdsel(7).attribute());
        }

        @Test
        void emptyFirstRowUsesProtectedFsetAttribute() {
            givenCards(5);
            SendMapAndReturn r = next(first(), EibAid.ENTER, CardListScreenInput.filters("99999999999", null));
            assertEquals(BmsAttributes.DFHBMPRF, r.screen().crdsel(1).attribute());
            assertEquals(BmsAttributes.DFHBMPRO, r.screen().crdsel(2).attribute());
        }
    }

    @Nested
    class Filters {
        List<CardRecord> mixed() {
            return cards(30, i -> i % 3 == 0 ? 22_222_222_222L : 11_111_111_111L);
        }

        @Test
        void validAccountFilterShowsOnlyThatAccount() {
            givenCards(mixed());
            SendMapAndReturn r = next(first(), EibAid.ENTER, CardListScreenInput.filters("22222222222", null));
            assertEquals(List.of(num(3), num(6), num(9), num(12), num(15), num(18), num(21)), rowCards(r.screen()));
            assertEquals("22222222222", r.screen().acctsid.text());
            assertEquals(BmsAttributes.DFHBMFSE, r.screen().acctsid.attribute());
            assertEquals(22_222_222_222L, r.commarea().getAcctId());
            assertEquals("ACCTSID", r.screen().cursorField());
        }

        @Test
        void accountFilterLookAheadIsUnfilteredSoNextPageCanBeReportedSpuriously() {
            givenCards(mixed());
            SendMapAndReturn p1 = next(first(), EibAid.ENTER, CardListScreenInput.filters("22222222222", null));
            assertEquals(num(22), p1.programState().lastCardNum());
            SendMapAndReturn p2 = next(p1, EibAid.PF8, CardListScreenInput.filters("22222222222", null));
            assertEquals(List.of(num(24), num(27), num(30)), rowCards(p2.screen()));
            assertEquals(MSG_NO_MORE_RECORDS, p2.screen().errmsg.text());
        }

        @Test
        void validCardFilterShowsSingleCard() {
            givenCards(mixed());
            SendMapAndReturn r = next(first(), EibAid.ENTER, CardListScreenInput.filters(null, num(17)));
            assertEquals(List.of(num(17)), rowCards(r.screen()));
            assertEquals(num(17), r.screen().cardsid.text());
            assertEquals(4000000000000017L, r.commarea().getCardNum());
            assertEquals(MSG_NO_MORE_RECORDS, r.screen().errmsg.text());
        }

        @Test
        void accountAndCardFiltersCombine() {
            givenCards(mixed());
            SendMapAndReturn r = next(first(), EibAid.ENTER, CardListScreenInput.filters("11111111111", num(3)));
            assertEquals(MSG_NO_RECORDS_FOUND, r.screen().errmsg.text());
            assertTrue(rowCards(r.screen()).isEmpty());
        }

        @Test
        void noRecordsFoundSuppressesInfoMessage() {
            givenCards(mixed());
            SendMapAndReturn r = next(first(), EibAid.ENTER, CardListScreenInput.filters("33333333333", null));
            assertEquals(MSG_NO_RECORDS_FOUND, r.screen().errmsg.text());
            assertEquals("", r.screen().infomsg.text());
            assertEquals(BmsAttributes.DFHBMDAR, r.screen().infomsg.color());
        }

        @Test
        void nonNumericAccountFilterIsRejectedWithoutReadingFile() {
            givenCards(mixed());
            SendMapAndReturn p1 = first();
            gateway.commandLog().clear();
            SendMapAndReturn r = next(p1, EibAid.ENTER, CardListScreenInput.filters("ABC", null));
            CardListMap m = r.screen();
            assertEquals(MSG_ACCT_FILTER, m.errmsg.text());
            assertEquals("ABC", m.acctsid.text());
            assertEquals(BmsAttributes.DFHRED, m.acctsid.color());
            assertEquals("ACCTSID", m.cursorField());
            assertTrue(gateway.commandLog().isEmpty());
            assertEquals(nums(1, 7), rowCards(m), "previous rows are redisplayed from the COMMAREA");
            for (int row = 1; row <= 7; row++) {
                char expected = row == 1 ? BmsAttributes.DFHBMPRF : BmsAttributes.DFHBMPRO;
                assertEquals(expected, m.crdsel(row).attribute(), "row " + row + " protected");
            }
            assertEquals(0, r.commarea().getAcctId());
        }

        @Test
        void accountFilterShorterThanElevenDigitsIsNotNumeric() {
            givenCards(mixed());
            SendMapAndReturn r = next(first(), EibAid.ENTER, CardListScreenInput.filters("123", null));
            assertEquals(MSG_ACCT_FILTER, r.screen().errmsg.text());
        }

        @Test
        void nonNumericCardFilterIsRejected() {
            givenCards(mixed());
            SendMapAndReturn r = next(first(), EibAid.ENTER, CardListScreenInput.filters(null, "40000000000000AB"));
            assertEquals(MSG_CARD_FILTER, r.screen().errmsg.text());
            assertEquals(BmsAttributes.DFHRED, r.screen().cardsid.color());
            assertEquals("CARDSID", r.screen().cursorField());
            assertEquals(0, r.commarea().getCardNum());
        }

        @Test
        void bothFiltersInvalidKeepsAccountMessageAndFlagsBoth() {
            givenCards(mixed());
            SendMapAndReturn r = next(first(), EibAid.ENTER, CardListScreenInput.filters("X", "Y"));
            assertEquals(MSG_ACCT_FILTER, r.screen().errmsg.text());
            assertEquals(BmsAttributes.DFHRED, r.screen().acctsid.color());
            assertEquals(BmsAttributes.DFHRED, r.screen().cardsid.color());
            assertEquals("ACCTSID", r.screen().cursorField());
        }

        @Test
        void filterErrorTakesPriorityOverSelectionEdits() {
            givenCards(mixed());
            SendMapAndReturn r = next(first(), EibAid.ENTER,
                    CardListScreenInput.filters("X", null).withSelection(1, "S").withSelection(2, "S"));
            assertEquals(MSG_ACCT_FILTER, r.screen().errmsg.text());
            assertNotEquals(BmsAttributes.DFHRED, r.screen().crdsel(2).color());
        }

        @Test
        void blankOrZeroFiltersMeanNoFilter() {
            givenCards(mixed());
            SendMapAndReturn r = next(first(), EibAid.ENTER, CardListScreenInput.filters("00000000000", "    "));
            assertEquals(nums(1, 7), rowCards(r.screen()));
            assertTrue(Pic.isLowValues(r.screen().acctsid.data()));
            assertTrue(Pic.isLowValues(r.screen().cardsid.data()));
            assertEquals(0, r.commarea().getAcctId());
            assertEquals("", r.screen().errmsg.text());
        }
    }

    @Nested
    class Paging {
        @Test
        void pf8ShowsNextPageStartingAtLookAheadRecord() {
            givenCards(20);
            SendMapAndReturn p2 = next(first(), EibAid.PF8);
            assertEquals(nums(8, 14), rowCards(p2.screen()));
            assertEquals("2  ", p2.screen().pageno.data());
            assertEquals(num(15), p2.programState().lastCardNum());
            assertEquals(MSG_INFORM_REC_ACTIONS, p2.screen().infomsg.text());
        }

        @Test
        void pf7FromSecondPageReadsBackwardsAndReportsFirstPage() {
            givenCards(20);
            SendMapAndReturn p2 = next(first(), EibAid.PF8);
            gateway.commandLog().clear();
            SendMapAndReturn p1 = next(p2, EibAid.PF7);
            assertEquals(nums(1, 7), rowCards(p1.screen()));
            assertEquals("1  ", p1.screen().pageno.data());
            assertEquals(num(1), p1.programState().firstCardNum());
            assertEquals(num(8), p1.programState().lastCardNum());
            assertEquals(MSG_NO_PREVIOUS_PAGES, p1.screen().errmsg.text());
            assertEquals("STARTBR CARDDAT GTEQ '" + num(8) + "'", gateway.commandLog().get(0));
            assertEquals(8, gateway.commandLog().stream().filter(s -> s.startsWith("READPREV")).count());
        }

        @Test
        void pf7FromThirdPageReturnsSecondPageWithoutMessage() {
            givenCards(30);
            SendMapAndReturn p3 = next(next(first(), EibAid.PF8), EibAid.PF8);
            assertEquals(nums(15, 21), rowCards(p3.screen()));
            SendMapAndReturn p2 = next(p3, EibAid.PF7);
            assertEquals(nums(8, 14), rowCards(p2.screen()));
            assertEquals("2  ", p2.screen().pageno.data());
            assertEquals("", p2.screen().errmsg.text());
            assertTrue(p2.programState().isNextPageExists());
        }

        @Test
        void pf7OnFirstPageRereadsAndWarns() {
            givenCards(20);
            SendMapAndReturn r = next(first(), EibAid.PF7);
            assertEquals(nums(1, 7), rowCards(r.screen()));
            assertEquals(MSG_NO_PREVIOUS_PAGES, r.screen().errmsg.text());
        }

        @Test
        void pf8AtEndShowsInfoThenNoMorePages() {
            givenCards(10);
            SendMapAndReturn p1 = first();
            SendMapAndReturn p2 = next(p1, EibAid.PF8);
            assertEquals(nums(8, 10), rowCards(p2.screen()));
            assertEquals(MSG_NO_MORE_RECORDS, p2.screen().errmsg.text());
            assertEquals(MSG_INFORM_REC_ACTIONS, p2.screen().infomsg.text());
            assertTrue(p2.programState().isNextPageNotExists());
            assertTrue(p2.programState().isLastPageShown());

            SendMapAndReturn again = next(p2, EibAid.PF8);
            assertEquals(nums(8, 10), rowCards(again.screen()));
            assertEquals(MSG_NO_MORE_PAGES, again.screen().errmsg.text());
        }

        @Test
        void exactlySevenRecordsReportsNoMoreRecordsOnFirstPage() {
            givenCards(7);
            SendMapAndReturn p1 = first();
            assertEquals(nums(1, 7), rowCards(p1.screen()));
            assertEquals(MSG_NO_MORE_RECORDS, p1.screen().errmsg.text());
            assertTrue(p1.programState().isNextPageNotExists());
            assertTrue(p1.programState().isLastPageNotShown());

            SendMapAndReturn pf8 = next(p1, EibAid.PF8);
            assertEquals(MSG_NO_MORE_RECORDS, pf8.screen().errmsg.text());
            assertEquals(MSG_INFORM_REC_ACTIONS, pf8.screen().infomsg.text());
            assertTrue(pf8.programState().isLastPageShown());
            assertEquals(MSG_NO_MORE_PAGES, next(pf8, EibAid.PF8).screen().errmsg.text());
        }

        @Test
        void nonPf8KeyResetsLastPageShown() {
            givenCards(10);
            SendMapAndReturn p2 = next(first(), EibAid.PF8);
            SendMapAndReturn enter = next(p2, EibAid.ENTER);
            assertTrue(enter.programState().isLastPageNotShown());
        }

        @Test
        void screenNumberIsSingleDigitAndWrapsToOne() {
            givenCards(20);
            SendMapAndReturn p1 = first();
            ThisProgCommarea state = p1.programState();
            state.setScreenNum(9);
            String image = p1.commarea().toFixedWidth() + state.toFixedWidth();
            SendMapAndReturn r = screen(service.execute(CardListRequest.reenter(image, EibAid.PF8,
                    CardListScreenInput.empty())));
            assertEquals("1  ", r.screen().pageno.data());
            assertEquals(nums(8, 14), rowCards(r.screen()));
        }

        @Test
        void readBackwardsHittingStartOfFileReportsFileError() {
            givenCards(20);
            SendMapAndReturn p1 = first();
            ThisProgCommarea state = p1.programState();
            state.setScreenNum(3);
            state.setFirstCardKey(num(3), 10_000_000_003L);
            String image = p1.commarea().toFixedWidth() + state.toFixedWidth();
            SendMapAndReturn r = screen(service.execute(CardListRequest.reenter(image, EibAid.PF7,
                    CardListScreenInput.empty())));
            assertEquals("File Error: READ     on CARDDAT   returned RESP 000000020 ,RESP2 000000000",
                    r.screen().errmsg.text());
            assertEquals(List.of(num(1), num(2)), rowCards(r.screen()));
            assertEquals(num(2), r.screen().crdnum(7).text());
            assertEquals(num(1), r.screen().crdnum(6).text());
            assertEquals("2  ", r.screen().pageno.data());
        }

        @Test
        void readBackwardsFirstReadNotFoundSkipsEndBrowse() {
            givenCards(20);
            SendMapAndReturn p1 = first();
            ThisProgCommarea state = p1.programState();
            state.setScreenNum(3);
            state.setFirstCardKey("400000000000000A", 0);
            gateway.commandLog().clear();
            String image = p1.commarea().toFixedWidth() + state.toFixedWidth();
            SendMapAndReturn r = screen(service.execute(CardListRequest.reenter(image, EibAid.PF7,
                    CardListScreenInput.empty())));
            assertTrue(r.screen().errmsg.text().contains("RESP 000000013"));
            assertTrue(gateway.commandLog().stream().noneMatch(s -> s.startsWith("ENDBR")));
        }
    }

    @Nested
    class FileErrors {
        @Test
        void readNextIoErrorBuildsFileErrorMessage() {
            givenCards(20);
            gateway.injectNextRead(CicsResp.IOERR, 5);
            SendMapAndReturn r = first();
            assertEquals("File Error: READ     on CARDDAT   returned RESP 000000017 ,RESP2 000000005",
                    r.screen().errmsg.text());
            assertEquals(75, CocrdlicService.fileErrorMessage("READ", LIT_CARD_FILE, 17, 5).substring(0, 75)
                    .length());
        }

        @Test
        void lookAheadErrorAfterFullPageReportsFileError() {
            givenCards(20);
            SendMapAndReturn p1 = first();
            InMemoryCardFileGateway failing = new InMemoryCardFileGateway(cards(20, i -> 1L)) {
                int reads;

                @Override
                public CardFileGateway.ReadResult readNext(String file) {
                    if (++reads == 8) {
                        return new CardFileGateway.ReadResult(CicsResp.NOTOPEN, 60, null);
                    }
                    return super.readNext(file);
                }
            };
            SendMapAndReturn r = screen(new CocrdlicService(failing, CLOCK)
                    .execute(CardListRequest.reenter(p1.commareaImage(), EibAid.ENTER, CardListScreenInput.empty())));
            assertEquals(7, rowCards(r.screen()).size());
            assertTrue(r.screen().errmsg.text().contains("RESP 000000019 ,RESP2 000000060"));
        }

        @Test
        void emptyFileAbendsOnEndBrowse() {
            givenCards(List.of());
            CicsAbendException e = assertThrows(CicsAbendException.class, () -> first());
            assertEquals("AEIP", e.abendCode());
        }
    }
}
