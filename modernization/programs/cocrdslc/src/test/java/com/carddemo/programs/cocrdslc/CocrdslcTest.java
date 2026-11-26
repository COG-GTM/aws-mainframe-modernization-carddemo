package com.carddemo.programs.cocrdslc;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.programs.cocrdslc.CocrdslcResult.Abended;
import com.carddemo.programs.cocrdslc.CocrdslcResult.ScreenSent;
import com.carddemo.programs.cocrdslc.CocrdslcResult.TextSent;
import com.carddemo.programs.cocrdslc.CocrdslcResult.Xctl;
import com.carddemo.programs.cocrdslc.copybook.CardRecord;
import com.carddemo.programs.cocrdslc.copybook.CarddemoCommarea;
import com.carddemo.programs.cocrdslc.copybook.CcrdslaField;
import com.carddemo.programs.cocrdslc.copybook.CcrdslaInput;
import com.carddemo.programs.cocrdslc.copybook.Dfhaid;
import com.carddemo.programs.cocrdslc.copybook.Dfhbmsca;
import com.carddemo.programs.cocrdslc.copybook.ScreenTitles;
import java.time.Clock;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Optional;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

class CocrdslcTest {

    static final String CARD = "0500024453765740";
    static final long ACCT = 50L;
    static final CardRecord AN_CARD = new CardRecord(CARD, ACCT, 747, "Aniya Von", "2023-03-09", "Y");
    static final Clock CLOCK = Clock.fixed(
            LocalDateTime.of(2026, 11, 26, 9, 5, 7).toInstant(ZoneOffset.UTC), ZoneOffset.UTC);

    InMemoryCardFileGateway gateway;
    Cocrdslc program;

    @BeforeEach
    void setUp() {
        gateway = new InMemoryCardFileGateway().put(AN_CARD);
        program = new Cocrdslc(gateway, CLOCK);
    }

    static String lv(int n) {
        return CobolText.lowValues(n);
    }

    static String pad(String s, int n) {
        return CobolText.pic(s, n);
    }

    static CarddemoCommarea reenter() {
        CarddemoCommarea c = CarddemoCommarea.initialized();
        c.cdemoFromTranid = "CCDL";
        c.cdemoFromProgram = "COCRDSLC";
        c.setPgmReenter();
        return c;
    }

    static CarddemoCommarea fromList(long acct, long card) {
        CarddemoCommarea c = CarddemoCommarea.initialized();
        c.cdemoFromTranid = "CCLI";
        c.cdemoFromProgram = "COCRDLIC";
        c.cdemoLastMapset = "COCRDLI";
        c.cdemoLastMap = "CCRDLIA";
        c.setPgmEnter();
        c.acctId(acct);
        c.cardNum(card);
        return c;
    }

    ScreenSent screen(CocrdslcResult r) {
        return assertInstanceOf(ScreenSent.class, r);
    }

    ScreenSent submit(String acct, String card) {
        return screen(program.execute(CocrdslcRequest.of(reenter(), Dfhaid.DFHENTER, CcrdslaInput.of(acct, card))));
    }

    @Nested
    class FirstEntry {
        @Test
        void noCommareaSendsEmptySearchScreenWithPrompt() {
            ScreenSent s = screen(program.execute(CocrdslcRequest.firstTime(Dfhaid.DFHENTER)));
            assertEquals("CCDL", s.transid());
            assertEquals(pad(Cocrdslc.WS_PROMPT_FOR_INPUT, 40), s.map().data(CcrdslaField.INFOMSG));
            assertEquals(pad("", 80), s.map().data(CcrdslaField.ERRMSG));
            assertEquals(lv(11), s.map().data(CcrdslaField.ACCTSID));
            assertEquals(lv(16), s.map().data(CcrdslaField.CARDSID));
            assertEquals(Optional.of(CcrdslaField.ACCTSID), s.map().cursor());
            assertEquals(Dfhbmsca.DFHBMFSE, s.map().field(CcrdslaField.ACCTSID).attribute());
            assertEquals(Dfhbmsca.DFHBMFSE, s.map().field(CcrdslaField.CARDSID).attribute());
            assertEquals(Dfhbmsca.DFHDFCOL, s.map().field(CcrdslaField.ACCTSID).color());
            assertEquals(Dfhbmsca.DFHNEUTR, s.map().field(CcrdslaField.INFOMSG).color());
            assertEquals(lv(50), s.map().data(CcrdslaField.CRDNAME));
            assertEquals(lv(75), s.map().data(CcrdslaField.FKEYS));
        }

        @Test
        void screenHeaderCarriesTitlesTranProgramDateAndTime() {
            ScreenSent s = screen(program.execute(CocrdslcRequest.firstTime(Dfhaid.DFHENTER)));
            assertEquals(ScreenTitles.CCDA_TITLE01, s.map().data(CcrdslaField.TITLE01));
            assertEquals(ScreenTitles.CCDA_TITLE02, s.map().data(CcrdslaField.TITLE02));
            assertEquals("CCDL", s.map().data(CcrdslaField.TRNNAME));
            assertEquals("COCRDSLC", s.map().data(CcrdslaField.PGMNAME));
            assertEquals("11/26/26", s.map().data(CcrdslaField.CURDATE));
            assertEquals("09:05:07", s.map().data(CcrdslaField.CURTIME));
        }

        @Test
        void commonReturnPassesInitialisedCommareaFlaggedForReentry() {
            ScreenSent s = screen(program.execute(CocrdslcRequest.firstTime(Dfhaid.DFHENTER)));
            assertEquals(Cocrdslc.WS_COMMAREA_LENGTH, s.commarea().length());
            CarddemoCommarea c = s.carddemoCommarea();
            assertTrue(c.isPgmReenter());
            assertEquals("        ", c.cdemoFromProgram);
            assertEquals(0L, c.acctId());
            assertEquals("            ", s.thisProgCommarea().toFixedWidth());
            assertTrue(CobolText.isSpaces(s.commarea().substring(172)));
        }

        @Test
        void comingFromMenuShowsBlankSearchFieldsAndKeepsCommarea() {
            CarddemoCommarea c = CarddemoCommarea.initialized();
            c.cdemoFromTranid = "CM00";
            c.cdemoFromProgram = "COMEN01C";
            c.cdemoUserId = "USER0001";
            c.cdemoCustFname = "Pat";
            c.acctId(ACCT);
            c.cardNum(Long.parseLong(CARD));
            ScreenSent s = screen(program.execute(CocrdslcRequest.of(c, Dfhaid.DFHENTER, null)));
            // CC-ACCT-ID / CC-CARD-NUM were INITIALIZEd to spaces and never loaded on this path
            assertEquals(pad("", 11), s.map().data(CcrdslaField.ACCTSID));
            assertEquals(pad("", 16), s.map().data(CcrdslaField.CARDSID));
            assertEquals(pad(Cocrdslc.WS_PROMPT_FOR_INPUT, 40), s.map().data(CcrdslaField.INFOMSG));
            CarddemoCommarea back = s.carddemoCommarea();
            assertEquals("USER0001", back.cdemoUserId);
            assertEquals(pad("Pat", 25), back.cdemoCustFname);
            assertEquals(ACCT, back.acctId());
            assertTrue(back.isPgmReenter());
        }

        @Test
        void nonDigitPgmContextIsAnUnexpectedDataScenario() {
            CarddemoCommarea c = CarddemoCommarea.initialized();
            c.cdemoPgmContext = " ";
            TextSent t = assertInstanceOf(TextSent.class, program.execute(CocrdslcRequest.of(c, Dfhaid.DFHENTER, null)));
            assertEquals(pad(Cocrdslc.MSG_UNEXPECTED_DATA_SCENARIO, 75), t.text());
        }

        @Test
        void searchUsesTheCommareaReturnedByTheFirstScreen() {
            ScreenSent first = screen(program.execute(CocrdslcRequest.firstTime(Dfhaid.DFHENTER)));
            ScreenSent second = screen(program.execute(new CocrdslcRequest(first.commarea(), Dfhaid.DFHENTER,
                    CcrdslaInput.of("00000000050", CARD))));
            assertEquals(pad("Aniya Von", 50), second.map().data(CcrdslaField.CRDNAME));
            assertEquals(pad(Cocrdslc.FOUND_CARDS_FOR_ACCOUNT, 40), second.map().data(CcrdslaField.INFOMSG));
            ScreenSent third = screen(program.execute(new CocrdslcRequest(second.commarea(), Dfhaid.DFHENTER,
                    CcrdslaInput.of("", ""))));
            assertEquals(pad(Cocrdslc.NO_SEARCH_CRITERIA_RECEIVED, 80), third.map().data(CcrdslaField.ERRMSG));
            Xctl exit = assertInstanceOf(Xctl.class,
                    program.execute(new CocrdslcRequest(third.commarea(), Dfhaid.DFHPF3, null)));
            assertEquals("COMEN01C", exit.program());
        }
    }

    @Nested
    class Pf3Exit {
        @Test
        void withoutCallerXctlsToMainMenu() {
            Xctl x = assertInstanceOf(Xctl.class, program.execute(CocrdslcRequest.firstTime(Dfhaid.DFHPF3)));
            assertEquals("COMEN01C", x.program());
            CarddemoCommarea c = x.commarea();
            assertEquals("CM00", c.cdemoToTranid);
            assertEquals("COMEN01C", c.cdemoToProgram);
            assertEquals("CCDL", c.cdemoFromTranid);
            assertEquals("COCRDSLC", c.cdemoFromProgram);
            assertTrue(c.isUsrtypUser());
            assertTrue(c.isPgmEnter());
            assertEquals("COCRDSL", c.cdemoLastMapset);
            assertEquals("CCRDSLA", c.cdemoLastMap);
        }

        @Test
        void returnsToCallingProgramAndDowngradesAdminToUser() {
            CarddemoCommarea c = fromList(ACCT, Long.parseLong(CARD));
            c.setUsrtypAdmin();
            c.setPgmReenter();
            Xctl x = assertInstanceOf(Xctl.class, program.execute(CocrdslcRequest.of(c, Dfhaid.DFHPF3, null)));
            assertEquals("COCRDLIC", x.program());
            assertEquals("CCLI", x.commarea().cdemoToTranid);
            assertTrue(x.commarea().isUsrtypUser());
            assertFalse(x.commarea().isUsrtypAdmin());
        }

        @Test
        void lowValueCallerFieldsFallBackToMenu() {
            CarddemoCommarea c = reenter();
            c.cdemoFromTranid = lv(4);
            c.cdemoFromProgram = lv(8);
            Xctl x = assertInstanceOf(Xctl.class, program.execute(CocrdslcRequest.of(c, Dfhaid.DFHPF3, null)));
            assertEquals("COMEN01C", x.program());
            assertEquals("CM00", x.commarea().cdemoToTranid);
        }

        @Test
        void pf15FoldsOntoPf3() {
            Xctl x = assertInstanceOf(Xctl.class, program.execute(CocrdslcRequest.firstTime(Dfhaid.DFHPF15)));
            assertEquals("COMEN01C", x.program());
        }
    }

    @ParameterizedTest
    @EnumSource(value = Dfhaid.class, names = {"DFHCLEAR", "DFHPA1", "DFHPA2", "DFHPA3", "DFHPF1", "DFHPF5",
        "DFHPF12", "DFHPF24", "DFHNULL"})
    void anyOtherKeyIsTreatedAsEnter(Dfhaid aid) {
        ScreenSent s = screen(program.execute(
                CocrdslcRequest.of(reenter(), aid, CcrdslaInput.of("00000000050", CARD))));
        assertEquals(pad("Aniya Von", 50), s.map().data(CcrdslaField.CRDNAME));
    }

    @Nested
    class FromCardList {
        @Test
        void foundCardIsDisplayedWithProtectedKeys() {
            ScreenSent s = screen(program.execute(
                    CocrdslcRequest.of(fromList(ACCT, Long.parseLong(CARD)), Dfhaid.DFHENTER, null)));
            assertEquals("00000000050", s.map().data(CcrdslaField.ACCTSID));
            assertEquals(CARD, s.map().data(CcrdslaField.CARDSID));
            assertEquals(pad("Aniya Von", 50), s.map().data(CcrdslaField.CRDNAME));
            assertEquals("03", s.map().data(CcrdslaField.EXPMON));
            assertEquals("2023", s.map().data(CcrdslaField.EXPYEAR));
            assertEquals("Y", s.map().data(CcrdslaField.CRDSTCD));
            assertEquals(pad(Cocrdslc.FOUND_CARDS_FOR_ACCOUNT, 40), s.map().data(CcrdslaField.INFOMSG));
            assertEquals(pad("", 80), s.map().data(CcrdslaField.ERRMSG));
            assertEquals(Dfhbmsca.DFHBMPRF, s.map().field(CcrdslaField.ACCTSID).attribute());
            assertEquals(Dfhbmsca.DFHBMPRF, s.map().field(CcrdslaField.CARDSID).attribute());
            assertEquals(Dfhbmsca.DFHDFCOL, s.map().field(CcrdslaField.ACCTSID).color());
            assertEquals(Optional.of(CcrdslaField.ACCTSID), s.map().cursor());
            assertTrue(s.carddemoCommarea().isPgmReenter());
        }

        @Test
        void unknownCardShowsNotFoundMessageInRed() {
            ScreenSent s = screen(program.execute(
                    CocrdslcRequest.of(fromList(ACCT, 1234567890123456L), Dfhaid.DFHENTER, null)));
            assertEquals(pad(Cocrdslc.DID_NOT_FIND_ACCTCARD_COMBO, 80), s.map().data(CcrdslaField.ERRMSG));
            assertEquals(Dfhbmsca.DFHRED, s.map().field(CcrdslaField.ACCTSID).color());
            assertEquals(Dfhbmsca.DFHRED, s.map().field(CcrdslaField.CARDSID).color());
            assertEquals(lv(50), s.map().data(CcrdslaField.CRDNAME));
            assertEquals(pad(Cocrdslc.WS_PROMPT_FOR_INPUT, 40), s.map().data(CcrdslaField.INFOMSG));
        }

        @Test
        void keysStayUnprotectedWhenLastMapsetIsNotTheListScreen() {
            CarddemoCommarea c = fromList(ACCT, Long.parseLong(CARD));
            c.cdemoLastMapset = "COMEN01";
            ScreenSent s = screen(program.execute(CocrdslcRequest.of(c, Dfhaid.DFHENTER, null)));
            assertEquals(Dfhbmsca.DFHBMFSE, s.map().field(CcrdslaField.ACCTSID).attribute());
            assertEquals(pad("Aniya Von", 50), s.map().data(CcrdslaField.CRDNAME));
        }
    }

    @Nested
    class ReentryEdits {
        @Test
        void validAccountAndCardDisplaysDetails() {
            ScreenSent s = submit("00000000050", CARD);
            assertEquals("00000000050", s.map().data(CcrdslaField.ACCTSID));
            assertEquals(CARD, s.map().data(CcrdslaField.CARDSID));
            assertEquals(pad("Aniya Von", 50), s.map().data(CcrdslaField.CRDNAME));
            assertEquals(pad(Cocrdslc.FOUND_CARDS_FOR_ACCOUNT, 40), s.map().data(CcrdslaField.INFOMSG));
            assertEquals(Dfhbmsca.DFHBMFSE, s.map().field(CcrdslaField.ACCTSID).attribute());
            assertEquals(Optional.of(CcrdslaField.ACCTSID), s.map().cursor());
            CarddemoCommarea c = s.carddemoCommarea();
            assertEquals(ACCT, c.acctId());
            assertEquals(Long.parseLong(CARD), c.cardNum());
        }

        @Test
        void cardIsReadByCardNumberOnlyAccountIsNotCrossChecked() {
            ScreenSent s = submit("99999999999", CARD);
            assertEquals(pad("Aniya Von", 50), s.map().data(CcrdslaField.CRDNAME));
            assertEquals(pad("", 80), s.map().data(CcrdslaField.ERRMSG));
        }

        @Test
        void noInputReportsNoInputReceivedAndStarsBothFields() {
            ScreenSent s = submit("", "");
            assertEquals(pad(Cocrdslc.NO_SEARCH_CRITERIA_RECEIVED, 80), s.map().data(CcrdslaField.ERRMSG));
            assertEquals(pad("*", 11), s.map().data(CcrdslaField.ACCTSID));
            assertEquals(pad("*", 16), s.map().data(CcrdslaField.CARDSID));
            assertEquals(Dfhbmsca.DFHRED, s.map().field(CcrdslaField.ACCTSID).color());
            assertEquals(Dfhbmsca.DFHRED, s.map().field(CcrdslaField.CARDSID).color());
            assertEquals(Optional.of(CcrdslaField.ACCTSID), s.map().cursor());
        }

        @Test
        void notTransmittedFieldsBehaveLikeBlank() {
            ScreenSent s = submit(null, null);
            assertEquals(pad(Cocrdslc.NO_SEARCH_CRITERIA_RECEIVED, 80), s.map().data(CcrdslaField.ERRMSG));
        }

        @Test
        void starPlaceholdersAreTreatedAsBlank() {
            ScreenSent s = submit("*", "*");
            assertEquals(pad(Cocrdslc.NO_SEARCH_CRITERIA_RECEIVED, 80), s.map().data(CcrdslaField.ERRMSG));
        }

        @Test
        void missingAccountPromptsForAccountAndKeepsCard() {
            ScreenSent s = submit("", CARD);
            assertEquals(pad(Cocrdslc.WS_PROMPT_FOR_ACCT, 80), s.map().data(CcrdslaField.ERRMSG));
            assertEquals(pad("*", 11), s.map().data(CcrdslaField.ACCTSID));
            assertEquals(Dfhbmsca.DFHRED, s.map().field(CcrdslaField.ACCTSID).color());
            assertEquals(CARD, s.map().data(CcrdslaField.CARDSID));
            assertEquals(0, s.map().field(CcrdslaField.CARDSID).color());
            assertEquals(Optional.of(CcrdslaField.ACCTSID), s.map().cursor());
            assertEquals(lv(50), s.map().data(CcrdslaField.CRDNAME));
        }

        @Test
        void missingCardPromptsForCardAndPutsCursorOnCard() {
            ScreenSent s = submit("00000000050", "");
            assertEquals(pad(Cocrdslc.WS_PROMPT_FOR_CARD, 80), s.map().data(CcrdslaField.ERRMSG));
            assertEquals("00000000050", s.map().data(CcrdslaField.ACCTSID));
            assertEquals(pad("*", 16), s.map().data(CcrdslaField.CARDSID));
            assertEquals(Dfhbmsca.DFHRED, s.map().field(CcrdslaField.CARDSID).color());
            assertEquals(Optional.of(CcrdslaField.CARDSID), s.map().cursor());
        }

        @Test
        void allZeroAccountIsTreatedAsNotProvided() {
            ScreenSent s = submit("00000000000", CARD);
            assertEquals(pad(Cocrdslc.WS_PROMPT_FOR_ACCT, 80), s.map().data(CcrdslaField.ERRMSG));
        }

        @Test
        void singleZeroWithTrailingSpacesIsNotZerosButNonNumeric() {
            ScreenSent s = submit("0", CARD);
            assertEquals(pad(Cocrdslc.MSG_ACCT_FILTER_NOT_NUMERIC, 80), s.map().data(CcrdslaField.ERRMSG));
        }

        @Test
        void nonNumericAccountIsRejectedWithExactMessage() {
            ScreenSent s = submit("ABC", CARD);
            assertEquals(pad("ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER", 80),
                    s.map().data(CcrdslaField.ERRMSG));
            assertEquals(lv(11), s.map().data(CcrdslaField.ACCTSID));
            assertEquals(Dfhbmsca.DFHRED, s.map().field(CcrdslaField.ACCTSID).color());
            assertEquals(Optional.of(CcrdslaField.ACCTSID), s.map().cursor());
            assertEquals(lv(50), s.map().data(CcrdslaField.CRDNAME));
        }

        @Test
        void shortAccountIsNotNumericBecauseOfPaddingSpaces() {
            ScreenSent s = submit("50", CARD);
            assertEquals(pad(Cocrdslc.MSG_ACCT_FILTER_NOT_NUMERIC, 80), s.map().data(CcrdslaField.ERRMSG));
        }

        @Test
        void nonNumericCardIsRejectedWithExactMessage() {
            ScreenSent s = submit("00000000050", "05000244537657XX");
            assertEquals(pad("CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER", 80),
                    s.map().data(CcrdslaField.ERRMSG));
            assertEquals(lv(16), s.map().data(CcrdslaField.CARDSID));
            assertEquals(Dfhbmsca.DFHRED, s.map().field(CcrdslaField.CARDSID).color());
            assertEquals(Optional.of(CcrdslaField.CARDSID), s.map().cursor());
        }

        @Test
        void firstErrorMessageWinsWhenBothFieldsAreInvalid() {
            ScreenSent s = submit("ABC", "XYZ");
            assertEquals(pad(Cocrdslc.MSG_ACCT_FILTER_NOT_NUMERIC, 80), s.map().data(CcrdslaField.ERRMSG));
            assertEquals(Dfhbmsca.DFHRED, s.map().field(CcrdslaField.ACCTSID).color());
            assertEquals(Dfhbmsca.DFHRED, s.map().field(CcrdslaField.CARDSID).color());
        }

        @Test
        void blankAccountMessageIsNotOverwrittenByCardError() {
            ScreenSent s = submit("", "XYZ");
            assertEquals(pad(Cocrdslc.WS_PROMPT_FOR_ACCT, 80), s.map().data(CcrdslaField.ERRMSG));
        }

        @Test
        void unknownCardReportsNotFound() {
            ScreenSent s = submit("00000000050", "1111222233334444");
            assertEquals(pad(Cocrdslc.DID_NOT_FIND_ACCTCARD_COMBO, 80), s.map().data(CcrdslaField.ERRMSG));
            assertEquals(Dfhbmsca.DFHRED, s.map().field(CcrdslaField.ACCTSID).color());
            assertEquals(Dfhbmsca.DFHRED, s.map().field(CcrdslaField.CARDSID).color());
            assertEquals(Optional.of(CcrdslaField.ACCTSID), s.map().cursor());
        }

        @Test
        void fileErrorFormatsResponseCodes() {
            gateway.failWith(CicsResp.NOTOPEN, 0);
            ScreenSent s = submit("00000000050", CARD);
            String expected = "File Error: READ     on CARDDAT   returned RESP 000000019 ,RESP2 000000000 ";
            assertEquals(75, expected.length());
            assertEquals(pad(expected, 80), s.map().data(CcrdslaField.ERRMSG));
            assertEquals(Dfhbmsca.DFHRED, s.map().field(CcrdslaField.ACCTSID).color());
            assertEquals(0, s.map().field(CcrdslaField.CARDSID).color());
        }

        @Test
        void reentryPreservesThisProgramCommareaTail() {
            ThisProgCommarea t = ThisProgCommarea.parse("COCRDLICCCLI");
            ScreenSent s = screen(program.execute(CocrdslcRequest.of(reenter(), t, Dfhaid.DFHENTER,
                    CcrdslaInput.of("00000000050", CARD))));
            assertEquals("COCRDLICCCLI", s.thisProgCommarea().toFixedWidth());
        }
    }

    @Nested
    class ErrorPaths {
        @Test
        void unexpectedPgmContextSendsPlainTextAndEndsTask() {
            CarddemoCommarea c = reenter();
            c.cdemoPgmContext = "2";
            TextSent t = assertInstanceOf(TextSent.class, program.execute(CocrdslcRequest.of(c, Dfhaid.DFHENTER, null)));
            assertEquals(pad("UNEXPECTED DATA SCENARIO", 75), t.text());
            assertEquals("0001", t.abendData().abendCode());
            assertEquals("COCRDSLC", t.abendData().abendCulprit());
            assertEquals(pad("", 50), t.abendData().abendReason());
        }

        @Test
        void runtimeFailureRunsAbendRoutineWith9999() {
            RuntimeException boom = new IllegalStateException("VSAM gone");
            Cocrdslc failing = new Cocrdslc(new CardFileGateway() {
                @Override
                public CardReadResponse readCardDat(String cardNum16) {
                    throw boom;
                }

                @Override
                public CardReadResponse readCardAix(String acctId11) {
                    throw boom;
                }
            }, CLOCK);
            Abended a = assertInstanceOf(Abended.class, failing.execute(
                    CocrdslcRequest.of(reenter(), Dfhaid.DFHENTER, CcrdslaInput.of("00000000050", CARD))));
            assertEquals("9999", a.abcode());
            assertEquals("COCRDSLC", a.abendData().abendCulprit());
            // ABEND-MSG has VALUE SPACES, not LOW-VALUES, so the default text is never moved in
            assertEquals(pad("", 72), a.abendData().abendMsg());
            assertSame(boom, a.cause());
            assertEquals(134, a.abendData().toFixedWidth().length());
        }

        @Test
        void abendRoutineSuppliesDefaultMessageWhenAbendMsgIsLowValues() {
            Cocrdslc.Task task = program.newTask(CocrdslcRequest.firstTime(Dfhaid.DFHENTER));
            task.abendData.abendMsg(lv(72));
            Abended a = assertInstanceOf(Abended.class, task.abendRoutine(null));
            assertEquals(pad("UNEXPECTED ABEND OCCURRED.", 72), a.abendData().abendMsg());
        }

        @Test
        void sendLongTextReturnsLongMessage() {
            Cocrdslc.Task task = program.newTask(CocrdslcRequest.firstTime(Dfhaid.DFHENTER));
            task.wsLongMsg = pad("debug", 500);
            RuntimeException ret = assertThrows(RuntimeException.class, task::sendLongText);
            assertEquals("CicsReturn", ret.getClass().getSimpleName());
        }
    }

    @Nested
    class GetCardByAcct9150 {
        Cocrdslc.Task task() {
            Cocrdslc.Task t = program.newTask(CocrdslcRequest.firstTime(Dfhaid.DFHENTER));
            return t;
        }

        @Test
        void foundByAlternateIndex() {
            Cocrdslc.Task t = task();
            t.wsCardRidAcctId = CobolText.zoned(ACCT, 11);
            t.getCardByAcct9150();
            assertTrue(t.isFoundCardsForAccount());
            assertEquals(CARD, t.cardRecord.cardNum());
        }

        @Test
        void duplicateAlternateKeyIsAFileError() {
            gateway.put(new CardRecord("1111222233334444", ACCT, 123, "Second Card", "2025-01-31", "Y"));
            Cocrdslc.Task t = task();
            t.wsCardRidAcctId = CobolText.zoned(ACCT, 11);
            t.getCardByAcct9150();
            assertTrue(t.isInputError());
            assertFalse(t.isFoundCardsForAccount());
            assertEquals("File Error: READ     on CARDAIX   returned RESP 000000015 ,RESP2 000000140 ",
                    t.wsReturnMsg());
        }

        @Test
        void alternateKeyMatchesAllElevenBytes() {
            gateway.put(new CardRecord("1111222233334444", 51, 123, "Other", "2025-01-31", "Y"));
            Cocrdslc.Task t = task();
            t.wsCardRidAcctId = "0000000005A";
            t.getCardByAcct9150();
            assertEquals(pad(Cocrdslc.DID_NOT_FIND_ACCT_IN_CARDXREF, 75), t.wsReturnMsg());
        }

        @Test
        void notFoundSetsXrefMessage() {
            Cocrdslc.Task t = task();
            t.getCardByAcct9150();
            assertTrue(t.isInputError());
            assertTrue(t.isAcctFilterNotOk());
            assertEquals(pad(Cocrdslc.DID_NOT_FIND_ACCT_IN_CARDXREF, 75), t.wsReturnMsg());
        }

        @Test
        void errorNamesTheAlternateIndexPath() {
            gateway.failWith(CicsResp.DISABLED, 50);
            Cocrdslc.Task t = task();
            t.getCardByAcct9150();
            assertEquals("File Error: READ     on CARDAIX   returned RESP 000000084 ,RESP2 000000050 ",
                    t.wsReturnMsg());
        }
    }

    @Nested
    class EditParagraphs {
        Cocrdslc.Task task(String acct, String card) {
            Cocrdslc.Task t = program.newTask(
                    CocrdslcRequest.of(reenter(), Dfhaid.DFHENTER, CcrdslaInput.of(acct, card)));
            t.commarea = reenter();
            t.receiveMap2100();
            return t;
        }

        @Test
        void validInputsLeaveBothFiltersValidAndInputOk() {
            Cocrdslc.Task t = task("00000000050", CARD);
            t.editMapInputs2200();
            assertTrue(t.isInputOk());
            assertTrue(t.isAcctFilterIsValid());
            assertTrue(t.isCardFilterIsValid());
            assertTrue(t.isReturnMsgOff());
        }

        @Test
        void blankAccountSetsBlankFlagAndZeroesCommareaAccount() {
            Cocrdslc.Task t = task("", CARD);
            t.commarea.acctId(123);
            t.editMapInputs2200();
            assertTrue(t.isAcctFilterBlank());
            assertTrue(t.isInputError());
            assertEquals(0L, t.commarea.acctId());
        }

        @Test
        void processInputsCopiesMessageAndNextScreenToWorkArea() {
            Cocrdslc.Task t = task("", "");
            t.processInputs2000();
            assertEquals(pad(Cocrdslc.NO_SEARCH_CRITERIA_RECEIVED, 75), t.cc.ccardErrorMsg);
            assertEquals("COCRDSLC", t.cc.ccardNextProg);
            assertEquals("COCRDSL", t.cc.ccardNextMapset);
            assertEquals("CCRDSLA", t.cc.ccardNextMap);
        }
    }

    @Test
    void cardRecordFixtureMatchesAnInMemoryRead() {
        assertEquals(CardReadResponse.normal(AN_CARD), gateway.readCardDat(CARD));
        assertEquals(CicsResp.NOTFND, gateway.readCardDat("0000000000000000").resp());
    }
}
