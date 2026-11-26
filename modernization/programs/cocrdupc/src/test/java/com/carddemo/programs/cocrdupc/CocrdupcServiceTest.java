package com.carddemo.programs.cocrdupc;

import static com.carddemo.programs.cocrdupc.copybook.CcrdupaMap.Field.ACCTSID;
import static com.carddemo.programs.cocrdupc.copybook.CcrdupaMap.Field.CARDSID;
import static com.carddemo.programs.cocrdupc.copybook.CcrdupaMap.Field.CRDNAME;
import static com.carddemo.programs.cocrdupc.copybook.CcrdupaMap.Field.CRDSTCD;
import static com.carddemo.programs.cocrdupc.copybook.CcrdupaMap.Field.CURDATE;
import static com.carddemo.programs.cocrdupc.copybook.CcrdupaMap.Field.CURTIME;
import static com.carddemo.programs.cocrdupc.copybook.CcrdupaMap.Field.ERRMSG;
import static com.carddemo.programs.cocrdupc.copybook.CcrdupaMap.Field.EXPDAY;
import static com.carddemo.programs.cocrdupc.copybook.CcrdupaMap.Field.EXPMON;
import static com.carddemo.programs.cocrdupc.copybook.CcrdupaMap.Field.EXPYEAR;
import static com.carddemo.programs.cocrdupc.copybook.CcrdupaMap.Field.FKEYSC;
import static com.carddemo.programs.cocrdupc.copybook.CcrdupaMap.Field.INFOMSG;
import static com.carddemo.programs.cocrdupc.copybook.CcrdupaMap.Field.PGMNAME;
import static com.carddemo.programs.cocrdupc.copybook.CcrdupaMap.Field.TITLE01;
import static com.carddemo.programs.cocrdupc.copybook.CcrdupaMap.Field.TITLE02;
import static com.carddemo.programs.cocrdupc.copybook.CcrdupaMap.Field.TRNNAME;
import static com.carddemo.programs.cocrdupc.copybook.Dfhbmsca.DFHBMBRY;
import static com.carddemo.programs.cocrdupc.copybook.Dfhbmsca.DFHBMDAR;
import static com.carddemo.programs.cocrdupc.copybook.Dfhbmsca.DFHBMFSE;
import static com.carddemo.programs.cocrdupc.copybook.Dfhbmsca.DFHBMPRF;
import static com.carddemo.programs.cocrdupc.copybook.Dfhbmsca.DFHRED;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.programs.cocrdupc.CocrdupcResponse.Kind;
import com.carddemo.programs.cocrdupc.copybook.AidKey;
import com.carddemo.programs.cocrdupc.copybook.CardDemoCommarea;
import com.carddemo.programs.cocrdupc.copybook.CardRecord;

import java.time.LocalDateTime;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class CocrdupcServiceTest {

    static final String ACCT = "00000000050";
    static final String CARD = "0500024453765740";
    static final CardRecord ANIYA = new CardRecord(CARD, 50, 747, "Aniya Von", "2023-03-09", "Y", "");
    static final LocalDateTime NOW = LocalDateTime.of(2026, 11, 26, 17, 22, 33);

    InMemoryCardFileGateway file;
    CocrdupcService service;

    @BeforeEach
    void setUp() {
        file = new InMemoryCardFileGateway();
        file.put(ANIYA);
        service = new CocrdupcService(file, () -> NOW);
    }

    CocrdupcResponse send(String commarea, AidKey aid, CcrdupaInput in) {
        return service.process(new CocrdupcRequest(commarea, aid, in));
    }

    CocrdupcResponse fresh() {
        return send(null, AidKey.ENTER, null);
    }

    CocrdupcResponse search(String acct, String card) {
        return send(fresh().commarea(), AidKey.ENTER, CcrdupaInput.search(acct, card));
    }

    CocrdupcResponse shown() {
        return search(ACCT, CARD);
    }

    static CcrdupaInput edit(String name, String status, String mon, String year) {
        return new CcrdupaInput(ACCT, CARD, name, status, mon, year, "09");
    }

    CocrdupcResponse validated() {
        return send(shown().commarea(), AidKey.ENTER, edit("Aniya Smith", "N", "12", "2027"));
    }

    static String commareaFrom(CardDemoCommarea c, CardUpdateProgramArea p) {
        return Cobol.alnum(c.encode() + p.encode(), 2000);
    }

    @Nested
    class MainFreshEntry {
        @Test
        void eibcalenZeroShowsSearchPrompt() {
            CocrdupcResponse r = fresh();
            assertEquals(Kind.RETURN_TRANSID, r.kind());
            assertEquals("CCUP", r.transId());
            assertEquals(2000, r.commarea().length());
            assertTrue(r.carddemoCommarea().isPgmReenter());
            assertEquals(Cobol.LOW_VALUE, r.programArea().changeAction());
            assertEquals("Please enter Account and Card Number", r.screen().text(INFOMSG));
            assertEquals("", r.screen().text(ERRMSG));
            assertEquals(ACCTSID, r.screen().cursorField());
            assertEquals(DFHBMFSE, r.screen().field(ACCTSID).attribute());
            assertEquals(DFHBMFSE, r.screen().field(CARDSID).attribute());
            assertEquals(DFHBMPRF, r.screen().field(CRDNAME).attribute());
            assertEquals(DFHBMBRY, r.screen().field(INFOMSG).attribute());
            assertEquals(0, r.screen().field(ACCTSID).color());
        }

        @Test
        void screenHeaderUsesTitlesTranidProgramAndClock() {
            CocrdupcResponse r = fresh();
            assertEquals("COCRDUP", r.screen().mapset());
            assertEquals("CCRDUPA", r.screen().map());
            assertEquals("AWS Mainframe Modernization", r.screen().text(TITLE01).strip());
            assertEquals("CardDemo", r.screen().text(TITLE02).strip());
            assertEquals("CCUP", r.screen().text(TRNNAME));
            assertEquals("COCRDUPC", r.screen().text(PGMNAME));
            assertEquals("11/26/26", r.screen().text(CURDATE));
            assertEquals("17:22:33", r.screen().text(CURTIME));
            assertEquals(DFHBMDAR, r.screen().field(EXPDAY).color());
        }

        @Test
        void entryFromMenuWithCommareaKeepsCallerContext() {
            CardDemoCommarea c = CardDemoCommarea.initialized();
            c.setFromTranid("CM00");
            c.setFromProgram("COMEN01C");
            c.setUserId("USER0001");
            c.setAcctId(77);
            CardUpdateProgramArea p = CardUpdateProgramArea.uninitialized();
            p.initialize();
            p.setChangeAction('S');
            CocrdupcResponse r = send(commareaFrom(c, p), AidKey.ENTER, null);
            assertEquals("USER0001", r.carddemoCommarea().userId());
            assertEquals(77, r.carddemoCommarea().acctId());
            assertTrue(r.carddemoCommarea().isPgmReenter());
            assertTrue(r.programArea().isDetailsNotFetched());
            assertEquals("Please enter Account and Card Number", r.screen().text(INFOMSG));
        }

        @Test
        void commonReturnPacksCommareaThenProgramArea() {
            CocrdupcResponse r = shown();
            assertEquals(r.carddemoCommarea().encode(), r.commarea().substring(0, 160));
            assertEquals(r.programArea().encode(), r.commarea().substring(160, 489));
            assertEquals(" ".repeat(2000 - 489), r.commarea().substring(489));
        }
    }

    @Nested
    class MainPf03Exit {
        @Test
        void pf03WithoutCallerReturnsToMainMenu() {
            CocrdupcResponse r = send(fresh().commarea(), AidKey.PF3, null);
            assertEquals(Kind.XCTL, r.kind());
            assertTrue(r.syncpoint());
            assertEquals("COMEN01C", r.program());
            assertEquals("CM00", r.carddemoCommarea().toTranid());
            assertEquals("CCUP", r.carddemoCommarea().fromTranid());
            assertEquals("COCRDUPC", r.carddemoCommarea().fromProgram());
            assertTrue(r.carddemoCommarea().isUsrtypUser());
            assertTrue(r.carddemoCommarea().isPgmEnter());
            assertEquals("COCRDUP", r.carddemoCommarea().lastMapset());
            assertEquals("CCRDUPA", r.carddemoCommarea().lastMap());
            assertEquals(160, r.commarea().length());
            assertNull(r.screen());
        }

        @Test
        void pf15FoldsOntoPf03() {
            assertEquals(Kind.XCTL, send(fresh().commarea(), AidKey.PF15, null).kind());
        }

        @Test
        void pf03ReturnsToCallingListAndClearsKeys() {
            CardDemoCommarea c = listCommarea();
            CocrdupcResponse shown = send(commareaFrom(c, freshArea()), AidKey.ENTER, null);
            CocrdupcResponse r = send(shown.commarea(), AidKey.PF3, null);
            assertEquals("COCRDLIC", r.program());
            assertEquals("CCLI", r.carddemoCommarea().toTranid());
            assertEquals(0, r.carddemoCommarea().acctId());
            assertEquals(0, r.carddemoCommarea().cardNum());
        }

        @Test
        void doneAfterComingFromListGoesBackToList() {
            CardDemoCommarea c = listCommarea();
            CardUpdateProgramArea p = freshArea();
            p.setChangeAction('C');
            CocrdupcResponse r = send(commareaFrom(c, p), AidKey.ENTER, null);
            assertEquals(Kind.XCTL, r.kind());
            assertEquals("COCRDLIC", r.program());
        }

        @Test
        void failedAfterComingFromListGoesBackToList() {
            CardDemoCommarea c = listCommarea();
            CardUpdateProgramArea p = freshArea();
            p.setChangeAction('L');
            assertEquals(Kind.XCTL, send(commareaFrom(c, p), AidKey.ENTER, null).kind());
        }
    }

    static CardDemoCommarea listCommarea() {
        CardDemoCommarea c = CardDemoCommarea.initialized();
        c.setFromTranid("CCLI");
        c.setFromProgram("COCRDLIC");
        c.setLastMapset("COCRDLI");
        c.setLastMap("CCRDSLA");
        c.setAcctId(50);
        c.setCardNum(500024453765740L);
        c.setPgmEnter();
        return c;
    }

    static CardUpdateProgramArea freshArea() {
        CardUpdateProgramArea p = CardUpdateProgramArea.uninitialized();
        p.initialize();
        return p;
    }

    @Nested
    class MainEntryFromCardList {
        @Test
        void fetchesCardFromCommareaKeysAndShowsDetails() {
            CocrdupcResponse r = send(commareaFrom(listCommarea(), freshArea()), AidKey.ENTER, null);
            assertTrue(r.programArea().isShowDetails());
            assertTrue(r.carddemoCommarea().isPgmReenter());
            assertEquals(ACCT, r.screen().text(ACCTSID));
            assertEquals(CARD, r.screen().text(CARDSID));
            assertEquals("ANIYA VON", r.screen().text(CRDNAME));
            assertEquals("Y", r.screen().text(CRDSTCD));
            assertEquals("03", r.screen().text(EXPMON));
            assertEquals("2023", r.screen().text(EXPYEAR));
            assertEquals("09", r.screen().text(EXPDAY));
            assertEquals("Details of selected card shown above", r.screen().text(INFOMSG));
            assertEquals(CRDNAME, r.screen().cursorField());
            assertEquals(DFHBMPRF, r.screen().field(ACCTSID).attribute());
            assertEquals(DFHBMFSE, r.screen().field(CRDNAME).attribute());
            assertEquals("747", r.programArea().oldDetails().cvvCd());
        }

        @Test
        void cardNotFoundStillSetsShowDetails() {
            CardDemoCommarea c = listCommarea();
            c.setCardNum(1111222233334444L);
            CocrdupcResponse r = send(commareaFrom(c, freshArea()), AidKey.ENTER, null);
            assertTrue(r.programArea().isShowDetails());
            assertEquals("Did not find cards for this search condition", r.screen().text(ERRMSG));
            assertEquals("Details of selected card shown above", r.screen().text(INFOMSG));
            assertEquals(DFHRED, r.screen().field(ACCTSID).color());
        }
    }

    @Nested
    class EditAccount1210 {
        @Test
        void blankAccountPromptsAndShowsRedStar() {
            CocrdupcResponse r = search("", CARD);
            assertEquals("Account number not provided", r.screen().text(ERRMSG));
            assertEquals("*", r.screen().text(ACCTSID));
            assertEquals(DFHRED, r.screen().field(ACCTSID).color());
            assertEquals(ACCTSID, r.screen().cursorField());
            assertTrue(r.programArea().isDetailsNotFetched());
            assertEquals("Please enter Account and Card Number", r.screen().text(INFOMSG));
        }

        @ParameterizedTest
        @CsvSource({"'*'", "'00000000000'"})
        void starOrZeroAccountIsTreatedAsNotSupplied(String acct) {
            assertEquals("Account number not provided", search(acct, CARD).screen().text(ERRMSG));
        }

        @Test
        void accountNotReceivedIsTreatedAsNotSupplied() {
            CocrdupcResponse r = search(null, CARD);
            assertEquals("Account number not provided", r.screen().text(ERRMSG));
        }

        @Test
        void bothKeysBlankReportsNoInput() {
            CocrdupcResponse r = search("", "");
            assertEquals("No input received", r.screen().text(ERRMSG));
            assertEquals("*", r.screen().text(CARDSID));
        }

        @ParameterizedTest
        @CsvSource({"'1234'", "'0000000005A'"})
        void nonNumericAccountIsRejected(String acct) {
            CocrdupcResponse r = search(acct, CARD);
            assertEquals("ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER", r.screen().text(ERRMSG));
            assertEquals(DFHRED, r.screen().field(ACCTSID).color());
            assertEquals(0, r.carddemoCommarea().acctId());
        }

        @Test
        void accountErrorWinsOverCardError() {
            CocrdupcResponse r = search("12", "ABC");
            assertEquals("ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER", r.screen().text(ERRMSG));
            assertEquals(DFHRED, r.screen().field(CARDSID).color());
        }
    }

    @Nested
    class EditCard1220 {
        @Test
        void blankCardPrompts() {
            CocrdupcResponse r = search(ACCT, "");
            assertEquals("Card number not provided", r.screen().text(ERRMSG));
            assertEquals(CARDSID, r.screen().cursorField());
            assertEquals("0000000000000000", r.programArea().newDetails().cardId());
            assertEquals(50, r.carddemoCommarea().acctId());
        }

        @Test
        void nonNumericCardIsRejected() {
            CocrdupcResponse r = search(ACCT, "05000244537657XX");
            assertEquals("CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER", r.screen().text(ERRMSG));
            assertEquals(CARDSID, r.screen().cursorField());
        }
    }

    @Nested
    class GetCard9100 {
        @Test
        void foundCardShowsUppercasedDetails() {
            CocrdupcResponse r = shown();
            assertTrue(r.programArea().isShowDetails());
            assertEquals("ANIYA VON", r.screen().text(CRDNAME));
            assertEquals("Details of selected card shown above", r.screen().text(INFOMSG));
            assertEquals(CRDNAME, r.screen().cursorField());
            assertEquals(ACCT, r.programArea().oldDetails().acctId());
            assertEquals(CARD, r.programArea().oldDetails().cardId());
            assertEquals(50, r.carddemoCommarea().acctId());
            assertEquals(500024453765740L, r.carddemoCommarea().cardNum());
        }

        @Test
        void notFoundMarksBothKeysInError() {
            CocrdupcResponse r = search(ACCT, "1111222233334444");
            assertTrue(r.programArea().isDetailsNotFetched());
            assertEquals("Did not find cards for this search condition", r.screen().text(ERRMSG));
            assertEquals(DFHRED, r.screen().field(ACCTSID).color());
            assertEquals(DFHRED, r.screen().field(CARDSID).color());
            assertEquals(ACCTSID, r.screen().cursorField());
        }

        @Test
        void otherResponseBuildsFileErrorMessage() {
            String commarea = fresh().commarea();
            file.failNextRead(CicsResp.NOTOPEN, 60);
            CocrdupcResponse r = send(commarea, AidKey.ENTER, CcrdupaInput.search(ACCT, CARD));
            assertEquals("File Error: READ     on CARDDAT   returned RESP 000000019 ,RESP2 000000060",
                    r.screen().text(ERRMSG));
            assertEquals(DFHRED, r.screen().field(ACCTSID).color());
            assertTrue(r.programArea().isDetailsNotFetched());
        }
    }

    @Nested
    class EditMapInputs1200 {
        @Test
        void unchangedValuesAreReported() {
            CocrdupcResponse r = send(shown().commarea(), AidKey.ENTER, edit("ANIYA VON", "Y", "03", "2023"));
            assertEquals("No change detected with respect to values fetched.", r.screen().text(ERRMSG));
            assertTrue(r.programArea().isShowDetails());
            assertEquals(CRDNAME, r.screen().cursorField());
        }

        @Test
        void changeDetectionIgnoresCase() {
            CocrdupcResponse r = send(shown().commarea(), AidKey.ENTER, edit("aniya von", "y", "03", "2023"));
            assertEquals("No change detected with respect to values fetched.", r.screen().text(ERRMSG));
        }

        @Test
        void validChangesAskForConfirmation() {
            CocrdupcResponse r = validated();
            assertTrue(r.programArea().isChangesOkNotConfirmed());
            assertEquals("Changes validated.Press F5 to save", r.screen().text(INFOMSG));
            assertEquals("", r.screen().text(ERRMSG));
            assertEquals("Aniya Smith", r.screen().text(CRDNAME));
            assertEquals("N", r.screen().text(CRDSTCD));
            assertEquals("12", r.screen().text(EXPMON));
            assertEquals("2027", r.screen().text(EXPYEAR));
            assertEquals("09", r.screen().text(EXPDAY));
            for (var f : new com.carddemo.programs.cocrdupc.copybook.CcrdupaMap.Field[] {
                    ACCTSID, CARDSID, CRDNAME, CRDSTCD, EXPMON, EXPYEAR}) {
                assertEquals(DFHBMPRF, r.screen().field(f).attribute(), f.name());
            }
            assertEquals(DFHBMBRY, r.screen().field(FKEYSC).attribute());
            assertEquals(ACCTSID, r.screen().cursorField());
        }

        @Test
        void enterOnConfirmationScreenKeepsWaiting() {
            CocrdupcResponse r = send(validated().commarea(), AidKey.ENTER,
                    edit("Aniya Smith", "N", "12", "2027"));
            assertTrue(r.programArea().isChangesOkNotConfirmed());
            assertEquals("Changes validated.Press F5 to save", r.screen().text(INFOMSG));
            assertEquals("Aniya Von", file.get(CARD).orElseThrow().cardEmbossedName().strip());
        }

        @Test
        void firstFailingEditSetsTheMessage() {
            CocrdupcResponse r = send(shown().commarea(), AidKey.ENTER, edit("R2D2", "X", "13", "1900"));
            assertEquals("Card name can only contain alphabets and spaces", r.screen().text(ERRMSG));
            assertTrue(r.programArea().isChangesNotOk());
            assertEquals("Update card details presented above.", r.screen().text(INFOMSG));
            assertEquals(DFHRED, r.screen().field(CRDNAME).color());
            assertEquals(DFHRED, r.screen().field(CRDSTCD).color());
            assertEquals(DFHRED, r.screen().field(EXPMON).color());
            assertEquals(DFHRED, r.screen().field(EXPYEAR).color());
            assertEquals(CRDNAME, r.screen().cursorField());
            assertEquals(DFHBMFSE, r.screen().field(CRDNAME).attribute());
            assertEquals(DFHBMPRF, r.screen().field(ACCTSID).attribute());
        }
    }

    @Nested
    class EditName1230 {
        @ParameterizedTest
        @CsvSource({"''", "'*'", "'00000000000000000000000000000000000000000000000000'"})
        void blankNameIsPrompted(String name) {
            CocrdupcResponse r = send(shown().commarea(), AidKey.ENTER, edit(name, "Y", "03", "2023"));
            assertEquals("Card name not provided", r.screen().text(ERRMSG));
            if (!name.startsWith("0")) {
                assertEquals("*", r.screen().text(CRDNAME));
            }
            assertEquals(DFHRED, r.screen().field(CRDNAME).color());
        }

        @Test
        void fewerThanFiftyZerosIsNotBlankButFailsAlphaCheck() {
            CocrdupcResponse r = send(shown().commarea(), AidKey.ENTER, edit("000", "Y", "03", "2023"));
            assertEquals("Card name can only contain alphabets and spaces", r.screen().text(ERRMSG));
        }

        @Test
        void lettersAndSpacesOnly() {
            CocrdupcResponse r = send(shown().commarea(), AidKey.ENTER, edit("Mary-Jane", "Y", "03", "2023"));
            assertEquals("Card name can only contain alphabets and spaces", r.screen().text(ERRMSG));
        }
    }

    @Nested
    class EditCardStatus1240 {
        @ParameterizedTest
        @CsvSource({"''", "'0'", "'X'", "'y'"})
        void statusMustBeUppercaseYOrN(String status) {
            CocrdupcResponse r = send(shown().commarea(), AidKey.ENTER, edit("ANIYA SMITH", status, "03", "2023"));
            assertEquals("Card Active Status must be Y or N", r.screen().text(ERRMSG));
            assertEquals(CRDSTCD, r.screen().cursorField());
        }
    }

    @Nested
    class EditExpiryMon1250 {
        @ParameterizedTest
        @CsvSource({"''", "'00'", "'13'"})
        void monthOutsideOneToTwelveIsRejected(String mon) {
            CocrdupcResponse r = send(shown().commarea(), AidKey.ENTER, edit("ANIYA SMITH", "Y", mon, "2023"));
            assertEquals("Card expiry month must be between 1 and 12", r.screen().text(ERRMSG));
            assertEquals(EXPMON, r.screen().cursorField());
        }

        @Test
        void singleDigitMonthIsRightJustifiedAndAccepted() {
            CocrdupcResponse r = send(shown().commarea(), AidKey.ENTER, edit("ANIYA SMITH", "Y", "5", "2023"));
            assertTrue(r.programArea().isChangesOkNotConfirmed());
            assertEquals(" 5", r.programArea().newDetails().expMon());
        }
    }

    @Nested
    class EditExpiryYear1260 {
        @ParameterizedTest
        @CsvSource({"''", "'0000'", "'1949'", "'2100'"})
        void yearOutside1950To2099IsRejected(String year) {
            CocrdupcResponse r = send(shown().commarea(), AidKey.ENTER, edit("ANIYA SMITH", "Y", "03", year));
            assertEquals("Invalid card expiry year", r.screen().text(ERRMSG));
            assertEquals(EXPYEAR, r.screen().cursorField());
        }

        @ParameterizedTest
        @CsvSource({"'1950'", "'2099'"})
        void boundaryYearsAreAccepted(String year) {
            CocrdupcResponse r = send(shown().commarea(), AidKey.ENTER, edit("ANIYA SMITH", "Y", "03", year));
            assertTrue(r.programArea().isChangesOkNotConfirmed());
        }
    }

    @Nested
    class WriteProcessing9200 {
        @Test
        void pf5CommitsTheChange() {
            CocrdupcResponse r = send(validated().commarea(), AidKey.PF5, edit("Aniya Smith", "N", "12", "2027"));
            assertTrue(r.programArea().isChangesOkayedAndDone());
            assertEquals("Changes committed to database", r.screen().text(INFOMSG));
            assertEquals("", r.screen().text(ERRMSG));
            CardRecord saved = file.get(CARD).orElseThrow();
            assertEquals("Aniya Smith", saved.cardEmbossedName().strip());
            assertEquals("2027-12-09", saved.cardExpiraionDate());
            assertEquals("N", saved.cardActiveStatus());
            assertEquals(50, saved.cardAcctId());
            // CCUP-NEW-CVV-CD is never populated, so the COBOL rewrites the CVV from spaces.
            assertEquals(0, saved.cardCvvCd());
            assertFalse(file.isLocked());
            assertEquals(DFHBMPRF, r.screen().field(CRDNAME).attribute());
        }

        @Test
        void enterAfterCommitStartsANewSearch() {
            CocrdupcResponse done = send(validated().commarea(), AidKey.PF5, edit("Aniya Smith", "N", "12", "2027"));
            CocrdupcResponse r = send(done.commarea(), AidKey.ENTER, null);
            assertTrue(r.programArea().isDetailsNotFetched());
            assertEquals(0, r.carddemoCommarea().acctId());
            assertEquals(0, r.carddemoCommarea().cardNum());
            assertEquals("Please enter Account and Card Number", r.screen().text(INFOMSG));
            assertEquals("", r.screen().text(ACCTSID));
        }

        @Test
        void lockFailureIsReported() {
            String commarea = validated().commarea();
            file.failNextReadForUpdate(CicsResp.RECORDBUSY, 0);
            CocrdupcResponse r = send(commarea, AidKey.PF5, edit("Aniya Smith", "N", "12", "2027"));
            assertTrue(r.programArea().isChangesOkayedLockError());
            assertEquals("Could not lock record for update", r.screen().text(ERRMSG));
            assertEquals("Changes unsuccessful. Please try again", r.screen().text(INFOMSG));
            assertEquals(DFHBMFSE, r.screen().field(ACCTSID).attribute());
        }

        @Test
        void rewriteFailureIsReported() {
            String commarea = validated().commarea();
            file.failNextRewrite(CicsResp.IOERR, 0);
            CocrdupcResponse r = send(commarea, AidKey.PF5, edit("Aniya Smith", "N", "12", "2027"));
            assertTrue(r.programArea().isChangesOkayedButFailed());
            assertEquals("Update of record failed", r.screen().text(ERRMSG));
            assertEquals("Changes unsuccessful. Please try again", r.screen().text(INFOMSG));
        }

        @Test
        void failedUpdateThenEnterResetsSearch() {
            String commarea = validated().commarea();
            file.failNextRewrite(CicsResp.IOERR, 0);
            CocrdupcResponse failed = send(commarea, AidKey.PF5, edit("Aniya Smith", "N", "12", "2027"));
            CocrdupcResponse r = send(failed.commarea(), AidKey.ENTER, null);
            assertTrue(r.programArea().isDetailsNotFetched());
            assertEquals("", r.screen().text(ERRMSG));
        }
    }

    @Nested
    class CheckChangeInRec9300 {
        @Test
        void concurrentChangeRefreshesOldValuesAndShowsThem() {
            String commarea = validated().commarea();
            file.changeBeforeNextReadForUpdate(
                    new CardRecord(CARD, 50, 747, "Aniya Jones", "2024-01-31", "Y", ""));
            CocrdupcResponse r = send(commarea, AidKey.PF5, edit("Aniya Smith", "N", "12", "2027"));
            assertTrue(r.programArea().isShowDetails());
            assertEquals("Record changed by some one else. Please review", r.screen().text(ERRMSG));
            assertEquals("ANIYA JONES", r.screen().text(CRDNAME));
            assertEquals("01", r.screen().text(EXPMON));
            assertEquals("2024", r.screen().text(EXPYEAR));
            assertEquals("31", r.screen().text(EXPDAY));
            assertEquals("Aniya Jones", file.get(CARD).orElseThrow().cardEmbossedName().strip());
        }
    }

    @Nested
    class PfKeyValidation {
        @Test
        void pf5BeforeValidationIsTreatedAsEnter() {
            CocrdupcResponse r = send(shown().commarea(), AidKey.PF5, edit("Aniya Smith", "N", "12", "2027"));
            assertTrue(r.programArea().isChangesOkNotConfirmed());
            assertEquals("Aniya Von", file.get(CARD).orElseThrow().cardEmbossedName().strip());
        }

        @Test
        void pf12CancelsChangesAndRereadsTheCard() {
            CocrdupcResponse r = send(validated().commarea(), AidKey.PF12, edit("Aniya Smith", "N", "12", "2027"));
            assertTrue(r.programArea().isShowDetails());
            assertEquals("ANIYA VON", r.screen().text(CRDNAME));
            assertEquals("Details of selected card shown above", r.screen().text(INFOMSG));
        }

        @Test
        void pf12BeforeFetchIsTreatedAsEnter() {
            CocrdupcResponse r = send(fresh().commarea(), AidKey.PF12, CcrdupaInput.search(ACCT, CARD));
            assertTrue(r.programArea().isShowDetails());
        }

        @Test
        void unmappedAidIsTreatedAsEnter() {
            CocrdupcResponse r = send(fresh().commarea(), AidKey.PA3, CcrdupaInput.search(ACCT, CARD));
            assertTrue(r.programArea().isShowDetails());
        }

        @Test
        void otherPfKeyIsTreatedAsEnter() {
            CocrdupcResponse r = send(fresh().commarea(), AidKey.PF7, CcrdupaInput.search("", ""));
            assertEquals("No input received", r.screen().text(ERRMSG));
        }
    }

    @Nested
    class AbendRoutine {
        @Test
        void unexpectedChangeActionAbends9999() {
            CocrdupcResponse s = shown();
            CardUpdateProgramArea p = CardUpdateProgramArea.decode(s.commarea().substring(160, 489));
            p.setChangeAction('X');
            String commarea = commareaFrom(s.carddemoCommarea(), p);
            AbendException e = assertThrows(AbendException.class,
                    () -> send(commarea, AidKey.ENTER, edit("ANIYA VON", "Y", "03", "2023")));
            assertEquals("9999", e.abcode());
            assertEquals("0001", e.abendData().abendCode());
            assertEquals("COCRDUPC", e.abendData().abendCulprit());
            assertEquals("UNEXPECTED DATA SCENARIO", e.abendData().abendMsg().strip());
        }

        @Test
        void runtimeFailureGoesThroughHandleAbendWithSpacesMessage() {
            CardFileGateway broken = new CardFileGateway() {
                @Override
                public FileResult read(String cardNum) {
                    throw new IllegalStateException("file gone");
                }

                @Override
                public FileResult readForUpdate(String cardNum) {
                    throw new IllegalStateException();
                }

                @Override
                public FileResult rewrite(CardRecord record) {
                    throw new IllegalStateException();
                }
            };
            CocrdupcService svc = new CocrdupcService(broken, () -> NOW);
            String commarea = svc.process(new CocrdupcRequest(null, AidKey.ENTER, null)).commarea();
            AbendException e = assertThrows(AbendException.class, () -> svc.process(
                    new CocrdupcRequest(commarea, AidKey.ENTER, CcrdupaInput.search(ACCT, CARD))));
            assertEquals("9999", e.abcode());
            assertEquals("COCRDUPC", e.abendData().abendCulprit());
            // ABEND-MSG starts as SPACES (not LOW-VALUES), so the default text is never moved in.
            assertEquals(" ".repeat(72), e.abendData().abendMsg());
            assertTrue(e.getCause() instanceof IllegalStateException);
        }
    }
}
