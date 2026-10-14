package com.carddemo.online.cocrdslc;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.carddemo.common.copybook.CardDataSeed;
import com.carddemo.common.copybook.CardDemoCommarea;
import com.carddemo.common.copybook.CardRecord;
import com.carddemo.runtime.bms.BmsScreen;
import com.carddemo.runtime.cics.AidKey;
import com.carddemo.runtime.cics.CicsContext;
import com.carddemo.runtime.cics.CicsControlTransfer;
import com.carddemo.runtime.cics.CicsDispatcher;
import com.carddemo.runtime.cics.CicsOutcome;
import com.carddemo.runtime.cics.CicsResponse;
import com.carddemo.runtime.cobol.PicX;
import com.carddemo.runtime.vsam.FileControl;
import com.carddemo.runtime.vsam.FileState;
import com.carddemo.runtime.vsam.KsdsFile;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

/**
 * Behavioural parity tests for COCRDSLC. Line numbers refer to app/cbl/COCRDSLC.cbl; card data is
 * app/data/ASCII/carddata.txt (line 1: card 0500024453765740, account 00000000050, "Aniya Von",
 * expiry 2023-03-09, status Y).
 */
class CocrdslcTest {

    private static final Clock CLOCK = Clock.fixed(Instant.parse("2024-03-05T14:07:09.25Z"), ZoneOffset.UTC);
    private static final String CARD = "0500024453765740";
    private static final String ACCT = "00000000050";

    /** Runs the real program but keeps the task so tests can inspect colour bytes and WORKING-STORAGE. */
    static final class Probe extends Cocrdslc {
        Task task;

        @Override
        public void execute(CicsContext ctx) {
            task = new Task(ctx);
            task.run();
        }
    }

    private final Probe program = new Probe();
    private final KsdsFile carddat = KsdsFile.inMemory(CardDataSeed.FILE_NAME, CardRecord.LENGTH, 0, 16);
    private final FileControl fct = new FileControl().register(carddat);

    CocrdslcTest() {
        carddat.load(CardDataSeed.records());
    }

    private CicsContext ctx(AidKey aid, byte[] commarea, Map<String, String> fields) {
        return CicsContext.builder().transid("CCDL").aid(aid).commarea(commarea).terminalInput(fields)
                .fileControl(fct).clock(CLOCK).build();
    }

    private CicsResponse run(AidKey aid, byte[] commarea, Map<String, String> fields) {
        return CicsDispatcher.run(program, ctx(aid, commarea, fields));
    }

    /** Commarea as COCRDLIC passes it on XCTL after a card is selected (S/V on the list). */
    private static CardDemoCommarea fromCardList(String acct, String card) {
        CardDemoCommarea c = new CardDemoCommarea();
        c.initializeSpaces();
        c.setFromTranid("CCLI");
        c.setFromProgram("COCRDLIC");
        c.setUserId("ADMIN001");
        c.setUserType("A");
        c.setPgmContext(0);
        CardDemoCommarea.CUST_ID.set(c, 0L);
        CardDemoCommarea.ACCT_ID.set(c, Long.parseLong(acct));
        CardDemoCommarea.CARD_NUM.set(c, Long.parseLong(card));
        c.setLastMapset("COCRDLI");
        c.setLastMap("CCRDLIA");
        return c;
    }

    /** Commarea of a COCRDSLC screen already on display (PGM-REENTER), reached from the menu. */
    private static byte[] reenterFromMenu() {
        CardDemoCommarea c = new CardDemoCommarea();
        c.initializeSpaces();
        c.setFromTranid("CM00");
        c.setFromProgram("COMEN01C");
        c.setUserId("USER0001");
        c.setUserType("U");
        c.setPgmContext(1);
        CardDemoCommarea.CUST_ID.set(c, 0L);
        CardDemoCommarea.ACCT_ID.set(c, 0L);
        CardDemoCommarea.CARD_NUM.set(c, 0L);
        c.setLastMapset("COMEN01");
        c.setLastMap("COMEN1A");
        return withProgCommarea(c.toBytes());
    }

    private static byte[] withProgCommarea(byte[] cdemo) {
        byte[] all = new byte[Cocrdslc.WS_COMMAREA_LENGTH];
        Arrays.fill(all, (byte) ' ');
        System.arraycopy(cdemo, 0, all, 0, cdemo.length);
        return all;
    }

    private static Map<String, String> input(String acct, String card) {
        Map<String, String> m = new LinkedHashMap<>();
        if (acct != null) {
            m.put("ACCTSID", acct);
        }
        if (card != null) {
            m.put("CARDSID", card);
        }
        return m;
    }

    private static CardDemoCommarea outCommarea(CicsResponse r) {
        return CardDemoCommarea.of(Arrays.copyOf(r.outcome().commarea(), CardDemoCommarea.LENGTH));
    }

    private static void assertContinues(CicsResponse r) {
        // COMMON-RETURN lines 394-406: RETURN TRANSID('CCDL') COMMAREA(WS-COMMAREA) LENGTH(2000)
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.RETURN_TRANSID);
        assertThat(r.outcome().nextTransid()).isEqualTo("CCDL");
        assertThat(r.outcome().commarea()).hasSize(2000);
        assertThat(outCommarea(r).isPgmReenter()).isTrue(); // 1400-SEND-SCREEN line 567
    }

    private static void assertHeader(BmsScreen s) {
        // 1100-SCREEN-INIT lines 428-449
        assertThat(s.map()).isEqualTo("CCRDSLA");
        assertThat(s.mapset()).isEqualTo("COCRDSL");
        assertThat(s.value("TRNNAME")).isEqualTo("CCDL");
        assertThat(s.value("PGMNAME")).isEqualTo("COCRDSLC");
        assertThat(s.value("TITLE01")).isEqualTo("      AWS Mainframe Modernization       ");
        assertThat(s.value("CURDATE")).isEqualTo("03/05/24");
        assertThat(s.value("CURTIME")).isEqualTo("14:07:09");
        assertThat(s.erase()).isTrue();
    }

    private static String errmsg(String text) {
        return PicX.of(text, 80);
    }

    private static String info(String text) {
        return PicX.of(text, 40);
    }

    private void assertNoDetails(BmsScreen s) {
        for (String f : List.of("CRDNAME", "CRDSTCD", "EXPMON", "EXPYEAR")) {
            assertThat(s.field(f).orElseThrow().sent()).as(f).isFalse();
        }
    }

    private void assertDetailsOfFirstCard(BmsScreen s) {
        // 1200-SETUP-SCREEN-VARS lines 474-485 from carddata.txt line 1
        assertThat(s.value("CRDNAME")).isEqualTo(PicX.of("Aniya Von", 50));
        assertThat(s.value("EXPMON")).isEqualTo("03");
        assertThat(s.value("EXPYEAR")).isEqualTo("2023");
        assertThat(s.value("CRDSTCD")).isEqualTo("Y");
        assertThat(s.value("INFOMSG")).isEqualTo(info("   Displaying requested details"));
        assertThat(s.value("ERRMSG")).isEqualTo(PicX.spaces(80));
    }

    // ------------------------------------------------------------------ 0000-MAIN: first entry

    @Test
    void firstEntryWithoutCommareaPromptsForInput() {
        // lines 268-272 (EIBCALEN=0 -> INITIALIZE), 349-356 (PGM-ENTER, other context), 459-460 prompt
        CicsResponse r = run(AidKey.ENTER, null, null);
        BmsScreen s = r.lastMap().screen();
        assertHeader(s);
        assertThat(s.value("INFOMSG")).isEqualTo(info("Please enter Account and Card Number"));
        assertThat(s.value("ERRMSG")).isEqualTo(PicX.spaces(80));
        assertThat(s.field("ACCTSID").orElseThrow().sent()).isFalse();
        assertThat(s.field("CARDSID").orElseThrow().sent()).isFalse();
        assertNoDetails(s);
        // 1300 lines 509-523: not from the list -> FSET unprotected; flags still SPACES (BLANK) -> cursor ACCTSID
        assertThat(s.field("ACCTSID").orElseThrow().attributeByte()).isEqualTo(Cocrdslc.DFHBMFSE);
        assertThat(s.cursorField()).isEqualTo("ACCTSID");
        // PGM-ENTER at 1300 time: no '*' (lines 541-551); INFOMSG present -> DFHNEUTR (line 556)
        assertThat(program.task.colors).containsExactly(Map.entry("INFOMSG", Cocrdslc.DFHNEUTR));
        assertContinues(r);
        CardDemoCommarea out = outCommarea(r);
        assertThat(out.fromProgram()).isEqualTo(PicX.spaces(8));
        assertThat(CardDemoCommarea.ACCT_ID.raw(out)).isEqualTo("00000000000");
        assertThat(CardDemoCommarea.CARD_NUM.raw(out)).isEqualTo("0000000000000000");
        // INITIALIZE WS-THIS-PROGCOMMAREA + WS-COMMAREA -> bytes 161-2000 are SPACES
        assertThat(PicX.isSpaces(PicX.fromBytes(Arrays.copyOfRange(r.outcome().commarea(), 160, 2000)))).isTrue();
        assertThat(s.render().get(3)).contains("View Credit Card Detail");
        assertThat(s.render().get(23)).contains("ENTER=Search Cards  F3=Exit");
    }

    @Test
    void pf3WithoutCommareaReturnsToMenu() {
        // lines 305-334: FROM-TRANID/PROGRAM are SPACES after INITIALIZE -> CM00 / COMEN01C
        CicsResponse r = run(AidKey.PF3, null, null);
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.XCTL);
        assertThat(r.outcome().nextProgram()).isEqualTo("COMEN01C");
        assertThat(r.terminalOutputs()).isEmpty();
        CardDemoCommarea c = CardDemoCommarea.of(r.outcome().commarea());
        assertThat(r.outcome().commarea()).hasSize(160);
        assertThat(c.toTranid()).isEqualTo("CM00");
        assertThat(c.toProgram()).isEqualTo("COMEN01C");
        assertThat(c.fromTranid()).isEqualTo("CCDL");
        assertThat(c.fromProgram()).isEqualTo("COCRDSLC");
        assertThat(c.userType()).isEqualTo("U");
        assertThat(c.isPgmEnter()).isTrue();
        assertThat(c.lastMapset()).isEqualTo("COCRDSL");
        assertThat(c.lastMap()).isEqualTo("CCRDSLA");
    }

    // ------------------------------------------------------------------ coming from the card list

    @Test
    void fromCardListShowsCardDetailsProtected() {
        // lines 339-348: PGM-ENTER + FROM-PROGRAM COCRDLIC -> 9000-READ-DATA with CDEMO-CARD-NUM
        CicsResponse r = run(AidKey.ENTER, fromCardList(ACCT, CARD).toBytes(), null);
        BmsScreen s = r.lastMap().screen();
        assertHeader(s);
        assertThat(s.value("ACCTSID")).isEqualTo(ACCT);
        assertThat(s.value("CARDSID")).isEqualTo(CARD);
        assertDetailsOfFirstCard(s);
        // lines 505-508 / 527-531: LAST-MAPSET COCRDLI + FROM COCRDLIC -> protected, default colour
        assertThat(s.field("ACCTSID").orElseThrow().attributeByte()).isEqualTo(Cocrdslc.DFHBMPRF);
        assertThat(s.field("CARDSID").orElseThrow().attributeByte()).isEqualTo(Cocrdslc.DFHBMPRF);
        assertThat(program.task.colors).containsEntry("ACCTSID", Cocrdslc.DFHDFCOL)
                .containsEntry("CARDSID", Cocrdslc.DFHDFCOL);
        assertThat(s.cursorField()).isEqualTo("ACCTSID"); // flags never edited (BLANK), line 517
        assertContinues(r);
        CardDemoCommarea out = outCommarea(r);
        assertThat(out.fromProgram()).isEqualTo("COCRDLIC"); // not overwritten: PF3 goes back to the list
        assertThat(out.lastMapset()).isEqualTo("COCRDLI");
        assertThat(out.userType()).isEqualTo("A");
        // incoming commarea is only 160 bytes: WS-THIS-PROGCOMMAREA padded (DFHCOMMAREA beyond EIBCALEN)
        assertThat(PicX.fromBytes(Arrays.copyOfRange(r.outcome().commarea(), 160, 172))).isEqualTo(PicX.spaces(12));
    }

    @Test
    void fromCardListWithUnknownCardShowsNotFound() {
        // 9100 lines 755-761: NOTFND -> both filters NOT-OK, DID-NOT-FIND-ACCTCARD-COMBO; 386-391 resend
        CicsResponse r = run(AidKey.ENTER, fromCardList(ACCT, "1111111111111111").toBytes(), null);
        BmsScreen s = r.lastMap().screen();
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("Did not find cards for this search condition"));
        assertThat(s.value("INFOMSG")).isEqualTo(info("Please enter Account and Card Number"));
        assertThat(s.value("CARDSID")).isEqualTo("1111111111111111");
        assertNoDetails(s);
        assertThat(program.task.colors).containsEntry("ACCTSID", Cocrdslc.DFHRED).containsEntry("CARDSID", Cocrdslc.DFHRED);
        assertThat(s.cursorField()).isEqualTo("ACCTSID");
        assertThat(program.task.wsRespCd).isEqualTo(0); // overwritten by SEND MAP RESP (line 575)
        assertContinues(r);
    }

    @Test
    void pf3AfterCardListReturnsToCardList() {
        // lines 309-321: FROM-TRANID CCLI / FROM-PROGRAM COCRDLIC are the XCTL target
        CardDemoCommarea c = fromCardList(ACCT, CARD);
        c.setPgmContext(1);
        CicsResponse r = run(AidKey.PF3, withProgCommarea(c.toBytes()), null);
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.XCTL);
        assertThat(r.outcome().nextProgram()).isEqualTo("COCRDLIC");
        CardDemoCommarea out = CardDemoCommarea.of(r.outcome().commarea());
        assertThat(out.toTranid()).isEqualTo("CCLI");
        assertThat(out.fromProgram()).isEqualTo("COCRDSLC");
        assertThat(out.lastMapset()).isEqualTo("COCRDSL");
        // line 326 bug-for-bug: an admin comes back as a regular user
        assertThat(out.userType()).isEqualTo("U");
        assertThat(CardDemoCommarea.CARD_NUM.raw(out)).isEqualTo(CARD);
    }

    @Test
    void pf15FoldsToPf3() {
        // CSSTRPFY: PF15 -> CCARD-AID-PFK03
        CicsResponse r = run(AidKey.PF15, reenterFromMenu(), null);
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.XCTL);
        assertThat(r.outcome().nextProgram()).isEqualTo("COMEN01C");
    }

    // ------------------------------------------------------------------ re-entry: 2000-PROCESS-INPUTS

    @Test
    void reenterWithValidInputShowsCard() {
        CicsResponse r = run(AidKey.ENTER, reenterFromMenu(), input(ACCT, CARD));
        BmsScreen s = r.lastMap().screen();
        assertThat(s.value("ACCTSID")).isEqualTo(ACCT);
        assertThat(s.value("CARDSID")).isEqualTo(CARD);
        assertDetailsOfFirstCard(s);
        assertThat(s.field("ACCTSID").orElseThrow().attributeByte()).isEqualTo(Cocrdslc.DFHBMFSE);
        assertThat(program.task.colors).containsOnlyKeys("INFOMSG");
        assertThat(s.cursorField()).isEqualTo("ACCTSID"); // both ISVALID -> WHEN OTHER (line 523)
        assertContinues(r);
        CardDemoCommarea out = outCommarea(r);
        // lines 676 / 717: validated filters stored in the commarea
        assertThat(CardDemoCommarea.ACCT_ID.raw(out)).isEqualTo(ACCT);
        assertThat(CardDemoCommarea.CARD_NUM.raw(out)).isEqualTo(CARD);
        assertThat(out.userId()).isEqualTo("USER0001");
    }

    @Test
    void accountIsNotCheckedAgainstTheCard() {
        // 9100 reads CARDDAT by card number only (line 739 commented out): a wrong account still shows the card
        CicsResponse r = run(AidKey.ENTER, reenterFromMenu(), input("00000000099", CARD));
        BmsScreen s = r.lastMap().screen();
        assertThat(s.value("ACCTSID")).isEqualTo("00000000099");
        assertDetailsOfFirstCard(s);
    }

    @ParameterizedTest
    @EnumSource(value = AidKey.class, names = {"PF1", "PF5", "PF12", "CLEAR", "PA1", "PA3", "PF24"})
    void otherKeysActLikeEnter(AidKey aid) {
        // lines 291-299: anything but ENTER/PF3 is forced to ENTER
        CicsResponse r = run(aid, reenterFromMenu(), input(ACCT, CARD));
        assertDetailsOfFirstCard(r.lastMap().screen());
        assertContinues(r);
    }

    @Test
    void reenterWithNoInputSaysNoInputReceived() {
        // RECEIVE MAP -> MAPFAIL (map stays LOW-VALUES); 2210/2220 both BLANK; line 639 overrides the message
        CicsResponse r = run(AidKey.ENTER, reenterFromMenu(), null);
        BmsScreen s = r.lastMap().screen();
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("No input received"));
        // lines 541-551: blank filters on re-entry are shown as '*' in red
        assertThat(s.value("ACCTSID")).isEqualTo(PicX.of("*", 11));
        assertThat(s.value("CARDSID")).isEqualTo(PicX.of("*", 16));
        assertThat(program.task.colors).containsEntry("ACCTSID", Cocrdslc.DFHRED).containsEntry("CARDSID", Cocrdslc.DFHRED);
        assertThat(s.cursorField()).isEqualTo("ACCTSID");
        assertThat(program.task.wsInputFlag).isEqualTo("1");
        assertNoDetails(s);
        assertContinues(r);
    }

    @Test
    void starsAndSpacesCountAsBlank() {
        // lines 615-627: '*' or SPACES -> LOW-VALUES
        CicsResponse r = run(AidKey.ENTER, reenterFromMenu(), input("*", "     "));
        assertThat(r.lastMap().screen().value("ERRMSG")).isEqualTo(errmsg("No input received"));
    }

    @Test
    void blankAccountWithValidCard() {
        // 2210 lines 651-660: Account number not provided; no READ (lines 360-363)
        CicsResponse r = run(AidKey.ENTER, reenterFromMenu(), input(null, CARD));
        BmsScreen s = r.lastMap().screen();
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("Account number not provided"));
        assertThat(s.value("ACCTSID")).isEqualTo(PicX.of("*", 11));
        assertThat(s.value("CARDSID")).isEqualTo(CARD);
        assertThat(s.cursorField()).isEqualTo("ACCTSID");
        assertThat(program.task.colors).containsEntry("ACCTSID", Cocrdslc.DFHRED).doesNotContainKey("CARDSID");
        assertNoDetails(s);
    }

    @Test
    void validAccountWithBlankCard() {
        // 2220 lines 691-701: Card number not provided; cursor on CARDSID (lines 519-521)
        CicsResponse r = run(AidKey.ENTER, reenterFromMenu(), input(ACCT, "*"));
        BmsScreen s = r.lastMap().screen();
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("Card number not provided"));
        assertThat(s.value("ACCTSID")).isEqualTo(ACCT);
        assertThat(s.value("CARDSID")).isEqualTo(PicX.of("*", 16));
        assertThat(s.cursorField()).isEqualTo("CARDSID");
    }

    @Test
    void zeroAccountIsTreatedAsNotProvided() {
        // line 653: CC-ACCT-ID-N EQUAL ZEROS -> BLANK
        CicsResponse r = run(AidKey.ENTER, reenterFromMenu(), input("00000000000", CARD));
        assertThat(r.lastMap().screen().value("ERRMSG")).isEqualTo(errmsg("Account number not provided"));
        assertThat(r.lastMap().screen().value("ACCTSID")).isEqualTo(PicX.of("*", 11));
    }

    @Test
    void nonNumericAccount() {
        // 2210 lines 665-674: message, CDEMO-ACCT-ID = 0 so 1200 (line 462) blanks the field: typed value not echoed
        CicsResponse r = run(AidKey.ENTER, reenterFromMenu(), input("ABC", CARD));
        BmsScreen s = r.lastMap().screen();
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER"));
        assertThat(s.field("ACCTSID").orElseThrow().sent()).isFalse();
        assertThat(s.value("CARDSID")).isEqualTo(CARD);
        assertThat(program.task.colors).containsEntry("ACCTSID", Cocrdslc.DFHRED);
        assertThat(s.cursorField()).isEqualTo("ACCTSID");
        assertThat(CardDemoCommarea.ACCT_ID.raw(outCommarea(r))).isEqualTo("00000000000");
    }

    @Test
    void shortCardNumberIsNotNumeric() {
        // "12345" is space padded to 16 -> NOT NUMERIC (lines 706-715)
        CicsResponse r = run(AidKey.ENTER, reenterFromMenu(), input(ACCT, "12345"));
        BmsScreen s = r.lastMap().screen();
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER"));
        assertThat(s.field("CARDSID").orElseThrow().sent()).isFalse();
        assertThat(program.task.colors).containsEntry("CARDSID", Cocrdslc.DFHRED).doesNotContainKey("ACCTSID");
        assertThat(s.cursorField()).isEqualTo("CARDSID");
    }

    @Test
    void firstErrorMessageWins() {
        // WS-RETURN-MSG-OFF guards (lines 668, 709): the account message is kept
        CicsResponse r = run(AidKey.ENTER, reenterFromMenu(), input("12A", "XYZ"));
        assertThat(r.lastMap().screen().value("ERRMSG"))
                .isEqualTo(errmsg("ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER"));
        assertThat(program.task.colors).containsEntry("ACCTSID", Cocrdslc.DFHRED).containsEntry("CARDSID", Cocrdslc.DFHRED);
    }

    @Test
    void reenterWithUnknownCard() {
        CicsResponse r = run(AidKey.ENTER, reenterFromMenu(), input(ACCT, "9999999999999999"));
        BmsScreen s = r.lastMap().screen();
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("Did not find cards for this search condition"));
        assertThat(s.value("ACCTSID")).isEqualTo(ACCT);
        assertThat(s.value("CARDSID")).isEqualTo("9999999999999999");
        assertNoDetails(s);
    }

    @Test
    void fileErrorShowsRespCodes() {
        // 9100 WHEN OTHER lines 762-771: DISABLED = RESP 84, RESP2 50; message truncated to WS-RETURN-MSG X(75)
        carddat.setState(FileState.DISABLED);
        CicsResponse r = run(AidKey.ENTER, reenterFromMenu(), input(ACCT, CARD));
        String expected = "File Error: READ     on CARDDAT   returned RESP 000000084 ,RESP2 000000050 ";
        assertThat(expected).hasSize(75);
        assertThat(r.lastMap().screen().value("ERRMSG")).isEqualTo(errmsg(expected));
        // WS-RETURN-MSG was SPACES when the error hit -> account flagged NOT-OK (lines 764-766)
        assertThat(program.task.colors).containsEntry("ACCTSID", Cocrdslc.DFHRED).doesNotContainKey("CARDSID");
        assertThat(r.lastMap().screen().cursorField()).isEqualTo("ACCTSID");
    }

    @Test
    void unexpectedContextSendsPlainText() {
        // line 373-380: neither PF3, PGM-ENTER nor PGM-REENTER -> SEND TEXT + RETURN (no TRANSID)
        CardDemoCommarea c = CardDemoCommarea.of(reenterFromMenu());
        CardDemoCommarea.PGM_CONTEXT.set(c, 5L);
        CicsResponse r = run(AidKey.ENTER, withProgCommarea(c.toBytes()), null);
        assertThat(r.lastMap()).isNull();
        assertThat(r.lastText().text()).isEqualTo(PicX.of("UNEXPECTED DATA SCENARIO", 75));
        assertThat(r.lastText().erase()).isTrue();
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.RETURN);
        assertThat(program.task.abendData.code()).isEqualTo("0001");
        assertThat(program.task.abendData.culprit()).isEqualTo("COCRDSLC");
    }

    @Test
    void menuCommareaIsNotReinitialisedBugForBug() {
        // lines 268-270 test WORKING-STORAGE before the copy, so a commarea from COMEN01C is copied as is
        CardDemoCommarea c = CardDemoCommarea.of(reenterFromMenu());
        c.setPgmContext(0);
        CardDemoCommarea.ACCT_ID.set(c, 50L);
        CardDemoCommarea.CARD_NUM.set(c, Long.parseLong(CARD));
        CicsResponse r = run(AidKey.ENTER, c.toBytes(), null);
        BmsScreen s = r.lastMap().screen();
        // PGM-ENTER from another context: show the screen; CDEMO-ACCT-ID <> 0 so CC-ACCT-ID (SPACES) is moved
        assertThat(s.value("ACCTSID")).isEqualTo(PicX.spaces(11));
        assertThat(s.field("ACCTSID").orElseThrow().sent()).isTrue();
        assertThat(s.value("INFOMSG")).isEqualTo(info("Please enter Account and Card Number"));
        CardDemoCommarea out = outCommarea(r);
        assertThat(out.userId()).isEqualTo("USER0001");
        assertThat(out.fromProgram()).isEqualTo("COMEN01C");
        assertThat(CardDemoCommarea.ACCT_ID.raw(out)).isEqualTo(ACCT);
    }

    @Test
    void lowValueCommareaFieldsCompareAsZero() {
        // COSGN00C/COMEN01C leave unset commarea fields LOW-VALUES; X'00' digits compare = 0 (lines 462, 468)
        byte[] raw = new byte[CardDemoCommarea.LENGTH];
        CardDemoCommarea c = CardDemoCommarea.of(raw);
        c.setFromProgram("COMEN01C");
        c.setPgmContext(0);
        CicsResponse r = run(AidKey.ENTER, c.toBytes(), null);
        assertThat(r.lastMap().screen().field("ACCTSID").orElseThrow().sent()).isFalse();
        assertThat(r.lastMap().screen().field("CARDSID").orElseThrow().sent()).isFalse();
        assertContinues(r);
    }

    @Test
    void programCommareaRoundTrips() {
        // lines 276-278 / 398-400: WS-THIS-PROGCOMMAREA (bytes 161-172) is carried through unchanged
        byte[] in = reenterFromMenu();
        byte[] tail = "COCRDLICCCLI".getBytes(PicX.CHARSET);
        System.arraycopy(tail, 0, in, 160, 12);
        in[500] = 'Z';
        CicsResponse r = run(AidKey.ENTER, in, input(ACCT, CARD));
        byte[] out = r.outcome().commarea();
        assertThat(PicX.fromBytes(Arrays.copyOfRange(out, 160, 172))).isEqualTo("COCRDLICCCLI");
        assertThat(out[500]).isEqualTo((byte) ' '); // rest of WS-COMMAREA re-INITIALIZEd
    }

    // ------------------------------------------------------------------ paragraph-level

    private Cocrdslc.Task task(CicsContext ctx) {
        return new Cocrdslc.Task(ctx);
    }

    @Test
    void getcardByacct9150ReadsTheAlternateIndex() {
        // 9150 (never PERFORMed): WS-CARD-RID-ACCT-ID is ZEROS after INITIALIZE -> NOTFND
        Cocrdslc.Task t = task(ctx(AidKey.ENTER, reenterFromMenu(), null));
        t.getcardByacct9150();
        assertThat(t.wsReturnMsg).isEqualTo(PicX.of("Did not find this account in cards database", 75));
        assertThat(t.wsEditAcctFlag).isEqualTo("0");
        assertThat(t.wsInputFlag).isEqualTo("1");

        Cocrdslc.Task found = task(ctx(AidKey.ENTER, reenterFromMenu(), null));
        found.wsCardRidAcctId = ACCT;
        found.getcardByacct9150();
        assertThat(found.wsInfoMsg).isEqualTo(info("   Displaying requested details"));
        assertThat(found.cardRecord.cardNum()).isEqualTo(CARD);

        // non-unique AIX with two cards for the account -> DUPKEY (15) falls into WHEN OTHER
        carddat.write(PicX.of("0000000000000001" + ACCT + "123Second Card", 150));
        Cocrdslc.Task dup = task(ctx(AidKey.ENTER, reenterFromMenu(), null));
        dup.wsCardRidAcctId = ACCT;
        dup.getcardByacct9150();
        assertThat(dup.wsReturnMsg)
                .isEqualTo("File Error: READ     on CARDAIX   returned RESP 000000015 ,RESP2 000000000 ");
        assertThat(dup.cardRecord.cardNum()).isEqualTo("0000000000000001");
    }

    @Test
    void abendRoutineSendsAbendDataAndAbends9999() {
        // lines 857-877: ABEND-MSG is SPACES (VALUE SPACES), not LOW-VALUES, so the default text is not used
        CicsContext c = ctx(AidKey.ENTER, null, null);
        Cocrdslc.Task t = task(c);
        assertThatThrownBy(t::abendRoutine).isInstanceOfSatisfying(CicsControlTransfer.class,
                e -> assertThat(e.outcome().abendCode()).isEqualTo("9999"));
        String sent = ((com.carddemo.runtime.cics.TerminalOutput.TextOutput) c.terminalOutputs().getFirst()).text();
        assertThat(sent).hasSize(134);
        assertThat(sent.substring(4, 12)).isEqualTo("COCRDSLC");
        assertThat(sent.substring(62)).isBlank();
    }

    @Test
    void helpers() {
        assertThat(Cocrdslc.displayValue(PicX.spaces(11))).isZero();
        assertThat(Cocrdslc.displayValue(PicX.lowValues(11))).isZero();
        assertThat(Cocrdslc.displayValue("00000000050")).isEqualTo(50L);
        assertThat(Cocrdslc.displayValue("?")).isNull(); // EBCDIC X'6F': not a digit nibble
        assertThat(Cocrdslc.Task.binaryToAlnum(84)).isEqualTo("000000084 ");
        assertThat(Cocrdslc.isNumericText("12345678901")).isTrue();
        assertThat(Cocrdslc.isNumericText("1234567890 ")).isFalse();
        assertThat(task(ctx(AidKey.ENTER, null, null)).wsFileErrorMessage()).hasSize(80);
    }
}
