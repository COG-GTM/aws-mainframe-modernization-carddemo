package com.carddemo.online.cocrdslc;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.catchThrowable;

import com.carddemo.common.copybook.CardDataSeed;
import com.carddemo.common.copybook.CardDemoCommarea;
import com.carddemo.common.copybook.CardRecord;
import com.carddemo.runtime.bms.BmsScreen;
import com.carddemo.runtime.cics.AidKey;
import com.carddemo.runtime.cics.CicsContext;
import com.carddemo.runtime.cics.CicsDispatcher;
import com.carddemo.runtime.cics.CicsOutcome;
import com.carddemo.runtime.cics.CicsResponse;
import com.carddemo.runtime.cics.SystemInfo;
import com.carddemo.runtime.cobol.PicX;
import com.carddemo.runtime.vsam.FileControl;
import com.carddemo.runtime.vsam.InMemoryKsdsStore;
import com.carddemo.runtime.vsam.KsdsFile;
import com.carddemo.runtime.vsam.KsdsStore;
import java.math.BigDecimal;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.EnumSource;

/**
 * Behavioural parity tests for COCRDSLC. Line numbers refer to app/cbl/COCRDSLC.cbl. Card data is the CARDDAT
 * seed app/data/ASCII/carddata.txt; record 1 = card 0500024453765740, CARD-ACCT-ID 00000000050, CVV 747,
 * name 'Aniya Von', expiry 2023-03-09, status 'Y'.
 */
class CocrdslcTest {

    /** CURDATE 03/05/24, CURTIME 14:07:09 (1100-SCREEN-INIT lines 437-449). */
    private static final Clock CLOCK = Clock.fixed(Instant.parse("2024-03-05T14:07:09.25Z"), ZoneOffset.UTC);
    private static final String CARD = "0500024453765740";
    private static final String ACCT = "00000000050";

    private final Cocrdslc program = new Cocrdslc();
    private final KsdsFile carddat = KsdsFile.inMemory("CARDDAT", CardRecord.LENGTH, 0, 16);
    private final FileControl fct = new FileControl().register(carddat);

    CocrdslcTest() {
        carddat.load(CardDataSeed.records());
    }

    private CicsContext ctx(AidKey aid, byte[] commarea, Map<String, String> fields, FileControl files) {
        return CicsContext.builder().transid("CCDL").aid(aid).commarea(commarea).terminalInput(fields)
                .fileControl(files).clock(CLOCK).systemInfo(SystemInfo.defaults()).build();
    }

    private CicsResponse run(AidKey aid, byte[] commarea, Map<String, String> fields) {
        return CicsDispatcher.run(program, ctx(aid, commarea, fields, fct));
    }

    /** Runs a task directly so the symbolic map (colours) can be inspected after RETURN. */
    private Cocrdslc.Task runTask(AidKey aid, byte[] commarea, Map<String, String> fields) {
        Cocrdslc.Task task = new Cocrdslc.Task(ctx(aid, commarea, fields, fct));
        catchThrowable(task::mainPara);
        return task;
    }

    /** Commarea as COCRDLIC passes it on 'S' selection (COCRDLIC sets FROM-*, LAST-MAPSET, PGM-ENTER). */
    private static CardDemoCommarea fromCardList(String acct, String card) {
        CardDemoCommarea c = base();
        c.setFromTranid("CCLI");
        c.setFromProgram("COCRDLIC");
        c.setLastMapset("COCRDLI");
        c.setLastMap("CCRDLIA");
        CardDemoCommarea.ACCT_ID.set(c, new BigDecimal(acct));
        CardDemoCommarea.CARD_NUM.set(c, new BigDecimal(card));
        return c;
    }

    private static CardDemoCommarea fromMenu() {
        CardDemoCommarea c = base();
        c.setFromTranid("CM00");
        c.setFromProgram("COMEN01C");
        c.setLastMapset("COMEN01");
        c.setLastMap("COMEN1A");
        return c;
    }

    private static CardDemoCommarea base() {
        CardDemoCommarea c = new CardDemoCommarea();
        c.initializeSpaces();
        c.setUserId("USER0001");
        c.setUserType("U");
        c.setPgmContext(0);
        CardDemoCommarea.CUST_ID.set(c, 0);
        CardDemoCommarea.ACCT_ID.set(c, 0);
        CardDemoCommarea.CARD_NUM.set(c, 0);
        return c;
    }

    /** The commarea COCRDSLC returned on its own previous screen (PGM-REENTER, 2000 bytes). */
    private static byte[] reenter(CardDemoCommarea c) {
        c.setPgmContext(1);
        byte[] out = new byte[Cocrdslc.WS_COMMAREA_LENGTH];
        java.util.Arrays.fill(out, (byte) ' ');
        System.arraycopy(c.toBytes(), 0, out, 0, CardDemoCommarea.LENGTH);
        return out;
    }

    private CicsResponse submit(String acct, String card) {
        return submit(AidKey.ENTER, fromMenu(), acct, card);
    }

    private CicsResponse submit(AidKey aid, CardDemoCommarea c, String acct, String card) {
        Map<String, String> fields = new LinkedHashMap<>();
        if (acct != null) {
            fields.put("ACCTSID", acct);
        }
        if (card != null) {
            fields.put("CARDSID", card);
        }
        return run(aid, reenter(c), fields);
    }

    private static CardDemoCommarea commarea(CicsResponse r) {
        return CardDemoCommarea.of(r.outcome().commarea());
    }

    private static void assertReturnsCcdl(CicsResponse r) {
        // COMMON-RETURN lines 394-406: RETURN TRANSID(CCDL) COMMAREA(WS-COMMAREA) LENGTH(2000), PGM-REENTER (line 567)
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.RETURN_TRANSID);
        assertThat(r.outcome().nextTransid()).isEqualTo("CCDL");
        assertThat(r.outcome().commarea()).hasSize(2000);
        assertThat(commarea(r).pgmContextRaw()).isEqualTo("1");
    }

    private static void assertHeader(BmsScreen s) {
        // 1100-SCREEN-INIT lines 427-449
        assertThat(s.map()).isEqualTo("CCRDSLA");
        assertThat(s.value("TRNNAME")).isEqualTo("CCDL");
        assertThat(s.value("PGMNAME")).isEqualTo("COCRDSLC");
        assertThat(s.value("TITLE01")).isEqualTo("      AWS Mainframe Modernization       ");
        assertThat(s.value("TITLE02")).isEqualTo("              CardDemo                  ");
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

    private static void assertNoCardDetails(BmsScreen s) {
        assertThat(s.value("CRDNAME")).isEqualTo(PicX.spaces(50));
        assertThat(s.value("EXPMON")).isEqualTo("  ");
        assertThat(s.value("EXPYEAR")).isEqualTo("    ");
        assertThat(s.value("CRDSTCD")).isEqualTo(" ");
    }

    private static void assertCard1Details(BmsScreen s) {
        // 1200 lines 474-485 from carddata.txt record 1; expiry 2023-03-09 split by CARD-EXPIRAION-DATE-X (84-90)
        assertThat(s.value("CRDNAME")).isEqualTo(PicX.of("Aniya Von", 50));
        assertThat(s.value("EXPMON")).isEqualTo("03");
        assertThat(s.value("EXPYEAR")).isEqualTo("2023");
        assertThat(s.value("CRDSTCD")).isEqualTo("Y");
        assertThat(s.value("INFOMSG")).isEqualTo(info(" Displaying requested details"));
    }

    // ------------------------------------------------------------------ 0000-MAIN: first entry

    @Test
    void noCommareaShowsPromptScreen() {
        // lines 268-272: EIBCALEN = 0 -> INITIALIZE commarea (PGM-ENTER); WHEN CDEMO-PGM-ENTER (349-356)
        CicsResponse r = run(AidKey.ENTER, new byte[0], Map.of());
        assertReturnsCcdl(r);
        BmsScreen s = r.lastMap().screen();
        assertHeader(s);
        // 1200 lines 459-460: EIBCALEN = 0 -> WS-PROMPT-FOR-INPUT; ACCTSID/CARDSID not moved (LOW-VALUES)
        assertThat(s.value("INFOMSG")).isEqualTo(info("Please enter Account and Card Number"));
        assertThat(s.value("ERRMSG")).isEqualTo(PicX.spaces(80));
        assertThat(s.value("ACCTSID")).isEqualTo(PicX.spaces(11));
        assertThat(s.value("CARDSID")).isEqualTo(PicX.spaces(16));
        assertNoCardDetails(s);
        // 1300 lines 510-511 unprotected; 515-517 flags INITIALIZEd to ' ' (BLANK) -> cursor ACCTSID
        assertThat(s.field("ACCTSID").orElseThrow().attributeByte()).isEqualTo(Dfhbmsca.DFHBMFSE);
        assertThat(s.cursorField()).isEqualTo("ACCTSID");
        CardDemoCommarea c = commarea(r);
        assertThat(c.fromProgram()).isEqualTo("        ");
        assertThat(CardDemoCommarea.ACCT_ID.raw(c)).isEqualTo("00000000000");
        assertThat(r.outcome().commarea()).endsWith(PicX.spaces(1840).getBytes(PicX.CHARSET));
    }

    @Test
    void entryFromMenuKeepsCallerCommarea() {
        // lines 268-270: the menu clause tests WORKING-STORAGE CARDDEMO-COMMAREA before it is loaded (LOW-VALUES),
        // so it is never true and the menu's commarea is copied as-is (lines 274-278)
        CardDemoCommarea in = fromMenu();
        CicsResponse r = run(AidKey.ENTER, in.toBytes(), Map.of());
        assertReturnsCcdl(r);
        BmsScreen s = r.lastMap().screen();
        // 1200 lines 462-472: CDEMO-ACCT-ID / CARD-NUM = 0 -> LOW-VALUES; 490-491 prompt
        assertThat(s.value("ACCTSID")).isEqualTo(PicX.spaces(11));
        assertThat(s.value("INFOMSG")).isEqualTo(info("Please enter Account and Card Number"));
        assertThat(s.field("CARDSID").orElseThrow().attributeByte()).isEqualTo(Dfhbmsca.DFHBMFSE);
        CardDemoCommarea c = commarea(r);
        assertThat(c.fromProgram()).isEqualTo("COMEN01C");
        assertThat(c.fromTranid()).isEqualTo("CM00");
        assertThat(c.userId()).isEqualTo("USER0001");
    }

    @Test
    void entryFromCardListDisplaysSelectedCardProtected() {
        // WHEN CDEMO-PGM-ENTER AND FROM-PROGRAM = COCRDLIC (lines 339-348) -> 9100 READ CARDDAT by card number
        Cocrdslc.Task task = runTask(AidKey.ENTER, fromCardList(ACCT, CARD).toBytes(), Map.of());
        CicsResponse r = CicsDispatcher.run(program, ctx(AidKey.ENTER, fromCardList(ACCT, CARD).toBytes(), Map.of(), fct));
        assertReturnsCcdl(r);
        BmsScreen s = r.lastMap().screen();
        assertHeader(s);
        assertThat(s.value("ACCTSID")).isEqualTo(ACCT);
        assertThat(s.value("CARDSID")).isEqualTo(CARD);
        assertCard1Details(s);
        assertThat(s.value("ERRMSG")).isEqualTo(PicX.spaces(80));
        // 1300 lines 505-508 / 527-531: LAST-MAPSET COCRDLI + FROM COCRDLIC -> DFHBMPRF, DFHDFCOL
        assertThat(s.field("ACCTSID").orElseThrow().attributeByte()).isEqualTo(Dfhbmsca.DFHBMPRF);
        assertThat(s.field("CARDSID").orElseThrow().attributeByte()).isEqualTo(Dfhbmsca.DFHBMPRF);
        assertThat(task.ccrdsla.color("ACCTSID")).isEqualTo(Dfhbmsca.DFHDFCOL);
        assertThat(task.ccrdsla.color("CARDSID")).isEqualTo(Dfhbmsca.DFHDFCOL);
        // 553-556: info message present -> DFHNEUTR
        assertThat(task.ccrdsla.color("INFOMSG")).isEqualTo(Dfhbmsca.DFHNEUTR);
        // flags still ' ' (2200 not run) -> WHEN FLG-ACCTFILTER-BLANK -> cursor ACCTSID; no '*' (PGM-ENTER)
        assertThat(s.cursorField()).isEqualTo("ACCTSID");
        assertThat(commarea(r).fromProgram()).isEqualTo("COCRDLIC");
        assertThat(task.cardRecord.cardNum()).isEqualTo(CARD);
    }

    @Test
    void entryFromCardListWithUnknownCardIsNotFound() {
        // 9100 lines 755-761: NOTFND -> INPUT-ERROR, both filters NOT-OK, DID-NOT-FIND-ACCTCARD-COMBO
        Cocrdslc.Task task = runTask(AidKey.ENTER, fromCardList(ACCT, "9999999999999999").toBytes(), Map.of());
        CicsResponse r = run(AidKey.ENTER, fromCardList(ACCT, "9999999999999999").toBytes(), Map.of());
        assertReturnsCcdl(r);
        BmsScreen s = r.lastMap().screen();
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("Did not find cards for this search condition"));
        assertThat(s.value("INFOMSG")).isEqualTo(info("Please enter Account and Card Number"));
        assertNoCardDetails(s);
        assertThat(task.ccrdsla.color("ACCTSID")).isEqualTo(Dfhbmsca.DFHRED);
        assertThat(task.ccrdsla.color("CARDSID")).isEqualTo(Dfhbmsca.DFHRED);
        assertThat(s.cursorField()).isEqualTo("ACCTSID");
    }

    @Test
    void progCommareaTailIsPreserved() {
        // lines 276-278 and 398-400: bytes 161-172 (WS-THIS-PROGCOMMAREA) round-trip; 173-2000 SPACES
        CardDemoCommarea c = fromMenu();
        c.setPgmContext(1);
        byte[] in = new byte[172];
        System.arraycopy(c.toBytes(), 0, in, 0, 160);
        System.arraycopy("COCRDLICCCLI".getBytes(PicX.CHARSET), 0, in, 160, 12);
        CicsResponse r = run(AidKey.ENTER, in, Map.of("ACCTSID", ACCT, "CARDSID", CARD));
        byte[] out = r.outcome().commarea();
        assertThat(new String(out, 160, 12, PicX.CHARSET)).isEqualTo("COCRDLICCCLI");
        assertThat(new String(out, 172, 1828, PicX.CHARSET)).isBlank();
    }

    // ------------------------------------------------------------------ 2200/2210/2220 input edits

    @Test
    void validAccountAndCardDisplayDetails() {
        CicsResponse r = submit(ACCT, CARD);
        assertReturnsCcdl(r);
        BmsScreen s = r.lastMap().screen();
        assertCard1Details(s);
        assertThat(s.value("ACCTSID")).isEqualTo(ACCT);
        assertThat(s.value("CARDSID")).isEqualTo(CARD);
        assertThat(s.value("ERRMSG")).isEqualTo(PicX.spaces(80));
        // both filters ISVALID -> WHEN OTHER (line 522-523) -> cursor ACCTSID
        assertThat(s.cursorField()).isEqualTo("ACCTSID");
        // 2210 line 676 / 2220 line 717 move the filters into the commarea
        CardDemoCommarea c = commarea(r);
        assertThat(CardDemoCommarea.ACCT_ID.raw(c)).isEqualTo(ACCT);
        assertThat(CardDemoCommarea.CARD_NUM.raw(c)).isEqualTo(CARD);
    }

    @Test
    void accountIsNotCheckedAgainstTheCard() {
        // 9100 lines 740-750 read CARDDAT by card number only: a mismatching account still shows the card
        BmsScreen s = submit("99999999999", CARD).lastMap().screen();
        assertCard1Details(s);
        assertThat(s.value("ACCTSID")).isEqualTo("99999999999");
    }

    @Test
    void validInputButCardNotOnFile() {
        Cocrdslc.Task task = runTask(AidKey.ENTER, reenter(fromMenu()), Map.of("ACCTSID", ACCT, "CARDSID",
                "1234567890123456"));
        BmsScreen s = submit(ACCT, "1234567890123456").lastMap().screen();
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("Did not find cards for this search condition"));
        assertThat(s.value("ACCTSID")).isEqualTo(ACCT);
        assertThat(s.value("CARDSID")).isEqualTo("1234567890123456");
        assertThat(s.cursorField()).isEqualTo("ACCTSID");
        assertThat(task.ccrdsla.color("ACCTSID")).isEqualTo(Dfhbmsca.DFHRED);
        assertThat(task.ccrdsla.color("CARDSID")).isEqualTo(Dfhbmsca.DFHRED);
        assertNoCardDetails(s);
    }

    @Test
    void bothBlankIsNoInputReceived() {
        // 2210 657 sets 'Account number not provided', 2220 msg already on, then 637-639 NO-SEARCH-CRITERIA
        Cocrdslc.Task task = runTask(AidKey.ENTER, reenter(fromMenu()), Map.of());
        BmsScreen s = submit(null, null).lastMap().screen();
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("No input received"));
        // 1300 lines 541-551: blank + REENTER -> '*' in red
        assertThat(s.value("ACCTSID")).isEqualTo(PicX.of("*", 11));
        assertThat(s.value("CARDSID")).isEqualTo(PicX.of("*", 16));
        assertThat(task.ccrdsla.color("ACCTSID")).isEqualTo(Dfhbmsca.DFHRED);
        assertThat(task.ccrdsla.color("CARDSID")).isEqualTo(Dfhbmsca.DFHRED);
        assertThat(s.cursorField()).isEqualTo("ACCTSID");
        assertThat(s.value("INFOMSG")).isEqualTo(info("Please enter Account and Card Number"));
    }

    @ParameterizedTest
    @CsvSource(value = {"'           '", "'*'", "'00000000000'", "'0'"}, quoteCharacter = '\'')
    void blankStarOrZeroAccountIsNotProvided(String acct) {
        // 2200 615-617 ('*' / SPACES -> LOW-VALUES); 2210 651-660 (LOW-VALUES, SPACES or CC-ACCT-ID-N = ZEROS)
        CicsResponse r = submit(acct, CARD);
        BmsScreen s = r.lastMap().screen();
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("Account number not provided"));
        assertThat(s.value("ACCTSID")).isEqualTo(PicX.of("*", 11));
        assertThat(s.value("CARDSID")).isEqualTo(CARD);
        assertThat(s.cursorField()).isEqualTo("ACCTSID");
        assertThat(CardDemoCommarea.ACCT_ID.raw(commarea(r))).isEqualTo("00000000000");
        assertNoCardDetails(s);
    }

    @Test
    void blankCardIsNotProvided() {
        // 2220 691-701: WS-PROMPT-FOR-CARD; 1300 519-521 cursor CARDSID; 547-551 '*'
        BmsScreen s = submit(ACCT, "").lastMap().screen();
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("Card number not provided"));
        assertThat(s.value("ACCTSID")).isEqualTo(ACCT);
        assertThat(s.value("CARDSID")).isEqualTo(PicX.of("*", 16));
        assertThat(s.cursorField()).isEqualTo("CARDSID");
    }

    @ParameterizedTest
    @CsvSource({"ABC", "1234567890X", "12", "-0000000005"})
    void nonNumericAccount(String acct) {
        // 2210 665-674: NOT NUMERIC -> message, CDEMO-ACCT-ID = 0 -> 1200 moves LOW-VALUES (field shows blank), red
        Cocrdslc.Task task = runTask(AidKey.ENTER, reenter(fromMenu()), Map.of("ACCTSID", acct, "CARDSID", CARD));
        BmsScreen s = submit(acct, CARD).lastMap().screen();
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER"));
        assertThat(s.value("ACCTSID")).isEqualTo(PicX.spaces(11));
        assertThat(task.ccrdsla.color("ACCTSID")).isEqualTo(Dfhbmsca.DFHRED);
        assertThat(s.cursorField()).isEqualTo("ACCTSID");
    }

    @ParameterizedTest
    @CsvSource({"12345", "05000244537657AB", "0500 24453765740"})
    void nonNumericCard(String card) {
        // 2220 706-715
        Cocrdslc.Task task = runTask(AidKey.ENTER, reenter(fromMenu()), Map.of("ACCTSID", ACCT, "CARDSID", card));
        BmsScreen s = submit(ACCT, card).lastMap().screen();
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER"));
        assertThat(s.value("CARDSID")).isEqualTo(PicX.spaces(16));
        assertThat(task.ccrdsla.color("CARDSID")).isEqualTo(Dfhbmsca.DFHRED);
        assertThat(s.cursorField()).isEqualTo("CARDSID");
    }

    @Test
    void firstErrorMessageWins() {
        // WS-RETURN-MSG-OFF guards (668, 709): the account message is kept when both are invalid
        BmsScreen s = submit("ABC", "XYZ").lastMap().screen();
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER"));
        assertThat(s.cursorField()).isEqualTo("ACCTSID");
    }

    @Test
    void reentryFromCardListKeepsFieldsProtected() {
        // LAST-MAPSET / FROM-PROGRAM are not changed by COCRDSLC except on PF3, so 1300 keeps DFHBMPRF
        CicsResponse r = submit(AidKey.ENTER, fromCardList(ACCT, CARD), ACCT, CARD);
        BmsScreen s = r.lastMap().screen();
        assertThat(s.field("ACCTSID").orElseThrow().attributeByte()).isEqualTo(Dfhbmsca.DFHBMPRF);
        assertCard1Details(s);
    }

    // ------------------------------------------------------------------ PF keys

    @Test
    void pf3FromCardListReturnsToCardList() {
        // lines 305-334: TO-TRANID/TO-PROGRAM = caller; FROM = CCDL/COCRDSLC; USRTYP-USER; PGM-ENTER; LAST-MAP*
        CicsResponse r = run(AidKey.PF3, reenter(fromCardList(ACCT, CARD)), Map.of());
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.XCTL);
        assertThat(r.outcome().nextProgram()).isEqualTo("COCRDLIC");
        assertThat(r.outcome().commarea()).hasSize(160);
        CardDemoCommarea c = commarea(r);
        assertThat(c.toProgram()).isEqualTo("COCRDLIC");
        assertThat(c.toTranid()).isEqualTo("CCLI");
        assertThat(c.fromProgram()).isEqualTo("COCRDSLC");
        assertThat(c.fromTranid()).isEqualTo("CCDL");
        assertThat(c.userType()).isEqualTo("U");
        assertThat(c.pgmContextRaw()).isEqualTo("0");
        assertThat(c.lastMapset()).isEqualTo("COCRDSL");
        assertThat(c.lastMap()).isEqualTo("CCRDSLA");
        assertThat(r.terminalOutputs()).isEmpty();
    }

    @Test
    void pf3FromMenuReturnsToMenu() {
        CicsResponse r = run(AidKey.PF3, reenter(fromMenu()), Map.of());
        assertThat(r.outcome().nextProgram()).isEqualTo("COMEN01C");
        assertThat(commarea(r).toTranid()).isEqualTo("CM00");
    }

    @ParameterizedTest
    @EnumSource(value = AidKey.class, names = {"PF3", "PF15"})
    void pf3WithoutCallerDefaultsToMenu(AidKey aid) {
        // EIBCALEN = 0 -> FROM-* SPACES -> LIT-MENUTRANID / LIT-MENUPGM (311, 318); PF15 folds to PFK03 (CSSTRPFY)
        CicsResponse r = run(aid, new byte[0], Map.of());
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.XCTL);
        assertThat(r.outcome().nextProgram()).isEqualTo("COMEN01C");
        CardDemoCommarea c = commarea(r);
        assertThat(c.toTranid()).isEqualTo("CM00");
        assertThat(c.userType()).isEqualTo("U");
    }

    @ParameterizedTest
    @EnumSource(value = AidKey.class, names = {"PF1", "PF5", "PF7", "PF12", "PF13", "CLEAR", "PA1", "PA3"})
    void otherKeysActAsEnter(AidKey aid) {
        // lines 291-299: anything but ENTER/PFK03 -> CCARD-AID-ENTER
        CicsResponse r = submit(aid, fromMenu(), ACCT, CARD);
        assertReturnsCcdl(r);
        assertCard1Details(r.lastMap().screen());
    }

    // ------------------------------------------------------------------ WHEN OTHER / errors / abend

    @Test
    void unexpectedProgramContextSendsPlainText() {
        // lines 373-380: PGM-CONTEXT neither 0 nor 1 -> SEND-PLAIN-TEXT (838-847)
        CardDemoCommarea c = fromMenu();
        CardDemoCommarea.PGM_CONTEXT.set(c, 5);
        CicsResponse r = run(AidKey.ENTER, c.toBytes(), Map.of());
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.RETURN);
        assertThat(r.lastText().text()).isEqualTo(PicX.of("UNEXPECTED DATA SCENARIO", 75));
        assertThat(r.lastText().erase()).isTrue();
        assertThat(r.lastText().freekb()).isTrue();
    }

    @Test
    void fileErrorBuildsFileErrorMessage() {
        // 9100 WHEN OTHER (762-771): CARDDAT not defined -> FILENOTFOUND RESP 12 / RESP2 1 -> WS-FILE-ERROR-MESSAGE
        CicsResponse r = CicsDispatcher.run(program, ctx(AidKey.ENTER, reenter(fromMenu()),
                Map.of("ACCTSID", ACCT, "CARDSID", CARD), new FileControl()));
        BmsScreen s = r.lastMap().screen();
        assertThat(s.value("ERRMSG")).isEqualTo(
                errmsg("File Error: READ     on CARDDAT   returned RESP 000000012 ,RESP2 000000001 "));
        assertThat(s.cursorField()).isEqualTo("ACCTSID");
        assertNoCardDetails(s);
    }

    @Test
    void unexpectedExceptionRunsAbendRoutine() {
        // ABEND-ROUTINE lines 857-877: ABEND-MSG is VALUE SPACES (never LOW-VALUES) so it stays blank
        KsdsStore broken = new FailingStore();
        FileControl files = new FileControl().register(new KsdsFile("CARDDAT", CardRecord.LENGTH, 0, 16, broken));
        CicsResponse r = CicsDispatcher.run(program, ctx(AidKey.ENTER, reenter(fromMenu()),
                Map.of("ACCTSID", ACCT, "CARDSID", CARD), files));
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.ABEND);
        assertThat(r.outcome().abendCode()).isEqualTo("9999");
        assertThat(r.lastText().text()).isEqualTo(PicX.spaces(4) + PicX.of("COCRDSLC", 8) + PicX.spaces(122));
    }

    // ------------------------------------------------------------------ unit: helpers and dead paragraphs

    @Test
    void zonedZeroComparisonUsesDigitNibbles() {
        assertThat(Cocrdslc.zonedEqualsZero("00000000000")).isTrue();
        assertThat(Cocrdslc.zonedEqualsZero("0          ")).isTrue();
        assertThat(Cocrdslc.zonedEqualsZero(PicX.lowValues(11))).isTrue();
        assertThat(Cocrdslc.zonedEqualsZero("00000000001")).isFalse();
        assertThat(Cocrdslc.zonedEqualsZero("ABC        ")).isFalse();
        assertThat(Cocrdslc.zonedDigitValue("1")).isEqualTo(1);
        assertThat(Cocrdslc.zonedDigitValue("5")).isEqualTo(5);
    }

    @Test
    void getcardByacctReadsCardaixPath() {
        // 9150 (779-809) is never PERFORMed; with WS-CARD-RID-ACCT-ID = 0 the path read is NOTFND
        Cocrdslc.Task task = new Cocrdslc.Task(ctx(AidKey.ENTER, new byte[0], Map.of(), fct));
        task.getcardByacct9150();
        assertThat(task.wsReturnMsg).isEqualTo(PicX.of("Did not find this account in cards database", 75));
        assertThat(task.inputError()).isTrue();

        carddat.load(List.of(PicX.of("1111222233334444" + "00000000000" + "123" + "Zero Acct", 150)));
        Cocrdslc.Task found = new Cocrdslc.Task(ctx(AidKey.ENTER, new byte[0], Map.of(), fct));
        found.getcardByacct9150();
        assertThat(found.wsInfoMsg).isEqualTo(PicX.of(" Displaying requested details", 40));
        assertThat(found.cardRecord.embossedName()).isEqualTo(PicX.of("Zero Acct", 50));
    }

    private static final class FailingStore implements KsdsStore {
        private final InMemoryKsdsStore delegate = new InMemoryKsdsStore();

        @Override
        public Optional<byte[]> get(byte[] key) {
            throw new IllegalStateException("simulated storage failure");
        }

        @Override
        public boolean insert(byte[] key, byte[] record) {
            return delegate.insert(key, record);
        }

        @Override
        public boolean update(byte[] key, byte[] record) {
            return delegate.update(key, record);
        }

        @Override
        public boolean delete(byte[] key) {
            return delegate.delete(key);
        }

        @Override
        public Optional<Map.Entry<byte[], byte[]>> ceiling(byte[] key) {
            return delegate.ceiling(key);
        }

        @Override
        public Optional<Map.Entry<byte[], byte[]>> higher(byte[] key) {
            return delegate.higher(key);
        }

        @Override
        public Optional<Map.Entry<byte[], byte[]>> lower(byte[] key) {
            return delegate.lower(key);
        }

        @Override
        public Optional<Map.Entry<byte[], byte[]>> last() {
            return delegate.last();
        }

        @Override
        public List<Map.Entry<byte[], byte[]>> all() {
            return delegate.all();
        }

        @Override
        public long count() {
            return delegate.count();
        }

        @Override
        public void clear() {
            delegate.clear();
        }
    }
}
