package com.carddemo.online.cocrdlic;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.common.copybook.CardDataSeed;
import com.carddemo.common.copybook.CardDemoCommarea;
import com.carddemo.common.copybook.CardRecord;
import com.carddemo.runtime.bms.BmsScreen;
import com.carddemo.runtime.cics.AidKey;
import com.carddemo.runtime.cics.CicsContext;
import com.carddemo.runtime.cics.CicsDispatcher;
import com.carddemo.runtime.cics.CicsOutcome;
import com.carddemo.runtime.cics.CicsProgram;
import com.carddemo.runtime.cics.CicsResponse;
import com.carddemo.runtime.cics.SystemInfo;
import com.carddemo.runtime.cobol.PicX;
import com.carddemo.runtime.vsam.FileControl;
import com.carddemo.runtime.vsam.FileState;
import com.carddemo.runtime.vsam.KsdsFile;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.junit.jupiter.api.Test;

/**
 * Behavioural parity tests for COCRDLIC. Line numbers refer to app/cbl/COCRDLIC.cbl. Card data is the CARDDAT
 * seed app/data/ASCII/carddata.txt (50 cards, one per account, all CARD-ACTIVE-STATUS 'Y'); in CARD-NUM
 * (key) order page n shows sorted records 7n-6 .. 7n, page 8 only record 50 (see {@link #PAGE1} etc.).
 */
class CocrdlicTest {

    /** 2024-03-05 14:07:09 UTC: CURDATE 03/05/24, CURTIME 14:07:09 (1100-SCREEN-INIT lines 652-664). */
    private static final Clock CLOCK = Clock.fixed(Instant.parse("2024-03-05T14:07:09.25Z"), ZoneOffset.UTC);

    // carddata.txt sorted by CARD-NUM: {CARD-NUM, CARD-ACCT-ID}
    private static final String[][] PAGE1 = {
        {"0500024453765740", "00000000050"}, {"0683586198171516", "00000000027"}, {"0923877193247330", "00000000002"},
        {"0927987108636232", "00000000020"}, {"0982496213629795", "00000000012"}, {"1014086565224350", "00000000044"},
        {"1142167692878931", "00000000037"}};
    private static final String[][] PAGE2 = {
        {"1561409106491600", "00000000035"}, {"2745303720002090", "00000000039"}, {"2760836797107565", "00000000024"},
        {"2871968252812490", "00000000006"}, {"2940139362300449", "00000000022"}, {"2988091353094312", "00000000004"},
        {"3260763612337560", "00000000010"}};
    private static final String PAGE3_FIRST = "3766281984155154";
    private static final String LAST_CARD = "9805583408996588";

    private final KsdsFile carddat = KsdsFile.inMemory("CARDDAT", CardRecord.LENGTH, 0, 16);
    private FileControl fct = new FileControl().register(carddat);
    private Cocrdlic.Task task;

    CocrdlicTest() {
        carddat.load(CardDataSeed.records());
    }

    // ------------------------------------------------------------------ harness

    private CicsResponse run(AidKey aid, byte[] commarea, Map<String, String> fields) {
        CicsProgram probe = new CicsProgram() {
            @Override
            public String programName() {
                return Cocrdlic.PROGRAM;
            }

            @Override
            public Optional<String> transactionId() {
                return Optional.of(Cocrdlic.TRANSID);
            }

            @Override
            public void execute(CicsContext ctx) {
                task = new Cocrdlic.Task(ctx);
                task.main();
            }
        };
        return CicsDispatcher.run(probe, CicsContext.builder().transid("CCLI").aid(aid).commarea(commarea)
                .terminalInput(fields).fileControl(fct).clock(CLOCK).systemInfo(SystemInfo.defaults()).build());
    }

    private CicsResponse first() {
        return run(AidKey.ENTER, null, null);
    }

    /** Next pseudo-conversational task: RETURN TRANSID(CCLI) commarea + what the terminal sends back. */
    private CicsResponse next(CicsResponse prev, AidKey aid, Map<String, String> fields) {
        assertThat(prev.outcome().nextTransid()).isEqualTo("CCLI");
        return run(aid, prev.outcome().commarea(), fields == null ? Map.of() : fields);
    }

    private CicsResponse next(CicsResponse prev, AidKey aid) {
        return next(prev, aid, null);
    }

    private CicsResponse page(int n) {
        CicsResponse r = first();
        for (int p = 1; p < n; p++) {
            r = next(r, AidKey.PF8);
        }
        return r;
    }

    private static Map<String, String> fields(String... kv) {
        Map<String, String> m = new LinkedHashMap<>();
        for (int i = 0; i < kv.length; i += 2) {
            m.put(kv[i], kv[i + 1]);
        }
        return m;
    }

    private static BmsScreen screen(CicsResponse r) {
        return r.lastMap().screen();
    }

    private static CardDemoCommarea commarea(CicsResponse r) {
        return CardDemoCommarea.of(Arrays.copyOf(r.outcome().commarea(), CardDemoCommarea.LENGTH));
    }

    private static ThisProgCommarea prog(CicsResponse r) {
        ThisProgCommarea p = new ThisProgCommarea();
        p.setBytes(Arrays.copyOfRange(r.outcome().commarea(), 160, 160 + ThisProgCommarea.LENGTH));
        return p;
    }

    /** CRDNUMn values of the rows that were sent (rows left LOW-VALUES are not sent, line 680). */
    private static List<String> rows(BmsScreen s) {
        List<String> out = new ArrayList<>();
        for (int n = 1; n <= 7; n++) {
            BmsScreen.Field f = s.field("CRDNUM" + n).orElseThrow();
            out.add(f.sent() ? f.value() : null);
        }
        return out;
    }

    private static List<String> cardNums(String[][] page) {
        return Arrays.stream(page).map(c -> c[0]).toList();
    }

    private static String errmsg(String text) {
        return PicX.of(text, 78);
    }

    private static final String INFO = PicX.of(Cocrdlic.MSG_INFORM_REC_ACTIONS, 45);

    private static void assertHeader(BmsScreen s) {
        assertThat(s.map()).isEqualTo("CCRDLIA");
        assertThat(s.mapset()).isEqualTo("COCRDLI");
        assertThat(s.value("TRNNAME")).isEqualTo("CCLI");
        assertThat(s.value("PGMNAME")).isEqualTo("COCRDLIC");
        assertThat(s.value("TITLE01")).isEqualTo("      AWS Mainframe Modernization       ");
        assertThat(s.value("TITLE02")).isEqualTo("              CardDemo                  ");
        assertThat(s.value("CURDATE")).isEqualTo("03/05/24");
        assertThat(s.value("CURTIME")).isEqualTo("14:07:09");
        assertThat(s.erase()).isTrue();
    }

    private static void assertReturnTransid(CicsResponse r) {
        // COMMON-RETURN lines 604-619: RETURN TRANSID(CCLI) COMMAREA(WS-COMMAREA) LENGTH(2000)
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.RETURN_TRANSID);
        assertThat(r.outcome().nextTransid()).isEqualTo("CCLI");
        assertThat(r.outcome().commarea()).hasSize(2000);
        CardDemoCommarea c = commarea(r);
        assertThat(c.fromTranid()).isEqualTo("CCLI");
        assertThat(c.fromProgram()).isEqualTo("COCRDLIC");
        assertThat(c.lastMap()).isEqualTo("CCRDLIA");
        assertThat(c.lastMapset()).isEqualTo("COCRDLI");
        // bytes 415-2000 of WS-COMMAREA are the SPACES of INITIALIZE WS-COMMAREA (line 302)
        String tail = PicX.fromBytes(Arrays.copyOfRange(r.outcome().commarea(), 414, 2000));
        assertThat(PicX.isSpaces(tail)).isTrue();
    }

    private static byte[] commareaFrom(String fromProgram, int pgmContext, String userType, long acctId) {
        CardDemoCommarea c = new CardDemoCommarea();
        c.initializeSpaces();
        c.setFromTranid("XXXX");
        c.setFromProgram(fromProgram);
        c.setUserId("ADMIN001");
        c.setUserType(userType);
        c.setPgmContext(pgmContext);
        CardDemoCommarea.CUST_ID.set(c, 0);
        CardDemoCommarea.ACCT_ID.set(c, acctId);
        CardDemoCommarea.CARD_NUM.set(c, 0);
        return c.toBytes();
    }

    // ------------------------------------------------------------------ first entry / navigation in

    @Test
    void firstEntryListsFirstSevenCards() {
        // lines 315-325 (EIBCALEN = 0), EVALUATE WHEN OTHER (572-582) -> 9000-READ-FORWARD from SPACES
        CicsResponse r = first();
        BmsScreen s = screen(r);
        assertHeader(s);
        assertThat(rows(s)).containsExactlyElementsOf(cardNums(PAGE1));
        for (int n = 1; n <= 7; n++) {
            assertThat(s.value("ACCTNO" + n)).isEqualTo(PAGE1[n - 1][1]);
            assertThat(s.value("CRDSTS" + n)).isEqualTo("Y");
            // 1200 moves WS-EDIT-SELECT (SPACES after INITIALIZE) and 1250 sets DFHBMFSE on rows with data
            assertThat(s.value("CRDSEL" + n)).isEqualTo(" ");
            assertThat(s.field("CRDSEL" + n).orElseThrow().attributeByte()).isEqualTo(Dfhbmsca.DFHBMFSE);
        }
        assertThat(s.value("PAGENO")).isEqualTo("1  ");
        assertThat(s.value("ERRMSG")).isEqualTo(PicX.spaces(78));
        // 1400 WHEN WS-NO-INFO-MESSAGE -> WS-INFORM-REC-ACTIONS, shown in DFHNEUTR (lines 917-930)
        assertThat(s.value("INFOMSG")).isEqualTo(INFO);
        assertThat(task.ccrdlia.color("INFOMSG")).isEqualTo(Dfhbmsca.DFHNEUTR);
        // 1300 skipped for EIBCALEN = 0, INPUT-OK -> cursor on ACCTSID (line 885)
        assertThat(s.field("ACCTSID").orElseThrow().sent()).isFalse();
        assertThat(s.cursorField()).isEqualTo("ACCTSID");
        assertThat(s.render().get(10)).contains("00000000050", "0500024453765740");
        assertThat(s.render().get(23)).contains("F3=Exit F7=Backward  F8=Forward");

        assertReturnTransid(r);
        CardDemoCommarea c = commarea(r);
        assertThat(c.userType()).isEqualTo("U");
        assertThat(c.isPgmEnter()).isTrue();
        assertThat(c.userId()).isEqualTo(PicX.spaces(8));
        assertThat(CardDemoCommarea.ACCT_ID.raw(c)).isEqualTo("00000000000");
        ThisProgCommarea p = prog(r);
        assertThat(ThisProgCommarea.FIRST_CARD_NUM.get(p)).isEqualTo("0500024453765740");
        assertThat(ThisProgCommarea.FIRST_CARD_ACCT_ID.raw(p)).isEqualTo("00000000050");
        // read-ahead after the 7th row (lines 1191-1214) stores the 8th record
        assertThat(ThisProgCommarea.LAST_CARD_NUM.get(p)).isEqualTo(PAGE2[0][0]);
        assertThat(ThisProgCommarea.LAST_CARD_ACCT_ID.raw(p)).isEqualTo(PAGE2[0][1]);
        assertThat(p.screenNumRaw()).isEqualTo("1");
        assertThat(ThisProgCommarea.LAST_PAGE_DISPLAYED.raw(p)).isEqualTo("9");
        assertThat(ThisProgCommarea.NEXT_PAGE_IND.get(p)).isEqualTo("Y");
        assertThat(ThisProgCommarea.RETURN_FLAG.get(p)).isEqualTo(" ");
        assertThat(ThisProgCommarea.ROW_CARD_NUM[6].get(p)).isEqualTo(PAGE1[6][0]);
    }

    @Test
    void entryFromMenuStartsAfreshAndIgnoresStaleProgramArea() {
        // lines 336-343: CDEMO-PGM-ENTER and FROM-PROGRAM <> COCRDLIC -> INITIALIZE WS-THIS-PROGCOMMAREA
        byte[] in = Arrays.copyOf(commareaFrom("COMEN01C", 0, "U", 27), 414);
        Arrays.fill(in, 160, 414, (byte) '7');
        CicsResponse r = run(AidKey.ENTER, in, null);
        BmsScreen s = screen(r);
        assertThat(rows(s)).containsExactlyElementsOf(cardNums(PAGE1));
        assertThat(s.value("PAGENO")).isEqualTo("1  ");
        // 1300: PGM-ENTER from COMEN01C -> filters not shown even though CDEMO-ACCT-ID = 27
        assertThat(s.field("ACCTSID").orElseThrow().sent()).isFalse();
        assertThat(commarea(r).userId()).isEqualTo("ADMIN001");
        assertReturnTransid(r);
    }

    @Test
    void entryFromAnotherProgramShowsCommareaAccountButDoesNotFilter() {
        // 1300 WHEN OTHER (lines 851-853): flag blank (no RECEIVE) and CDEMO-ACCT-ID <> 0 -> shown, not applied
        CicsResponse r = run(AidKey.ENTER, commareaFrom("COADM01C", 0, "A", 27), null);
        BmsScreen s = screen(r);
        assertThat(rows(s)).containsExactlyElementsOf(cardNums(PAGE1));
        assertThat(s.value("ACCTSID")).isEqualTo("00000000027");
        assertThat(s.field("ACCTSID").orElseThrow().attributeByte()).isEqualTo(Dfhbmsca.DFHBMFSE);
        assertThat(s.field("CARDSID").orElseThrow().sent()).isFalse();
        assertThat(commarea(r).userType()).isEqualTo("A");
    }

    @Test
    void reentryFromAnotherProgramReinitialisesTheWholeCommarea() {
        // lines 459-482: CDEMO-PGM-REENTER from COCRDSLC -> INITIALIZE CARDDEMO-COMMAREA (user id lost)
        CicsResponse r = run(AidKey.ENTER, commareaFrom("COCRDSLC", 1, "A", 27), null);
        assertThat(rows(screen(r))).containsExactlyElementsOf(cardNums(PAGE1));
        CardDemoCommarea c = commarea(r);
        assertThat(c.userId()).isEqualTo(PicX.spaces(8));
        assertThat(c.userType()).isEqualTo("U");
        assertThat(c.isPgmEnter()).isTrue();
        assertThat(CardDemoCommarea.ACCT_ID.raw(c)).isEqualTo("00000000000");
        assertThat(screen(r).field("ACCTSID").orElseThrow().sent()).isFalse();
    }

    // ------------------------------------------------------------------ paging

    @Test
    void pf8PagesForward() {
        // lines 486-497: PF8 and CA-NEXT-PAGE-EXISTS -> READ-FORWARD from WS-CA-LAST-CARD-NUM, page + 1
        CicsResponse r = next(first(), AidKey.PF8);
        BmsScreen s = screen(r);
        assertThat(rows(s)).containsExactlyElementsOf(cardNums(PAGE2));
        assertThat(s.value("PAGENO")).isEqualTo("2  ");
        assertThat(s.value("ERRMSG")).isEqualTo(PicX.spaces(78));
        assertThat(s.value("INFOMSG")).isEqualTo(INFO);
        assertThat(ThisProgCommarea.LAST_CARD_NUM.get(prog(r))).isEqualTo(PAGE3_FIRST);
        assertReturnTransid(r);
    }

    @Test
    void pf20FoldsToPf8() {
        // CSSTRPFY: DFHPF20 -> CCARD-AID-PFK08
        assertThat(rows(screen(next(first(), AidKey.PF20)))).containsExactlyElementsOf(cardNums(PAGE2));
    }

    @Test
    void pf8ToLastPageThenPf8AgainAtTheBoundary() {
        CicsResponse p7 = page(7);
        assertThat(screen(p7).value("PAGENO")).isEqualTo("7  ");
        assertThat(ThisProgCommarea.NEXT_PAGE_IND.get(prog(p7))).isEqualTo("Y");

        // page 8 has one record: ENDFILE on the 2nd READNEXT (lines 1233-1240)
        CicsResponse p8 = next(p7, AidKey.PF8);
        BmsScreen s = screen(p8);
        assertThat(rows(s)).containsExactly(LAST_CARD, null, null, null, null, null, null);
        assertThat(s.value("PAGENO")).isEqualTo("8  ");
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("NO MORE RECORDS TO SHOW"));
        // 1400 WHEN PF8 AND CA-NEXT-PAGE-NOT-EXISTS (910-916): info shown, CA-LAST-PAGE-SHOWN set
        assertThat(s.value("INFOMSG")).isEqualTo(INFO);
        // rows 2-7 are LOW-VALUES: protected (DFHBMPRO), row 1 has data (DFHBMFSE)
        assertThat(s.field("CRDSEL1").orElseThrow().attributeByte()).isEqualTo(Dfhbmsca.DFHBMFSE);
        assertThat(s.field("CRDSEL2").orElseThrow().attributeByte()).isEqualTo(Dfhbmsca.DFHBMPRO);
        ThisProgCommarea p = prog(p8);
        assertThat(ThisProgCommarea.LAST_PAGE_DISPLAYED.raw(p)).isEqualTo("0");
        assertThat(ThisProgCommarea.NEXT_PAGE_IND.get(p)).isEqualTo("\0");
        assertThat(ThisProgCommarea.LAST_CARD_NUM.get(p)).isEqualTo(LAST_CARD);

        // PF8 again: WHEN OTHER re-reads the same page; 1400 lines 905-909 -> NO MORE PAGES, no info message
        CicsResponse again = next(p8, AidKey.PF8);
        BmsScreen s2 = screen(again);
        assertThat(rows(s2)).containsExactly(LAST_CARD, null, null, null, null, null, null);
        assertThat(s2.value("PAGENO")).isEqualTo("8  ");
        assertThat(s2.value("ERRMSG")).isEqualTo(errmsg("NO MORE PAGES TO DISPLAY"));
        assertThat(s2.value("INFOMSG")).isEqualTo(PicX.spaces(45));
        assertThat(task.ccrdlia.color("INFOMSG")).isEqualTo(Dfhbmsca.DFHBMDAR);

        // ENTER resets CA-LAST-PAGE-SHOWN (line 413): back to the "NO MORE RECORDS" message
        CicsResponse enter = next(again, AidKey.ENTER);
        assertThat(screen(enter).value("ERRMSG")).isEqualTo(errmsg("NO MORE RECORDS TO SHOW"));
        assertThat(ThisProgCommarea.LAST_PAGE_DISPLAYED.raw(prog(enter))).isEqualTo("9");
    }

    @Test
    void pf7OnFirstPageSaysNoPreviousPages() {
        // lines 439-454 re-read page 1; 1400 lines 901-904
        CicsResponse r = next(first(), AidKey.PF7);
        BmsScreen s = screen(r);
        assertThat(rows(s)).containsExactlyElementsOf(cardNums(PAGE1));
        assertThat(s.value("PAGENO")).isEqualTo("1  ");
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("NO PREVIOUS PAGES TO DISPLAY"));
        assertThat(s.value("INFOMSG")).isEqualTo(PicX.spaces(45));
    }

    @Test
    void pf7FromPage3ShowsPage2() {
        // lines 501-513 + 9100-READ-BACKWARDS: STARTBR at page 3's first card, READPREV x 8
        CicsResponse r = next(page(3), AidKey.PF7);
        BmsScreen s = screen(r);
        assertThat(rows(s)).containsExactlyElementsOf(cardNums(PAGE2));
        assertThat(s.value("PAGENO")).isEqualTo("2  ");
        assertThat(s.value("ERRMSG")).isEqualTo(PicX.spaces(78));
        assertThat(s.value("INFOMSG")).isEqualTo(INFO);
        ThisProgCommarea p = prog(r);
        assertThat(ThisProgCommarea.FIRST_CARD_NUM.get(p)).isEqualTo(PAGE2[0][0]);
        // line 1268: WS-CA-FIRST-CARDKEY (page 3's) copied to WS-CA-LAST-CARDKEY before the browse
        assertThat(ThisProgCommarea.LAST_CARD_NUM.get(p)).isEqualTo(PAGE3_FIRST);
        assertThat(ThisProgCommarea.NEXT_PAGE_IND.get(p)).isEqualTo("Y");

        // PF8 from here goes to page 3 again
        assertThat(screen(next(r, AidKey.PF8)).value("CRDNUM1")).isEqualTo(PAGE3_FIRST);
    }

    @Test
    void pf7FromPage2ReachesPage1ButStillSaysNoPreviousPages() {
        // bug-for-bug: 1400 tests CA-FIRST-PAGE after the SUBTRACT (line 508), so landing on page 1 shows the message
        CicsResponse r = next(page(2), AidKey.PF19);
        BmsScreen s = screen(r);
        assertThat(rows(s)).containsExactlyElementsOf(cardNums(PAGE1));
        assertThat(s.value("PAGENO")).isEqualTo("1  ");
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("NO PREVIOUS PAGES TO DISPLAY"));
    }

    @Test
    void screenNumberIsOneDigitAndWrapsAfterPage9() {
        // WS-CA-SCREEN-NUM PIC 9(1): ADD 1 to 9 truncates to 0 (line 492), READ-FORWARD bumps 0 to 1 (1177-1178)
        KsdsFile big = KsdsFile.inMemory("CARDDAT", CardRecord.LENGTH, 0, 16);
        List<String> records = new ArrayList<>();
        for (int n = 1; n <= 70; n++) {
            records.add(String.format("%016d%011d", n, n) + "123" + PicX.spaces(60) + "Y");
        }
        big.load(records);
        fct = new FileControl().register(big);
        CicsResponse r = page(9);
        assertThat(screen(r).value("PAGENO")).isEqualTo("9  ");
        assertThat(screen(r).value("CRDNUM1")).isEqualTo(String.format("%016d", 57));
        CicsResponse p10 = next(r, AidKey.PF8);
        assertThat(screen(p10).value("CRDNUM1")).isEqualTo(String.format("%016d", 64));
        assertThat(screen(p10).value("PAGENO")).isEqualTo("1  ");
        assertThat(prog(p10).screenNumRaw()).isEqualTo("1");
    }

    @Test
    void pf7WithScreenNumberZeroStoresAbsoluteValue() {
        // SUBTRACT 1 FROM an unsigned 9(1) holding 0 stores |-1| = 1 (line 508)
        CicsResponse p2 = page(2);
        byte[] ca = p2.outcome().commarea();
        ca[160 + ThisProgCommarea.SCREEN_NUM.offset()] = '0';
        CicsResponse r = run(AidKey.PF7, ca, Map.of());
        assertThat(screen(r).value("PAGENO")).isEqualTo("1  ");
        assertThat(rows(screen(r))).containsExactlyElementsOf(cardNums(PAGE1));
        assertThat(screen(r).value("ERRMSG")).isEqualTo(errmsg("NO PREVIOUS PAGES TO DISPLAY"));
    }

    @Test
    void invalidKeyIsTreatedAsEnterAndRedisplaysThePage() {
        // lines 370-380: PFK-INVALID -> CCARD-AID-ENTER; WHEN OTHER re-reads from WS-CA-FIRST-CARD-NUM
        CicsResponse r = next(page(2), AidKey.PF5);
        assertThat(rows(screen(r))).containsExactlyElementsOf(cardNums(PAGE2));
        assertThat(screen(r).value("ERRMSG")).isEqualTo(PicX.spaces(78));
        assertThat(screen(r).value("PAGENO")).isEqualTo("2  ");
    }

    // ------------------------------------------------------------------ PF3

    @Test
    void pf3ReturnsToMainMenu() {
        // lines 384-405: XCTL COMEN01C COMMAREA(CARDDEMO-COMMAREA)
        CicsResponse r = next(page(2), AidKey.PF3);
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.XCTL);
        assertThat(r.outcome().nextProgram()).isEqualTo("COMEN01C");
        assertThat(r.outcome().commarea()).hasSize(160);
        assertThat(r.terminalOutputs()).isEmpty();
        CardDemoCommarea c = commarea(r);
        assertThat(c.toProgram()).isEqualTo("COMEN01C");
        assertThat(c.fromProgram()).isEqualTo("COCRDLIC");
        assertThat(c.fromTranid()).isEqualTo("CCLI");
        assertThat(c.userType()).isEqualTo("U");
        assertThat(c.isPgmEnter()).isTrue();
        assertThat(c.lastMap()).isEqualTo("CCRDLIA");
        assertThat(c.lastMapset()).isEqualTo("COCRDLI");
    }

    @Test
    void pf3OnFirstEntryAndPf15AlsoExit() {
        // EIBCALEN = 0 sets CDEMO-FROM-PROGRAM = COCRDLIC (line 319), so PF3 exits at once; PF15 folds to PF3
        assertThat(run(AidKey.PF3, null, null).outcome().nextProgram()).isEqualTo("COMEN01C");
        assertThat(next(first(), AidKey.PF15).outcome().nextProgram()).isEqualTo("COMEN01C");
    }

    // ------------------------------------------------------------------ filters

    @Test
    void accountFilterShowsOnlyThatAccountsCard() {
        CicsResponse r = next(first(), AidKey.ENTER, fields("ACCTSID", "00000000027"));
        BmsScreen s = screen(r);
        assertThat(rows(s)).containsExactly("0683586198171516", null, null, null, null, null, null);
        assertThat(s.value("ACCTNO1")).isEqualTo("00000000027");
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("NO MORE RECORDS TO SHOW"));
        assertThat(s.value("INFOMSG")).isEqualTo(INFO);
        // 1300 WHEN FLG-ACCTFILTER-ISVALID (845-848): filter echoed, DFHBMFSE; INPUT-OK -> cursor on ACCTSID
        assertThat(s.value("ACCTSID")).isEqualTo("00000000027");
        assertThat(s.field("ACCTSID").orElseThrow().attributeByte()).isEqualTo(Dfhbmsca.DFHBMFSE);
        assertThat(s.field("CARDSID").orElseThrow().sent()).isFalse();
        assertThat(s.cursorField()).isEqualTo("ACCTSID");
        CardDemoCommarea c = commarea(r);
        assertThat(CardDemoCommarea.ACCT_ID.raw(c)).isEqualTo("00000000027");
        ThisProgCommarea p = prog(r);
        assertThat(ThisProgCommarea.FIRST_CARD_NUM.get(p)).isEqualTo("0683586198171516");
        // ENDFILE leaves CARD-RECORD = last record read (line 1236-1237)
        assertThat(ThisProgCommarea.LAST_CARD_NUM.get(p)).isEqualTo(LAST_CARD);
        assertThat(ThisProgCommarea.NEXT_PAGE_IND.get(p)).isEqualTo("\0");
    }

    @Test
    void cardFilterShowsOnlyThatCard() {
        CicsResponse r = next(first(), AidKey.ENTER, fields("CARDSID", "2760836797107565"));
        BmsScreen s = screen(r);
        assertThat(rows(s)).containsExactly("2760836797107565", null, null, null, null, null, null);
        assertThat(s.value("CARDSID")).isEqualTo("2760836797107565");
        assertThat(CardDemoCommarea.CARD_NUM.raw(commarea(r))).isEqualTo("2760836797107565");
    }

    @Test
    void filterWithNoMatchSaysNoRecordsFound() {
        // lines 1241-1245: page 1 and nothing selected -> WS-NO-RECORDS-FOUND, no info message (926-930)
        CicsResponse r = next(first(), AidKey.ENTER, fields("ACCTSID", "99999999999"));
        BmsScreen s = screen(r);
        assertThat(rows(s)).containsOnlyNulls();
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("NO RECORDS FOUND FOR THIS SEARCH CONDITION."));
        assertThat(s.value("INFOMSG")).isEqualTo(PicX.spaces(45));
        assertThat(task.ccrdlia.color("INFOMSG")).isEqualTo(Dfhbmsca.DFHBMDAR);
        // empty rows protected: row 1 DFHBMPRF (line 753), rows 2-7 DFHBMPRO
        assertThat(s.field("CRDSEL1").orElseThrow().attributeByte()).isEqualTo(Dfhbmsca.DFHBMPRF);
        for (int n = 2; n <= 7; n++) {
            assertThat(s.field("CRDSEL" + n).orElseThrow().attributeByte()).isEqualTo(Dfhbmsca.DFHBMPRO);
        }
    }

    @Test
    void bothFiltersMustMatch() {
        CicsResponse r = next(first(), AidKey.ENTER, fields("ACCTSID", "00000000050", "CARDSID", "0683586198171516"));
        assertThat(screen(r).value("ERRMSG")).isEqualTo(errmsg("NO RECORDS FOUND FOR THIS SEARCH CONDITION."));
        CicsResponse ok = next(first(), AidKey.ENTER, fields("ACCTSID", "00000000050", "CARDSID", "0500024453765740"));
        assertThat(rows(screen(ok))).containsExactly("0500024453765740", null, null, null, null, null, null);
    }

    @Test
    void filterOnLaterPageBrowsesOnlyFromThatPage() {
        // WHEN OTHER browses from WS-CA-FIRST-CARD-NUM (page 3), so account 50 (record 1) is not found, and since
        // WS-CA-SCREEN-NUM <> 1 the message is NO MORE RECORDS rather than NO RECORDS FOUND
        CicsResponse r = next(page(3), AidKey.ENTER, fields("ACCTSID", "00000000050"));
        BmsScreen s = screen(r);
        assertThat(rows(s)).containsOnlyNulls();
        assertThat(s.value("PAGENO")).isEqualTo("3  ");
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("NO MORE RECORDS TO SHOW"));
        assertThat(s.value("INFOMSG")).isEqualTo(INFO);
    }

    @Test
    void zeroOrBlankFilterIsNoFilter() {
        // 2210 lines 1007-1012: SPACES, LOW-VALUES or CC-ACCT-ID-N = ZEROS -> blank, CDEMO-ACCT-ID = 0
        CicsResponse r = next(first(), AidKey.ENTER, fields("ACCTSID", "00000000000", "CARDSID", ""));
        BmsScreen s = screen(r);
        assertThat(rows(s)).containsExactlyElementsOf(cardNums(PAGE1));
        assertThat(s.field("ACCTSID").orElseThrow().sent()).isFalse();
        assertThat(s.field("CARDSID").orElseThrow().sent()).isFalse();
    }

    @Test
    void nonNumericAccountFilterIsRejectedAndRowsAreProtected() {
        // 2210 lines 1017-1025; EVALUATE WHEN INPUT-ERROR with FLG-ACCTFILTER-NOT-OK -> no READ, rows from commarea
        CicsResponse p2 = page(2);
        CicsResponse r = next(p2, AidKey.ENTER, fields("ACCTSID", "12345"));
        BmsScreen s = screen(r);
        assertThat(rows(s)).containsExactlyElementsOf(cardNums(PAGE2));
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER"));
        assertThat(s.value("ACCTSID")).isEqualTo("12345      ");
        assertThat(s.cursorField()).isEqualTo("ACCTSID");
        assertThat(task.ccrdlia.color("ACCTSID")).isEqualTo(Dfhbmsca.DFHRED);
        // FLG-PROTECT-SELECT-ROWS-YES (line 1020) -> every selection field protected
        assertThat(s.field("CRDSEL1").orElseThrow().attributeByte()).isEqualTo(Dfhbmsca.DFHBMPRF);
        assertThat(s.field("CRDSEL5").orElseThrow().attributeByte()).isEqualTo(Dfhbmsca.DFHBMPRO);
        // 1400 WHEN FLG-ACCTFILTER-NOT-OK: CONTINUE -> no info message
        assertThat(s.value("INFOMSG")).isEqualTo(PicX.spaces(45));
        assertThat(s.value("PAGENO")).isEqualTo("2  ");
        assertThat(CardDemoCommarea.ACCT_ID.raw(commarea(r))).isEqualTo("00000000000");
        assertThat(ThisProgCommarea.SCREEN_DATA.get(prog(r))).isEqualTo(ThisProgCommarea.SCREEN_DATA.get(prog(p2)));
        assertReturnTransid(r);
    }

    @Test
    void nonNumericCardFilterIsRejected() {
        CicsResponse r = next(first(), AidKey.ENTER, fields("CARDSID", "4111-1111"));
        BmsScreen s = screen(r);
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER"));
        assertThat(s.cursorField()).isEqualTo("CARDSID");
        assertThat(task.ccrdlia.color("CARDSID")).isEqualTo(Dfhbmsca.DFHRED);
        assertThat(s.value("CARDSID")).isEqualTo(PicX.of("4111-1111", 16));
    }

    @Test
    void bothFiltersInvalidKeepsTheAccountMessage() {
        // 2220 line 1056: card message only if WS-ERROR-MSG-OFF; cursor on the first L = -1 field (ACCTSID)
        CicsResponse r = next(first(), AidKey.ENTER, fields("ACCTSID", "ABC", "CARDSID", "XYZ"));
        BmsScreen s = screen(r);
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER"));
        assertThat(task.ccrdlia.color("ACCTSID")).isEqualTo(Dfhbmsca.DFHRED);
        assertThat(task.ccrdlia.color("CARDSID")).isEqualTo(Dfhbmsca.DFHRED);
        assertThat(s.cursorField()).isEqualTo("ACCTSID");
    }

    @Test
    void readPreviousHittingStartOfFileReportsFileError() {
        // On page 3 type account 2 (record 3) and press PF7: 9100 browses back from page 3's first card, places
        // record 3 in row 7 and then READPREV returns ENDFILE (RESP 20, RESP2 90) -> WHEN OTHER (1361-1369)
        CicsResponse r = next(page(3), AidKey.PF7, fields("ACCTSID", "00000000002"));
        BmsScreen s = screen(r);
        assertThat(rows(s)).containsExactly(null, null, null, null, null, null, "0923877193247330");
        assertThat(s.value("ERRMSG"))
                .isEqualTo(errmsg("File Error: READ     on CARDDAT   returned RESP 000000020 ,RESP2 000000090 "));
        assertThat(s.value("PAGENO")).isEqualTo("2  ");
        ThisProgCommarea p = prog(r);
        assertThat(ThisProgCommarea.FIRST_CARD_NUM.get(p)).isEqualTo(PAGE3_FIRST);
        assertThat(ThisProgCommarea.LAST_CARD_NUM.get(p)).isEqualTo(PAGE3_FIRST);
    }

    // ------------------------------------------------------------------ row selection

    @Test
    void selectSViewsTheCard() {
        // lines 517-541: XCTL COCRDSLC with the row's account and card
        CicsResponse r = next(run(AidKey.ENTER, commareaFrom("COMEN01C", 0, "A", 0), null), AidKey.ENTER,
                fields("CRDSEL3", "S"));
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.XCTL);
        assertThat(r.outcome().nextProgram()).isEqualTo("COCRDSLC");
        assertThat(r.outcome().commarea()).hasSize(160);
        CardDemoCommarea c = commarea(r);
        assertThat(CardDemoCommarea.ACCT_ID.raw(c)).isEqualTo("00000000002");
        assertThat(CardDemoCommarea.CARD_NUM.raw(c)).isEqualTo("0923877193247330");
        assertThat(c.fromProgram()).isEqualTo("COCRDLIC");
        assertThat(c.fromTranid()).isEqualTo("CCLI");
        assertThat(c.lastMap()).isEqualTo("CCRDLIA");
        assertThat(c.lastMapset()).isEqualTo("COCRDLI");
        assertThat(c.isPgmEnter()).isTrue();
        // SET CDEMO-USRTYP-USER (line 522) turns an admin into a regular user
        assertThat(c.userType()).isEqualTo("U");
        assertThat(c.userId()).isEqualTo("ADMIN001");
    }

    @Test
    void selectUUpdatesTheCard() {
        // lines 545-569: XCTL COCRDUPC
        CicsResponse r = next(page(2), AidKey.ENTER, fields("CRDSEL2", "U"));
        assertThat(r.outcome().nextProgram()).isEqualTo("COCRDUPC");
        CardDemoCommarea c = commarea(r);
        assertThat(CardDemoCommarea.CARD_NUM.raw(c)).isEqualTo(PAGE2[1][0]);
        assertThat(CardDemoCommarea.ACCT_ID.raw(c)).isEqualTo(PAGE2[1][1]);
    }

    @Test
    void multipleSelectionsAreRejectedAndBrowseRestartsFromTheFirstCard() {
        // 2250 lines 1079-1095; WHEN INPUT-ERROR reads forward with WS-CARD-RID-CARDNUM still SPACES, so page 2
        // shows page 1's cards while WS-CA-SCREEN-NUM stays 2 (bug-for-bug)
        CicsResponse r = next(page(2), AidKey.ENTER, fields("CRDSEL1", "S", "CRDSEL4", "U"));
        BmsScreen s = screen(r);
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("PLEASE SELECT ONLY ONE RECORD TO VIEW OR UPDATE"));
        assertThat(rows(s)).containsExactlyElementsOf(cardNums(PAGE1));
        assertThat(s.value("PAGENO")).isEqualTo("2  ");
        assertThat(s.value("CRDSEL1")).isEqualTo("S");
        assertThat(s.value("CRDSEL4")).isEqualTo("U");
        assertThat(task.ccrdlia.color("CRDSEL1")).isEqualTo(Dfhbmsca.DFHRED);
        assertThat(task.ccrdlia.color("CRDSEL4")).isEqualTo(Dfhbmsca.DFHRED);
        assertThat(task.ccrdlia.color("CRDSEL2")).isZero();
        // row 1 never gets the cursor (lines 755-760); row 4 does (line 794)
        assertThat(s.cursorField()).isEqualTo("CRDSEL4");
        assertThat(s.value("INFOMSG")).isEqualTo(INFO);
        assertThat(ThisProgCommarea.FIRST_CARD_NUM.get(prog(r))).isEqualTo(PAGE1[0][0]);
        assertReturnTransid(r);
    }

    @Test
    void invalidActionCodeIsRejected() {
        // 2250 WHEN OTHER (1108-1113)
        CicsResponse r = next(first(), AidKey.ENTER, fields("CRDSEL2", "X"));
        BmsScreen s = screen(r);
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("INVALID ACTION CODE"));
        assertThat(s.value("CRDSEL2")).isEqualTo("X");
        assertThat(task.ccrdlia.color("CRDSEL2")).isEqualTo(Dfhbmsca.DFHRED);
        assertThat(s.cursorField()).isEqualTo("CRDSEL2");
        assertThat(rows(s)).containsExactlyElementsOf(cardNums(PAGE1));
    }

    @Test
    void lowerCaseSelectionIsAnInvalidActionCode() {
        // SELECT-OK VALUES 'S', 'U' (line 77): no upper-casing
        CicsResponse r = next(first(), AidKey.ENTER, fields("CRDSEL5", "s"));
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.RETURN_TRANSID);
        assertThat(screen(r).value("ERRMSG")).isEqualTo(errmsg("INVALID ACTION CODE"));
        assertThat(screen(r).cursorField()).isEqualTo("CRDSEL5");
    }

    @Test
    void invalidCodeOnRowOneWithValidSelectionElsewhere() {
        // one S/U only (no MORE-THAN-1) but row 1 'X' -> INPUT-ERROR; row 1 red without cursor, so the cursor goes
        // to the map's IC field (ACCTSID); I-SELECTED = 2 but the XCTL WHENs are not reached
        CicsResponse r = next(first(), AidKey.ENTER, fields("CRDSEL1", "X", "CRDSEL2", "S"));
        BmsScreen s = screen(r);
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.RETURN_TRANSID);
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("INVALID ACTION CODE"));
        assertThat(task.ccrdlia.color("CRDSEL1")).isEqualTo(Dfhbmsca.DFHRED);
        assertThat(task.ccrdlia.color("CRDSEL2")).isZero();
        assertThat(task.iSelected).isEqualTo(2);
        assertThat(s.cursorField()).isEqualTo("ACCTSID");
    }

    @Test
    void selectionWithPf8IsIgnored() {
        // the XCTL WHENs require CCARD-AID-ENTER; PF8 pages forward instead
        CicsResponse r = next(first(), AidKey.PF8, fields("CRDSEL1", "S"));
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.RETURN_TRANSID);
        assertThat(rows(screen(r))).containsExactlyElementsOf(cardNums(PAGE2));
        // the typed 'S' is echoed into row 1 of the new page (1200 moves WS-EDIT-SELECT(1))
        assertThat(screen(r).value("CRDSEL1")).isEqualTo("S");
    }

    // ------------------------------------------------------------------ file errors

    @Test
    void disabledCardFileAbendsOnEndbr() {
        // STARTBR/READNEXT use RESP; ENDBR (line 1258) does not, so its INVREQ raises the default abend AEIP
        carddat.setState(FileState.DISABLED);
        CicsResponse r = first();
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.ABEND);
        assertThat(r.outcome().abendCode()).isEqualTo("AEIP");
        assertThat(r.terminalOutputs()).isEmpty();
        assertThat(task.wsErrorMsg)
                .isEqualTo("File Error: READ     on CARDDAT   returned RESP 000000084 ,RESP2 000000050 ");
    }

    @Test
    void undefinedCardFileAbendsWithDsiderr() {
        fct = new FileControl();
        CicsResponse r = first();
        assertThat(r.outcome().abendCode()).isEqualTo("AEIL");
    }

    // ------------------------------------------------------------------ helpers

    @Test
    void nibbleValueTreatsSpacesAndLowValuesAsZero() {
        assertThat(Cocrdlic.nibbleValue("00000000027")).isEqualTo(27);
        assertThat(Cocrdlic.nibbleValue("           ")).isZero();
        assertThat(Cocrdlic.nibbleValue("\0\0\0")).isZero();
        assertThat(Cocrdlic.nibbleValue("0000000000 ")).isZero();
    }
}
