package com.carddemo.online.cocrdlic;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.common.copybook.CardDataSeed;
import com.carddemo.common.copybook.CardDemoCommarea;
import com.carddemo.common.copybook.CardRecord;
import com.carddemo.online.cocrdlic.Cocrdlic.Dfhbmsca;
import com.carddemo.runtime.bms.BmsScreen;
import com.carddemo.runtime.cics.AidKey;
import com.carddemo.runtime.cics.CicsContext;
import com.carddemo.runtime.cics.CicsDispatcher;
import com.carddemo.runtime.cics.CicsOutcome;
import com.carddemo.runtime.cics.CicsProgram;
import com.carddemo.runtime.cics.CicsResponse;
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
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

/**
 * Behavioural parity tests for COCRDLIC. Line numbers refer to app/cbl/COCRDLIC.cbl. Card data is the CARDDAT
 * seed {@code app/data/ASCII/carddata.txt} (50 records, one card per account, all CARD-ACTIVE-STATUS 'Y'),
 * which in key (CARD-NUM) order is:
 * <pre>
 *  1 0500024453765740 acct 00000000050     8 1561409106491600 acct 00000000035    15 3766281984155154 acct 00000000041
 *  2 0683586198171516 acct 00000000027     9 2745303720002090 acct 00000000039    ...
 *  3 0923877193247330 acct 00000000002    10 2760836797107565 acct 00000000024    49 9680294154603697 acct 00000000001
 *  4 0927987108636232 acct 00000000020    11 2871968252812490 acct 00000000006    50 9805583408996588 acct 00000000040
 *  5 0982496213629795 acct 00000000012    12 2940139362300449 acct 00000000022
 *  6 1014086565224350 acct 00000000044    13 2988091353094312 acct 00000000004
 *  7 1142167692878931 acct 00000000037    14 3260763612337560 acct 00000000010
 * </pre>
 * so 9000-READ-FORWARD pages are records 1-7, 8-14, ..., 43-49 and 50 (page 8).
 */
class CocrdlicTest {

    /** 2024-03-05 14:07:09.25 UTC: CURDATE 03/05/24, CURTIME 14:07:09 (1100-SCREEN-INIT lines 645-664). */
    private static final Clock CLOCK = Clock.fixed(Instant.parse("2024-03-05T14:07:09.25Z"), ZoneOffset.UTC);
    private static final String LOW = String.valueOf(PicX.LOW_VALUE);

    static final String[][] SEED = {
        {"0500024453765740", "00000000050"}, {"0683586198171516", "00000000027"}, {"0923877193247330", "00000000002"},
        {"0927987108636232", "00000000020"}, {"0982496213629795", "00000000012"}, {"1014086565224350", "00000000044"},
        {"1142167692878931", "00000000037"}, {"1561409106491600", "00000000035"}, {"2745303720002090", "00000000039"},
        {"2760836797107565", "00000000024"}, {"2871968252812490", "00000000006"}, {"2940139362300449", "00000000022"},
        {"2988091353094312", "00000000004"}, {"3260763612337560", "00000000010"}, {"3766281984155154", "00000000041"},
    };
    static final String CARD_50 = "9805583408996588";

    private KsdsFile carddat = KsdsFile.inMemory("CARDDAT", CardRecord.LENGTH, 0, 16);
    private FileControl fct = new FileControl().register(carddat);
    private Cocrdlic.Task task;

    CocrdlicTest() {
        carddat.load(CardDataSeed.records());
    }

    /** Runs one task through the dispatcher, keeping the Task to inspect colour bytes of the symbolic map. */
    private CicsResponse run(AidKey aid, byte[] commarea, Map<String, String> fields) {
        CicsProgram p = new CicsProgram() {
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
                task.mainPara();
            }
        };
        return CicsDispatcher.run(p, CicsContext.builder().transid("CCLI").aid(aid).commarea(commarea)
                .terminalInput(fields).fileControl(fct).clock(CLOCK).build());
    }

    private CicsResponse first() {
        return run(AidKey.ENTER, null, null);
    }

    private CicsResponse next(CicsResponse prev, AidKey aid, Map<String, String> fields) {
        return run(aid, prev.outcome().commarea(), fields);
    }

    private CicsResponse next(CicsResponse prev, AidKey aid) {
        return next(prev, aid, Map.of());
    }

    private static Map<String, String> sel(int row, String code) {
        return Map.of("CRDSEL" + row, code);
    }

    private static BmsScreen screen(CicsResponse r) {
        return r.lastMap().screen();
    }

    private static CardDemoCommarea commarea(CicsResponse r) {
        return CardDemoCommarea.of(Arrays.copyOf(r.outcome().commarea(), CardDemoCommarea.LENGTH));
    }

    private static ThisProgCommarea prog(CicsResponse r) {
        return ThisProgCommarea.of(Arrays.copyOfRange(r.outcome().commarea(), CardDemoCommarea.LENGTH,
                CardDemoCommarea.LENGTH + ThisProgCommarea.LENGTH));
    }

    /** Rows shown on the screen as "acct card status" (blank rows as ""). */
    private static List<String> rows(BmsScreen s) {
        List<String> out = new ArrayList<>();
        for (int n = 1; n <= 7; n++) {
            String line = (s.value("ACCTNO" + n) + " " + s.value("CRDNUM" + n) + " " + s.value("CRDSTS" + n)).trim();
            out.add(line);
        }
        return out;
    }

    /** Expected rows for seed records from..to (1-based, key order), padded with "" to 7 rows. */
    private static List<String> seedRows(int from, int to) {
        List<String> out = new ArrayList<>();
        for (int k = from; k <= to; k++) {
            out.add(SEED[k - 1][1] + " " + SEED[k - 1][0] + " Y");
        }
        while (out.size() < 7) {
            out.add("");
        }
        return out;
    }

    private static String errmsg(String text) {
        return PicX.of(text, 78);
    }

    private static int attr(BmsScreen s, String field) {
        return s.field(field).orElseThrow().attributeByte();
    }

    private static void assertReturnsToSelf(CicsResponse r) {
        // COMMON-RETURN lines 604-619: RETURN TRANSID(CCLI) COMMAREA(WS-COMMAREA) LENGTH(2000)
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.RETURN_TRANSID);
        assertThat(r.outcome().nextTransid()).isEqualTo("CCLI");
        assertThat(r.outcome().commarea()).hasSize(2000);
        assertThat(PicX.isSpaces(PicX.fromBytes(Arrays.copyOfRange(r.outcome().commarea(), 414, 2000)))).isTrue();
        CardDemoCommarea c = commarea(r);
        assertThat(c.fromTranid()).isEqualTo("CCLI");
        assertThat(c.fromProgram()).isEqualTo("COCRDLIC");
        assertThat(c.lastMapset()).isEqualTo("COCRDLI");
        assertThat(c.lastMap()).isEqualTo("CCRDLIA");
    }

    private CicsResponse goToPage(int page) {
        CicsResponse r = first();
        for (int p = 2; p <= page; p++) {
            r = next(r, AidKey.PF8);
        }
        return r;
    }

    // ------------------------------------------------------------------ first entry / 1100 / 1200 / 1300

    @Test
    void firstEntryListsFirstSevenCards() {
        // 0000-MAIN lines 315-325 (EIBCALEN = 0), WHEN OTHER 572-582, 9000-READ-FORWARD from SPACES
        CicsResponse r = first();
        BmsScreen s = screen(r);
        assertThat(s.map()).isEqualTo("CCRDLIA");
        assertThat(s.erase()).isTrue();
        assertThat(s.value("TRNNAME")).isEqualTo("CCLI");
        assertThat(s.value("PGMNAME")).isEqualTo("COCRDLIC");
        assertThat(s.value("TITLE01")).isEqualTo("      AWS Mainframe Modernization       ");
        assertThat(s.value("CURDATE")).isEqualTo("03/05/24");
        assertThat(s.value("CURTIME")).isEqualTo("14:07:09");
        assertThat(s.value("PAGENO")).isEqualTo("1  ");
        assertThat(rows(s)).isEqualTo(seedRows(1, 7));
        // 1400 WHEN WS-NO-INFO-MESSAGE (line 917): info line, NEUTRAL; no error
        assertThat(s.value("INFOMSG")).isEqualTo(PicX.of("TYPE S FOR DETAIL, U TO UPDATE ANY RECORD", 45));
        assertThat(task.ccrdlia.color("INFOMSG")).isEqualTo(Dfhbmsca.DFHNEUTR);
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg(""));
        // 1300 lines 839-842 skip the filters on first entry; line 884 INPUT-OK -> cursor on ACCTSID
        assertThat(s.field("ACCTSID").orElseThrow().sent()).isFalse();
        assertThat(s.cursorField()).isEqualTo("ACCTSID");
        // 1250: rows with data unprotected (DFHBMFSE); selections were SPACES (INITIALIZE, line 301)
        for (int n = 1; n <= 7; n++) {
            assertThat(attr(s, "CRDSEL" + n)).isEqualTo(Dfhbmsca.DFHBMFSE);
            assertThat(s.value("CRDSEL" + n)).isEqualTo(" ");
        }
        assertReturnsToSelf(r);
        CardDemoCommarea c = commarea(r);
        assertThat(c.userType()).isEqualTo("U");
        assertThat(c.isPgmEnter()).isTrue();
        assertThat(c.userId()).isEqualTo(PicX.spaces(8));
        assertThat(CardDemoCommarea.ACCT_ID.raw(c)).isEqualTo("00000000000");
        ThisProgCommarea p = prog(r);
        assertThat(p.screenNum()).isEqualTo(1);
        assertThat(p.firstCardNum()).isEqualTo(SEED[0][0]);
        assertThat(ThisProgCommarea.FIRST_CARD_ACCT_ID.raw(p)).isEqualTo(SEED[0][1]);
        // lines 1191-1214: the 8th record read becomes WS-CA-LAST-CARDKEY and sets CA-NEXT-PAGE-EXISTS
        assertThat(p.lastCardNum()).isEqualTo(SEED[7][0]);
        assertThat(ThisProgCommarea.NEXT_PAGE_IND.get(p)).isEqualTo("Y");
        assertThat(ThisProgCommarea.CA_LAST_PAGE_NOT_SHOWN.isTrue(p)).isTrue();
        assertThat(p.rowCardNum(7)).isEqualTo(SEED[6][0]);
        assertThat(s.render().get(3)).contains("List Credit Cards");
        assertThat(s.render().get(23)).contains("F3=Exit F7=Backward  F8=Forward");
    }

    @Test
    void entryFromMenuKeepsUserAndIgnoresCommareaFilters() {
        // lines 327-343: PGM-ENTER from another program resets WS-THIS-PROGCOMMAREA; 1300 line 840-842 skips filters
        CardDemoCommarea in = new CardDemoCommarea();
        Cocrdlic.initialize(in, CardDemoCommarea.LAYOUT);
        in.setFromTranid("CM00");
        in.setFromProgram("COMEN01C");
        in.setUserId("ADMIN001");
        in.setUserType("A");
        in.setPgmContext(0);
        CardDemoCommarea.ACCT_ID.set(in, 24);
        CicsResponse r = run(AidKey.ENTER, in.toBytes(), Map.of("ACCTSID", "IGNORED"));
        BmsScreen s = screen(r);
        assertThat(rows(s)).isEqualTo(seedRows(1, 7));
        assertThat(s.field("ACCTSID").orElseThrow().sent()).isFalse();
        assertThat(s.value("PAGENO")).isEqualTo("1  ");
        CardDemoCommarea c = commarea(r);
        assertThat(c.userId()).isEqualTo("ADMIN001");
        assertThat(c.userType()).isEqualTo("A");
        assertReturnsToSelf(r);
    }

    // ------------------------------------------------------------------ paging

    @Test
    void pf8ShowsNextPage() {
        // lines 486-497: STARTBR GTEQ WS-CA-LAST-CARD-NUM (record 8), ADD 1 TO WS-CA-SCREEN-NUM
        CicsResponse r = next(first(), AidKey.PF8);
        BmsScreen s = screen(r);
        assertThat(rows(s)).isEqualTo(seedRows(8, 14));
        assertThat(s.value("PAGENO")).isEqualTo("2  ");
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg(""));
        assertThat(s.value("INFOMSG")).isEqualTo(PicX.of("TYPE S FOR DETAIL, U TO UPDATE ANY RECORD", 45));
        ThisProgCommarea p = prog(r);
        assertThat(p.firstCardNum()).isEqualTo(SEED[7][0]);
        assertThat(p.lastCardNum()).isEqualTo(SEED[14][0]);
        assertReturnsToSelf(r);
    }

    @Test
    void lastPageAndPf8BeyondIt() {
        // page 8 holds record 50 only: READNEXT ENDFILE (lines 1233-1240) -> 'NO MORE RECORDS TO SHOW'
        CicsResponse last = goToPage(8);
        BmsScreen s = screen(last);
        assertThat(s.value("PAGENO")).isEqualTo("8  ");
        assertThat(rows(s).get(0)).isEqualTo("00000000040 " + CARD_50 + " Y");
        assertThat(rows(s).subList(1, 7)).allMatch(String::isEmpty);
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("NO MORE RECORDS TO SHOW"));
        // 1400 lines 910-916: PF8 + next page not exists -> info line, CA-LAST-PAGE-SHOWN
        assertThat(s.value("INFOMSG")).isEqualTo(PicX.of("TYPE S FOR DETAIL, U TO UPDATE ANY RECORD", 45));
        assertThat(attr(s, "CRDSEL1")).isEqualTo(Dfhbmsca.DFHBMFSE);
        for (int n = 2; n <= 7; n++) {
            assertThat(attr(s, "CRDSEL" + n)).isEqualTo(Dfhbmsca.DFHBMPRO);
        }
        ThisProgCommarea p = prog(last);
        assertThat(ThisProgCommarea.CA_NEXT_PAGE_NOT_EXISTS.isTrue(p)).isTrue();
        assertThat(ThisProgCommarea.CA_LAST_PAGE_SHOWN.isTrue(p)).isTrue();
        // ENDFILE moves the last record read into WS-CA-LAST-CARDKEY (lines 1236-1237)
        assertThat(p.lastCardNum()).isEqualTo(CARD_50);

        // PF8 again: no next page -> WHEN OTHER re-reads the same page; 1400 lines 905-909
        CicsResponse again = next(last, AidKey.PF8);
        BmsScreen s2 = screen(again);
        assertThat(s2.value("PAGENO")).isEqualTo("8  ");
        assertThat(rows(s2)).isEqualTo(rows(s));
        assertThat(s2.value("ERRMSG")).isEqualTo(errmsg("NO MORE PAGES TO DISPLAY"));
        assertThat(s2.value("INFOMSG")).isEqualTo(PicX.spaces(45));
        assertThat(task.ccrdlia.color("INFOMSG")).isEqualTo(Dfhbmsca.DFHBMDAR);

        // any other key resets CA-LAST-PAGE-NOT-SHOWN (line 410-414)
        CicsResponse enter = next(again, AidKey.ENTER);
        assertThat(screen(enter).value("ERRMSG")).isEqualTo(errmsg("NO MORE RECORDS TO SHOW"));
        assertThat(ThisProgCommarea.CA_LAST_PAGE_NOT_SHOWN.isTrue(prog(enter))).isTrue();
    }

    @Test
    void pf7OnFirstPage() {
        // lines 439-454 re-read from WS-CA-FIRST-CARD-NUM; 1400 lines 901-904
        CicsResponse r = next(first(), AidKey.PF7);
        BmsScreen s = screen(r);
        assertThat(rows(s)).isEqualTo(seedRows(1, 7));
        assertThat(s.value("PAGENO")).isEqualTo("1  ");
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("NO PREVIOUS PAGES TO DISPLAY"));
        assertThat(s.value("INFOMSG")).isEqualTo(PicX.spaces(45));
    }

    @Test
    void pf7ReadsBackwards() {
        // lines 501-513 + 9100-READ-BACKWARDS: READPREV returns record 15 (current first), then 14..8 into rows 7..1
        CicsResponse page3 = goToPage(3);
        assertThat(screen(page3).value("CRDNUM1")).isEqualTo(SEED[14][0]);
        CicsResponse r = next(page3, AidKey.PF7);
        BmsScreen s = screen(r);
        assertThat(rows(s)).isEqualTo(seedRows(8, 14));
        assertThat(s.value("PAGENO")).isEqualTo("2  ");
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg(""));
        ThisProgCommarea p = prog(r);
        assertThat(p.firstCardNum()).isEqualTo(SEED[7][0]);
        // line 1268: WS-CA-LAST-CARDKEY = previous WS-CA-FIRST-CARDKEY (record 15)
        assertThat(p.lastCardNum()).isEqualTo(SEED[14][0]);
        assertThat(ThisProgCommarea.NEXT_PAGE_IND.get(p)).isEqualTo("Y");
        // and PF8 from there goes to page 3 again
        assertThat(screen(next(r, AidKey.PF8)).value("CRDNUM1")).isEqualTo(SEED[14][0]);
    }

    @Test
    void pf7BackToFirstPageSaysNoPreviousPages() {
        // bug-for-bug: SUBTRACT 1 happens before 1400, so CA-FIRST-PAGE is true when page 1 is reached (line 901)
        CicsResponse r = next(goToPage(2), AidKey.PF7);
        BmsScreen s = screen(r);
        assertThat(rows(s)).isEqualTo(seedRows(1, 7));
        assertThat(s.value("PAGENO")).isEqualTo("1  ");
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("NO PREVIOUS PAGES TO DISPLAY"));
    }

    @Test
    void pf7ReachingStartOfFileShowsFileError() {
        // 9100 lines 1361-1369: READPREV ENDFILE before 7 rows are filled is reported as a file error.
        // Commarea crafted as page 3 starting at record 3: READPREV gives 3 (skipped), 2 -> row 7, 1 -> row 6, ENDFILE.
        CicsResponse base = first();
        byte[] ca = base.outcome().commarea();
        ThisProgCommarea p = prog(base);
        ThisProgCommarea.FIRST_CARD_NUM.set(p, SEED[2][0]);
        ThisProgCommarea.SCREEN_NUM.set(p, 3);
        System.arraycopy(p.toBytes(), 0, ca, CardDemoCommarea.LENGTH, ThisProgCommarea.LENGTH);
        CicsResponse r = run(AidKey.PF7, ca, Map.of());
        BmsScreen s = screen(r);
        List<String> expected = new ArrayList<>(List.of("", "", "", "", ""));
        expected.add(seedRows(1, 1).getFirst());
        expected.add(seedRows(2, 2).getFirst());
        assertThat(rows(s)).isEqualTo(expected);
        assertThat(s.value("PAGENO")).isEqualTo("2  ");
        // ENDFILE = RESP 20, RESP2 90 (runtime); WS-FILE-ERROR-MESSAGE truncated to 75 bytes
        assertThat(s.value("ERRMSG")).isEqualTo(
                errmsg("File Error: READ     on CARDDAT   returned RESP 000000020 ,RESP2 000000090 "));
        assertThat(attr(s, "CRDSEL1")).isEqualTo(Dfhbmsca.DFHBMPRF);
        assertThat(attr(s, "CRDSEL6")).isEqualTo(Dfhbmsca.DFHBMFSE);
        // WS-CA-FIRST-CARDKEY only changes when row 1 is filled
        assertThat(prog(r).firstCardNum()).isEqualTo(SEED[2][0]);
    }

    @Test
    void pageNumberWrapsAfterNine() {
        // ADD +1 TO WS-CA-SCREEN-NUM (PIC 9) truncates 10 to 0; 9000 line 1177 then sets it to 1
        CicsResponse base = first();
        byte[] ca = base.outcome().commarea();
        ThisProgCommarea p = prog(base);
        ThisProgCommarea.SCREEN_NUM.set(p, 9);
        System.arraycopy(p.toBytes(), 0, ca, CardDemoCommarea.LENGTH, ThisProgCommarea.LENGTH);
        CicsResponse r = run(AidKey.PF8, ca, Map.of());
        assertThat(screen(r).value("PAGENO")).isEqualTo("1  ");
        assertThat(rows(screen(r))).isEqualTo(seedRows(8, 14));
    }

    @Test
    void exactlySevenRecordsGivesSinglePageWithEndMessage() {
        // lines 1215-1221: 7th row filled, the look-ahead READNEXT hits ENDFILE
        carddat = KsdsFile.inMemory("CARDDAT", CardRecord.LENGTH, 0, 16);
        carddat.load(CardDataSeed.records().subList(0, 7));
        fct = new FileControl().register(carddat);
        CicsResponse r = first();
        List<String> shown = rows(screen(r));
        assertThat(shown).allMatch(row -> !row.isEmpty());
        assertThat(screen(r).value("ERRMSG")).isEqualTo(errmsg("NO MORE RECORDS TO SHOW"));
        assertThat(ThisProgCommarea.CA_NEXT_PAGE_NOT_EXISTS.isTrue(prog(r))).isTrue();
        // PF8: no next page -> same page, first time only the info line (lines 910-916)
        CicsResponse pf8 = next(r, AidKey.PF8);
        assertThat(screen(pf8).value("PAGENO")).isEqualTo("1  ");
        assertThat(screen(pf8).value("ERRMSG")).isEqualTo(errmsg("NO MORE RECORDS TO SHOW"));
        assertThat(screen(next(pf8, AidKey.PF8)).value("ERRMSG")).isEqualTo(errmsg("NO MORE PAGES TO DISPLAY"));
    }

    // ------------------------------------------------------------------ filters (2210 / 2220 / 9500)

    @Test
    void accountFilterMatch() {
        CicsResponse r = next(first(), AidKey.ENTER, Map.of("ACCTSID", "00000000024"));
        BmsScreen s = screen(r);
        assertThat(rows(s)).isEqualTo(List.of(SEED[9][1] + " " + SEED[9][0] + " Y", "", "", "", "", "", ""));
        // filtered loop runs to ENDFILE (lines 1233-1240)
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("NO MORE RECORDS TO SHOW"));
        assertThat(s.value("INFOMSG")).isEqualTo(PicX.of("TYPE S FOR DETAIL, U TO UPDATE ANY RECORD", 45));
        // 1300 lines 845-848 echo the filter
        assertThat(s.value("ACCTSID")).isEqualTo("00000000024");
        assertThat(attr(s, "ACCTSID")).isEqualTo(Dfhbmsca.DFHBMFSE);
        assertThat(s.cursorField()).isEqualTo("ACCTSID");
        assertThat(CardDemoCommarea.ACCT_ID.raw(commarea(r))).isEqualTo("00000000024");
        assertThat(ThisProgCommarea.CA_NEXT_PAGE_NOT_EXISTS.isTrue(prog(r))).isTrue();
    }

    @Test
    void cardFilterAndBothFilters() {
        CicsResponse card = next(first(), AidKey.ENTER, Map.of("CARDSID", SEED[4][0]));
        assertThat(rows(screen(card)).getFirst()).isEqualTo(SEED[4][1] + " " + SEED[4][0] + " Y");
        assertThat(rows(screen(card)).get(1)).isEmpty();
        assertThat(screen(card).value("CARDSID")).isEqualTo(SEED[4][0]);
        assertThat(CardDemoCommarea.CARD_NUM.raw(commarea(card))).isEqualTo(SEED[4][0]);

        CicsResponse both = next(first(), AidKey.ENTER, Map.of("ACCTSID", SEED[4][1], "CARDSID", SEED[4][0]));
        assertThat(rows(screen(both)).getFirst()).isEqualTo(SEED[4][1] + " " + SEED[4][0] + " Y");
        CicsResponse mismatch = next(first(), AidKey.ENTER, Map.of("ACCTSID", SEED[3][1], "CARDSID", SEED[4][0]));
        assertThat(screen(mismatch).value("ERRMSG")).isEqualTo(errmsg("NO RECORDS FOUND FOR THIS SEARCH CONDITION."));
    }

    @Test
    void filterWithNoMatch() {
        // lines 1241-1245: page 1 and nothing shown -> WS-NO-RECORDS-FOUND; 1400 line 927 suppresses the info line
        CicsResponse r = next(first(), AidKey.ENTER, Map.of("ACCTSID", "99999999999"));
        BmsScreen s = screen(r);
        assertThat(rows(s)).allMatch(String::isEmpty);
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("NO RECORDS FOUND FOR THIS SEARCH CONDITION."));
        assertThat(s.value("INFOMSG")).isEqualTo(PicX.spaces(45));
        for (int n = 1; n <= 7; n++) {
            assertThat(attr(s, "CRDSEL" + n)).isEqualTo(n == 1 ? Dfhbmsca.DFHBMPRF : Dfhbmsca.DFHBMPRO);
        }
        // CARD-RECORD still holds record 50 (last successful READNEXT), moved to WS-CA-LAST-CARDKEY
        assertThat(prog(r).lastCardNum()).isEqualTo(CARD_50);
    }

    @Test
    void filterWithNoMatchOnLaterPageSaysNoMoreRecords() {
        // bug-for-bug: the filtered browse starts at the current page's first card and keeps WS-CA-SCREEN-NUM,
        // so on page 2 an unmatched (or earlier) account gives 'NO MORE RECORDS TO SHOW', not 'NO RECORDS FOUND'
        CicsResponse r = next(goToPage(2), AidKey.ENTER, Map.of("ACCTSID", SEED[0][1]));
        BmsScreen s = screen(r);
        assertThat(rows(s)).allMatch(String::isEmpty);
        assertThat(s.value("PAGENO")).isEqualTo("2  ");
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("NO MORE RECORDS TO SHOW"));
    }

    @Test
    void invalidAccountFilter() {
        // 2210 lines 1017-1025; WHEN INPUT-ERROR (419-438) skips the read: rows come from the commarea, protected
        CicsResponse page2 = goToPage(2);
        CicsResponse r = next(page2, AidKey.ENTER, Map.of("ACCTSID", "ABC", "CRDSEL1", "S"));
        BmsScreen s = screen(r);
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER"));
        assertThat(rows(s)).isEqualTo(seedRows(8, 14));
        assertThat(s.value("ACCTSID")).isEqualTo("ABC        ");
        assertThat(task.ccrdlia.color("ACCTSID")).isEqualTo(Dfhbmsca.DFHRED);
        assertThat(s.cursorField()).isEqualTo("ACCTSID");
        assertThat(attr(s, "CRDSEL1")).isEqualTo(Dfhbmsca.DFHBMPRF);
        assertThat(attr(s, "CRDSEL2")).isEqualTo(Dfhbmsca.DFHBMPRO);
        // the S typed in row 1 is echoed (1200) but not acted on (2250 exits on INPUT-ERROR)
        assertThat(s.value("CRDSEL1")).isEqualTo("S");
        assertThat(s.value("INFOMSG")).isEqualTo(PicX.spaces(45));
        assertThat(CardDemoCommarea.ACCT_ID.raw(commarea(r))).isEqualTo("00000000000");
        assertReturnsToSelf(r);
    }

    @Test
    void invalidCardFilterAndBothInvalid() {
        // 2220 lines 1052-1062: a short number is space padded by RECEIVE MAP and so NOT NUMERIC
        CicsResponse card = next(first(), AidKey.ENTER, Map.of("CARDSID", "12345"));
        BmsScreen s = screen(card);
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("CARD ID FILTER,IF SUPPLIED MUST BE A 16 DIGIT NUMBER"));
        assertThat(task.ccrdlia.color("CARDSID")).isEqualTo(Dfhbmsca.DFHRED);
        assertThat(s.cursorField()).isEqualTo("CARDSID");
        assertThat(s.field("ACCTSID").orElseThrow().sent()).isFalse();

        // account message wins (line 1056 only sets the card message when WS-ERROR-MSG is still SPACES)
        CicsResponse both = next(first(), AidKey.ENTER, Map.of("ACCTSID", "1234", "CARDSID", "X"));
        BmsScreen b = screen(both);
        assertThat(b.value("ERRMSG")).isEqualTo(errmsg("ACCOUNT FILTER,IF SUPPLIED MUST BE A 11 DIGIT NUMBER"));
        assertThat(task.ccrdlia.color("ACCTSID")).isEqualTo(Dfhbmsca.DFHRED);
        assertThat(task.ccrdlia.color("CARDSID")).isEqualTo(Dfhbmsca.DFHRED);
        assertThat(b.cursorField()).isEqualTo("ACCTSID");
    }

    @Test
    void zeroFiltersAreBlank() {
        // lines 1007-1012 / 1042-1047: CC-ACCT-ID-N / CC-CARD-NUM-N = ZEROS count as not supplied
        CicsResponse r = next(first(), AidKey.ENTER, Map.of("ACCTSID", "00000000000", "CARDSID", "0000000000000000"));
        BmsScreen s = screen(r);
        assertThat(rows(s)).isEqualTo(seedRows(1, 7));
        assertThat(s.field("ACCTSID").orElseThrow().sent()).isFalse();
        assertThat(s.field("CARDSID").orElseThrow().sent()).isFalse();
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg(""));
    }

    // ------------------------------------------------------------------ row selection (2250, XCTL)

    @Test
    void selectSGoesToCardDetail() {
        // lines 517-541: XCTL COCRDSLC with the selected row's keys from WS-SCREEN-DATA (commarea)
        CicsResponse r = next(first(), AidKey.ENTER, sel(3, "S"));
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.XCTL);
        assertThat(r.outcome().nextProgram()).isEqualTo("COCRDSLC");
        assertThat(r.outcome().commarea()).hasSize(160);
        assertThat(r.lastMap()).isNull();
        CardDemoCommarea c = CardDemoCommarea.of(r.outcome().commarea());
        assertThat(CardDemoCommarea.ACCT_ID.raw(c)).isEqualTo(SEED[2][1]);
        assertThat(CardDemoCommarea.CARD_NUM.raw(c)).isEqualTo(SEED[2][0]);
        assertThat(c.fromTranid()).isEqualTo("CCLI");
        assertThat(c.fromProgram()).isEqualTo("COCRDLIC");
        assertThat(c.userType()).isEqualTo("U");
        assertThat(c.isPgmEnter()).isTrue();
        assertThat(c.lastMapset()).isEqualTo("COCRDLI");
        assertThat(c.lastMap()).isEqualTo("CCRDLIA");
    }

    @Test
    void selectUGoesToCardUpdate() {
        CicsResponse r = next(goToPage(2), AidKey.ENTER, sel(7, "U"));
        assertThat(r.outcome().nextProgram()).isEqualTo("COCRDUPC");
        CardDemoCommarea c = CardDemoCommarea.of(r.outcome().commarea());
        assertThat(CardDemoCommarea.CARD_NUM.raw(c)).isEqualTo(SEED[13][0]);
        assertThat(CardDemoCommarea.ACCT_ID.raw(c)).isEqualTo(SEED[13][1]);
    }

    @Test
    void invalidPfKeyActsAsEnterSoSelectionStillTransfers() {
        // lines 370-380: keys other than ENTER/PF3/PF7/PF8 are treated as ENTER
        CicsResponse r = next(first(), AidKey.PF5, sel(1, "S"));
        assertThat(r.outcome().nextProgram()).isEqualTo("COCRDSLC");
    }

    @Test
    void multipleSelections() {
        // 2250 lines 1079-1095 + 1099-1115; WHEN INPUT-ERROR re-reads from WS-CARD-RID-CARDNUM = SPACES (line 433)
        CicsResponse page2 = goToPage(2);
        Map<String, String> in = new LinkedHashMap<>();
        in.put("CRDSEL1", "S");
        in.put("CRDSEL3", "U");
        CicsResponse r = next(page2, AidKey.ENTER, in);
        BmsScreen s = screen(r);
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.RETURN_TRANSID);
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("PLEASE SELECT ONLY ONE RECORD TO VIEW OR UPDATE"));
        // bug-for-bug: page 1 records under page number 2
        assertThat(rows(s)).isEqualTo(seedRows(1, 7));
        assertThat(s.value("PAGENO")).isEqualTo("2  ");
        assertThat(s.value("CRDSEL1")).isEqualTo("S");
        assertThat(s.value("CRDSEL3")).isEqualTo("U");
        assertThat(task.ccrdlia.color("CRDSEL1")).isEqualTo(Dfhbmsca.DFHRED);
        assertThat(task.ccrdlia.color("CRDSEL3")).isEqualTo(Dfhbmsca.DFHRED);
        assertThat(task.ccrdlia.color("CRDSEL2")).isZero();
        // row 1 never gets the cursor (lines 755-759); row 3 does (line 782)
        assertThat(s.cursorField()).isEqualTo("CRDSEL3");
        assertThat(String.valueOf(task.wsEditSelectErrors)).isEqualTo("1010000");
        assertThat(s.value("INFOMSG")).isEqualTo(PicX.of("TYPE S FOR DETAIL, U TO UPDATE ANY RECORD", 45));
        assertThat(attr(s, "CRDSEL1")).isEqualTo(Dfhbmsca.DFHBMFSE);
    }

    @Test
    void invalidActionCode() {
        CicsResponse r = next(first(), AidKey.ENTER, sel(2, "X"));
        BmsScreen s = screen(r);
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("INVALID ACTION CODE"));
        assertThat(s.value("CRDSEL2")).isEqualTo("X");
        assertThat(task.ccrdlia.color("CRDSEL2")).isEqualTo(Dfhbmsca.DFHRED);
        assertThat(s.cursorField()).isEqualTo("CRDSEL2");

        // lower-case 's' is not an action code (88 SELECT-OK VALUES 'S','U')
        CicsResponse lower = next(first(), AidKey.ENTER, sel(1, "s"));
        assertThat(screen(lower).value("ERRMSG")).isEqualTo(errmsg("INVALID ACTION CODE"));
        assertThat(task.ccrdlia.color("CRDSEL1")).isEqualTo(Dfhbmsca.DFHRED);
        // error on row 1: no '*' (selection not blank), no symbolic cursor -> IC field ACCTSID
        assertThat(screen(lower).value("CRDSEL1")).isEqualTo("s");
        assertThat(screen(lower).cursorField()).isEqualTo("ACCTSID");
    }

    @Test
    void invalidCodeTogetherWithMultipleSelectionsKeepsMultipleMessage() {
        Map<String, String> in = Map.of("CRDSEL2", "S", "CRDSEL4", "U", "CRDSEL6", "X");
        CicsResponse r = next(first(), AidKey.ENTER, in);
        assertThat(screen(r).value("ERRMSG")).isEqualTo(errmsg("PLEASE SELECT ONLY ONE RECORD TO VIEW OR UPDATE"));
        assertThat(String.valueOf(task.wsEditSelectErrors)).isEqualTo("0101010");
        assertThat(screen(r).cursorField()).isEqualTo("CRDSEL2");
    }

    @Test
    void selectionWithPf8PagesAndEchoesTheCode() {
        // ENTER is required for the XCTL (line 517); with PF8 the code is accepted and echoed on the next page
        CicsResponse r = next(first(), AidKey.PF8, sel(2, "S"));
        BmsScreen s = screen(r);
        assertThat(rows(s)).isEqualTo(seedRows(8, 14));
        assertThat(s.value("CRDSEL2")).isEqualTo("S");
    }

    // ------------------------------------------------------------------ PF3 / re-entry from other programs

    @Test
    void pf3ReturnsToMenu() {
        // lines 384-406
        CicsResponse page = first();
        byte[] ca = page.outcome().commarea();
        CardDemoCommarea c0 = commarea(page);
        c0.setUserId("ADMIN001");
        c0.setUserType("A");
        System.arraycopy(c0.toBytes(), 0, ca, 0, CardDemoCommarea.LENGTH);
        CicsResponse r = run(AidKey.PF3, ca, Map.of());
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.XCTL);
        assertThat(r.outcome().nextProgram()).isEqualTo("COMEN01C");
        assertThat(r.outcome().commarea()).hasSize(160);
        CardDemoCommarea c = CardDemoCommarea.of(r.outcome().commarea());
        assertThat(c.toProgram()).isEqualTo("COMEN01C");
        assertThat(c.fromProgram()).isEqualTo("COCRDLIC");
        assertThat(c.userId()).isEqualTo("ADMIN001");
        // bug-for-bug: SET CDEMO-USRTYP-USER (line 388) demotes an admin
        assertThat(c.userType()).isEqualTo("U");
    }

    @Test
    void pf15FoldsToPf3() {
        assertThat(next(first(), AidKey.PF15).outcome().nextProgram()).isEqualTo("COMEN01C");
    }

    @Test
    void pf3FromAnotherProgramRestartsTheList() {
        // WHEN CCARD-AID-PFK03 (line 458) when CDEMO-FROM-PROGRAM is not COCRDLIC: INITIALIZE CARDDEMO-COMMAREA
        CardDemoCommarea in = new CardDemoCommarea();
        Cocrdlic.initialize(in, CardDemoCommarea.LAYOUT);
        in.setFromProgram("COCRDSLC");
        in.setUserId("USER0001");
        in.setPgmContext(0);
        CicsResponse r = run(AidKey.PF3, in.toBytes(), null);
        BmsScreen s = screen(r);
        assertThat(rows(s)).isEqualTo(seedRows(1, 7));
        assertThat(s.value("PAGENO")).isEqualTo("1  ");
        assertThat(commarea(r).userId()).isEqualTo(PicX.spaces(8));
        assertReturnsToSelf(r);
    }

    @Test
    void returnFromDetailShowsCommareaKeysButListIsUnfiltered() {
        // 1300 WHEN OTHER (lines 851-853 / 863-866) shows CDEMO-ACCT-ID / CDEMO-CARD-NUM, but 9500 only
        // filters on keys typed in this task, so all cards are listed (bug-for-bug)
        CardDemoCommarea in = new CardDemoCommarea();
        Cocrdlic.initialize(in, CardDemoCommarea.LAYOUT);
        in.setFromProgram("COCRDSLC");
        in.setPgmContext(0);
        CardDemoCommarea.ACCT_ID.set(in, 24);
        CardDemoCommarea.CARD_NUM.set(in, new java.math.BigDecimal(SEED[9][0]));
        CicsResponse r = run(AidKey.ENTER, in.toBytes(), null);
        BmsScreen s = screen(r);
        assertThat(s.value("ACCTSID")).isEqualTo("00000000024");
        assertThat(s.value("CARDSID")).isEqualTo(SEED[9][0]);
        assertThat(rows(s)).isEqualTo(seedRows(1, 7));
    }

    @Test
    void pgmReenterFromAnotherProgramRestartsTheList() {
        // WHEN CDEMO-PGM-REENTER AND CDEMO-FROM-PROGRAM NOT = LIT-THISPGM (lines 459-482)
        CardDemoCommarea in = new CardDemoCommarea();
        Cocrdlic.initialize(in, CardDemoCommarea.LAYOUT);
        in.setFromProgram("COACTVWC");
        in.setUserId("USER0001");
        in.setPgmContext(1);
        CicsResponse r = run(AidKey.ENTER, in.toBytes(), null);
        assertThat(rows(screen(r))).isEqualTo(seedRows(1, 7));
        assertThat(commarea(r).isPgmEnter()).isTrue();
        assertThat(commarea(r).userId()).isEqualTo(PicX.spaces(8));
    }

    // ------------------------------------------------------------------ file errors

    @Test
    void emptyFileAbendsAtEndbr() {
        // STARTBR NOTFND is not checked; READNEXT INVREQ (no browse) -> file error; ENDBR has no RESP -> AEIP
        carddat = KsdsFile.inMemory("CARDDAT", CardRecord.LENGTH, 0, 16);
        fct = new FileControl().register(carddat);
        CicsResponse r = first();
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.ABEND);
        assertThat(r.outcome().abendCode()).isEqualTo("AEIP");
        assertThat(task.wsErrorMsg).startsWith("File Error: READ     on CARDDAT   returned RESP 000000016 ,RESP2 000000041");
        assertThat(r.lastMap()).isNull();
    }

    @Test
    void disabledFileAbends() {
        carddat.setState(FileState.DISABLED);
        CicsResponse r = first();
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.ABEND);
        assertThat(task.wsErrorMsg).startsWith("File Error: READ     on CARDDAT   returned RESP 000000084 ,RESP2 000000050");
    }

    // ------------------------------------------------------------------ helpers / copybooks

    @ParameterizedTest
    @EnumSource(value = AidKey.class, names = {"CLEAR", "PA1", "PA2", "PA3", "PF1", "PF12", "PF24"})
    void otherKeysRefreshTheCurrentPage(AidKey aid) {
        CicsResponse r = next(goToPage(2), aid);
        assertThat(rows(screen(r))).isEqualTo(seedRows(8, 14));
        assertThat(screen(r).value("PAGENO")).isEqualTo("2  ");
    }

    @Test
    void commareaLayoutDescribesBothParts() {
        Map<String, Object> d = Cocrdlic.COMMAREA_LAYOUT.describe(com.carddemo.runtime.cobol.CobolRecord.ofBytes(
                first().outcome().commarea()));
        assertThat(Cocrdlic.COMMAREA_LAYOUT.length()).isEqualTo(2000);
        assertThat(d.get("CDEMO-FROM-PROGRAM")).isEqualTo("COCRDLIC");
        assertThat(d.get("WS-CA-FIRST-CARD-NUM")).isEqualTo(SEED[0][0]);
        assertThat(d.get("WS-ROW-CARD-NUM(7)")).isEqualTo(SEED[6][0]);
        assertThat(ThisProgCommarea.LENGTH).isEqualTo(254);
        assertThat(Cocrdlic.COMMAREA_LAYOUT.field("WS-THIS-PROGCOMMAREA").offset()).isEqualTo(160);
    }

    @Test
    void workAreaLayoutMatchesCvcrd01y() {
        assertThat(CardWorkArea.LAYOUT.length()).isEqualTo(5 + 8 + 7 + 7 + 75 + 75 + 11 + 16 + 9);
        assertThat(CardWorkArea.CC_ACCT_ID_N.offset()).isEqualTo(CardWorkArea.CC_ACCT_ID.offset());
    }
}
