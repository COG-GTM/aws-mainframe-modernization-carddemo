package com.carddemo.online.coactvwc;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.common.copybook.AccountRecord;
import com.carddemo.common.copybook.CardDemoCommarea;
import com.carddemo.common.copybook.CardDemoDatasets;
import com.carddemo.common.copybook.CardXrefRecord;
import com.carddemo.common.copybook.CustomerRecord;
import com.carddemo.runtime.bms.BmsScreen;
import com.carddemo.runtime.cics.AidKey;
import com.carddemo.runtime.cics.CicsContext;
import com.carddemo.runtime.cics.CicsDispatcher;
import com.carddemo.runtime.cics.CicsOutcome;
import com.carddemo.runtime.cics.CicsResponse;
import com.carddemo.runtime.cobol.PicX;
import com.carddemo.runtime.vsam.FileControl;
import com.carddemo.runtime.vsam.FileState;
import com.carddemo.runtime.vsam.KsdsFile;
import java.math.BigDecimal;
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
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Behavioural parity tests for COACTVWC. Line numbers refer to app/cbl/COACTVWC.cbl. Files are seeded from
 * app/data/ASCII: account 00000000001 (acctdata.txt line 1), customer 000000001 (custdata.txt line 1) and card
 * 9680294154603697 -> cust 000000001 / acct 00000000001 (cardxref.txt line 49). See docs/mapping/COACTVWC.md.
 */
class CoactvwcTest {

    /** 2024-03-05 14:07:09.25 UTC: CURDATE 03/05/24, CURTIME 14:07:09 (1100-SCREEN-INIT lines 431-456). */
    private static final Clock CLOCK = Clock.fixed(Instant.parse("2024-03-05T14:07:09.25Z"), ZoneOffset.UTC);

    private final Coactvwc program = new Coactvwc();
    private final KsdsFile acctdat = KsdsFile.inMemory("ACCTDAT", AccountRecord.LENGTH, 0, 11);
    private final KsdsFile custdat = KsdsFile.inMemory("CUSTDAT", CustomerRecord.LENGTH, 0, 9);
    private final KsdsFile ccxref = KsdsFile.inMemory("CCXREF", CardXrefRecord.LENGTH, 0, 16);
    private final FileControl fct = new FileControl().register(acctdat).register(custdat).register(ccxref)
            .register(ccxref.path("CXACAIX", 25, 11, false));

    CoactvwcTest() {
        acctdat.load(CardDemoDatasets.accounts());
        custdat.load(CardDemoDatasets.customers());
        ccxref.load(CardDemoDatasets.cardXrefs());
    }

    private CicsResponse run(AidKey aid, byte[] commarea, Map<String, String> fields) {
        return CicsDispatcher.run(program, CicsContext.builder().transid("CAVW").aid(aid).commarea(commarea)
                .terminalInput(fields).fileControl(fct).clock(CLOCK).build());
    }

    /** The commarea a first (EIBCALEN = 0) task returns: CDEMO-PGM-REENTER, 2000 bytes. */
    private byte[] reentryCommarea() {
        return run(AidKey.ENTER, null, null).outcome().commarea();
    }

    private CicsResponse enter(String acctsid) {
        Map<String, String> fields = new LinkedHashMap<>();
        if (acctsid != null) {
            fields.put("ACCTSID", acctsid);
        }
        return run(AidKey.ENTER, reentryCommarea(), fields);
    }

    private static CardDemoCommarea commarea(CicsResponse r) {
        return CardDemoCommarea.of(Arrays.copyOf(r.outcome().commarea(), CardDemoCommarea.LENGTH));
    }

    private static byte[] menuCommarea(String fromProgram, String fromTranid, String userType) {
        CardDemoCommarea c = new CardDemoCommarea();
        c.initializeSpaces();
        c.setFromProgram(fromProgram);
        c.setFromTranid(fromTranid);
        c.setUserId("ADMIN001");
        c.setUserType(userType);
        c.setPgmContext(0);
        CardDemoCommarea.ACCT_ID.set(c, 42);
        CardDemoCommarea.CUST_ID.set(c, 7);
        CardDemoCommarea.CARD_NUM.set(c, 0);
        return c.toBytes();
    }

    private static void assertReturnsToSelf(CicsResponse r) {
        // COMMON-RETURN lines 394-407: RETURN TRANSID('CAVW') COMMAREA(WS-COMMAREA) LENGTH(2000)
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.RETURN_TRANSID);
        assertThat(r.outcome().nextTransid()).isEqualTo("CAVW");
        assertThat(r.outcome().commarea()).hasSize(2000);
        // 1400-SEND-SCREEN line 581: SET CDEMO-PGM-REENTER TO TRUE
        assertThat(commarea(r).isPgmReenter()).isTrue();
    }

    private static void assertHeader(BmsScreen s) {
        assertThat(s.map()).isEqualTo("CACTVWA");
        assertThat(s.mapset()).isEqualTo("COACTVW");
        assertThat(s.value("TRNNAME")).isEqualTo("CAVW");
        assertThat(s.value("PGMNAME")).isEqualTo("COACTVWC");
        assertThat(s.value("TITLE01")).isEqualTo("      AWS Mainframe Modernization       ");
        assertThat(s.value("TITLE02")).isEqualTo("              CardDemo                  ");
        assertThat(s.value("CURDATE")).isEqualTo("03/05/24");
        assertThat(s.value("CURTIME")).isEqualTo("14:07:09");
        assertThat(s.erase()).isTrue();
        // 1300 lines 546-552: cursor always on ACCTSID
        assertThat(s.cursorField()).isEqualTo("ACCTSID");
    }

    private static String errmsg(String text) {
        return PicX.of(text, 78);
    }

    private static final String PROMPT = PicX.of("Enter or update id of account to display", 45);

    // ------------------------------------------------------------------ 0000-MAIN / first entry

    @Test
    void firstEntryShowsPromptAndEmptyScreen() {
        // lines 282-286 (EIBCALEN = 0: INITIALIZE commarea -> CDEMO-PGM-ENTER), 353-360, 1200 lines 462-463
        CicsResponse r = run(AidKey.ENTER, null, null);
        BmsScreen s = r.lastMap().screen();
        assertHeader(s);
        assertThat(s.value("INFOMSG")).isEqualTo(PROMPT);
        assertThat(s.value("ERRMSG")).isEqualTo(PicX.spaces(78));
        assertThat(s.field("ACCTSID").orElseThrow().sent()).isFalse();
        assertThat(s.field("ACCTSID").orElseThrow().attributeByte()).isEqualTo(Dfhbmsca.DFHBMFSE);
        assertThat(s.field("ACSFNAM").orElseThrow().sent()).isFalse();
        assertReturnsToSelf(r);
        CardDemoCommarea c = commarea(r);
        // INITIALIZE CARDDEMO-COMMAREA: alphanumerics SPACES, numerics ZERO
        assertThat(c.fromProgram()).isEqualTo(PicX.spaces(8));
        assertThat(CardDemoCommarea.ACCT_ID.raw(c)).isEqualTo("00000000000");
        // WS-COMMAREA bytes 161-172 = INITIALIZEd WS-THIS-PROGCOMMAREA (spaces); 173-2000 spaces
        assertThat(PicX.isSpaces(PicX.fromBytes(Arrays.copyOfRange(r.outcome().commarea(), 160, 2000)))).isTrue();
        assertThat(s.render().get(4)).contains("Account Number :");
        assertThat(s.render().get(21)).contains("Enter or update id of account to display");
    }

    @Test
    void entryFromMenuKeepsTheIncomingCommarea() {
        // lines 282-292: the COMEN01C test reads CARDDEMO-COMMAREA before DFHCOMMAREA is moved into it
        // (uninitialised = LOW-VALUES), so it is never true and the menu's commarea is copied, not INITIALIZEd.
        CicsResponse r = run(AidKey.ENTER, menuCommarea("COMEN01C", "CM00", "A"), null);
        BmsScreen s = r.lastMap().screen();
        assertHeader(s);
        // FLG-ACCTFILTER-BLANK (INITIALIZEd) -> LOW-VALUES TO ACCTSIDO (line 466); PGM-ENTER -> no '*'
        assertThat(s.field("ACCTSID").orElseThrow().sent()).isFalse();
        assertThat(s.value("INFOMSG")).isEqualTo(PROMPT);
        assertReturnsToSelf(r);
        CardDemoCommarea c = commarea(r);
        assertThat(c.userId()).isEqualTo("ADMIN001");
        assertThat(c.fromProgram()).isEqualTo("COMEN01C");
        assertThat(CardDemoCommarea.ACCT_ID.getLong(c)).isEqualTo(42);
        assertThat(CardDemoCommarea.CUST_ID.getLong(c)).isEqualTo(7);
        // DFHCOMMAREA(161:12) is beyond the 160-byte EIBCALEN: padded with spaces here (see LIMITATIONS)
        assertThat(PicX.fromBytes(Arrays.copyOfRange(r.outcome().commarea(), 160, 172))).isEqualTo(PicX.spaces(12));
    }

    @Test
    void ownCommareaTailRoundTrips() {
        byte[] in = reentryCommarea();
        byte[] tail = PicX.toBytes("COACTVWCCAVW");
        System.arraycopy(tail, 0, in, 160, 12);
        CicsResponse r = run(AidKey.ENTER, in, Map.of("ACCTSID", "00000000001"));
        assertThat(PicX.fromBytes(Arrays.copyOfRange(r.outcome().commarea(), 160, 172))).isEqualTo("COACTVWCCAVW");
    }

    @Test
    void unexpectedProgramContextSendsPlainText() {
        // lines 375-382: CDEMO-PGM-CONTEXT neither 0 nor 1 (here LOW-VALUE) -> SEND-PLAIN-TEXT, RETURN
        CicsResponse r = run(AidKey.ENTER, new byte[160], null);
        assertThat(r.lastMap()).isNull();
        assertThat(r.lastText().text()).isEqualTo(PicX.of("UNEXPECTED DATA SCENARIO", 75));
        assertThat(r.lastText().erase()).isTrue();
        assertThat(r.lastText().freekb()).isTrue();
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.RETURN);
        assertThat(r.outcome().commarea()).isNull();
    }

    // ------------------------------------------------------------------ PF keys

    @ParameterizedTest
    @EnumSource(value = AidKey.class, names = {"PF3", "PF15"})
    void pf3ReturnsToCallingProgram(AidKey aid) {
        // lines 324-352 (+ CSSTRPFY: PF15 folds to PFK03)
        CicsResponse r = run(aid, menuCommarea("COADM01C", "CA00", "A"), null);
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.XCTL);
        assertThat(r.outcome().nextProgram()).isEqualTo("COADM01C");
        assertThat(r.outcome().commarea()).hasSize(160);
        CardDemoCommarea c = commarea(r);
        assertThat(c.toProgram()).isEqualTo("COADM01C");
        assertThat(c.toTranid()).isEqualTo("CA00");
        assertThat(c.fromProgram()).isEqualTo("COACTVWC");
        assertThat(c.fromTranid()).isEqualTo("CAVW");
        // line 344: SET CDEMO-USRTYP-USER even for an admin user (bug-for-bug)
        assertThat(c.userType()).isEqualTo("U");
        assertThat(c.isPgmEnter()).isTrue();
        assertThat(c.lastMapset()).isEqualTo("COACTVW");
        assertThat(c.lastMap()).isEqualTo("CACTVWA");
        assertThat(r.terminalOutputs()).isEmpty();
    }

    @Test
    void pf3WithoutCallerGoesToMainMenu() {
        // lines 328-338: FROM-TRANID / FROM-PROGRAM spaces (INITIALIZEd on EIBCALEN = 0) -> CM00 / COMEN01C
        CicsResponse r = run(AidKey.PF3, null, null);
        assertThat(r.outcome().nextProgram()).isEqualTo("COMEN01C");
        assertThat(commarea(r).toTranid()).isEqualTo("CM00");

        byte[] lowCaller = menuCommarea("COMEN01C", "CM00", "U");
        Arrays.fill(lowCaller, 0, 12, (byte) 0);
        CicsResponse r2 = run(AidKey.PF3, lowCaller, null);
        assertThat(r2.outcome().nextProgram()).isEqualTo("COMEN01C");
        assertThat(commarea(r2).toTranid()).isEqualTo("CM00");
    }

    @ParameterizedTest
    @EnumSource(value = AidKey.class, names = {"ENTER", "PF3", "PF15"}, mode = EnumSource.Mode.EXCLUDE)
    void anyOtherKeyActsAsEnter(AidKey aid) {
        // lines 306-314: PFK-INVALID -> SET CCARD-AID-ENTER; re-entry then processes the input normally
        CicsResponse r = run(aid, reentryCommarea(), Map.of("ACCTSID", "00000000001"));
        BmsScreen s = r.lastMap().screen();
        assertThat(s.value("ERRMSG")).isEqualTo(PicX.spaces(78));
        assertThat(s.value("ACSFNAM")).isEqualTo(PicX.of("Immanuel", 25));
        assertReturnsToSelf(r);
    }

    // ------------------------------------------------------------------ successful view

    @Test
    void validAccountShowsAccountAndCustomer() {
        // 9000-READ-ACCT: CXACAIX 00000000001 -> xref line 49; ACCTDAT line 1; CUSTDAT line 1. 1200 lines 469-526
        CicsResponse r = enter("00000000001");
        BmsScreen s = r.lastMap().screen();
        assertHeader(s);
        assertThat(s.value("ACCTSID")).isEqualTo("00000000001");
        assertThat(s.value("ACSTTUS")).isEqualTo("Y");
        // S9(10)V99 -> PIC +ZZZ,ZZZ,ZZZ.99: 00000001940{ = +194.00, 00000020200{ = +2020.00, ...
        assertThat(s.value("ACURBAL")).isEqualTo("+        194.00");
        assertThat(s.value("ACRDLIM")).isEqualTo("+      2,020.00");
        assertThat(s.value("ACSHLIM")).isEqualTo("+      1,020.00");
        assertThat(s.value("ACRCYCR")).isEqualTo("+           .00");
        assertThat(s.value("ACRCYDB")).isEqualTo("+           .00");
        assertThat(s.value("ADTOPEN")).isEqualTo("2014-11-20");
        assertThat(s.value("AEXPDT")).isEqualTo("2025-05-20");
        assertThat(s.value("AREISDT")).isEqualTo("2025-05-20");
        assertThat(s.value("AADDGRP")).isEqualTo(PicX.spaces(10));
        assertThat(s.value("ACSTNUM")).isEqualTo("000000001");
        // STRING CUST-SSN(1:3) '-' (4:2) '-' (6:4): 020973888 -> 020-97-3888 (12th byte LOW-VALUE, shown blank)
        assertThat(s.value("ACSTSSN")).isEqualTo("020-97-3888 ");
        assertThat(s.value("ACSTFCO")).isEqualTo("274");
        assertThat(s.value("ACSTDOB")).isEqualTo("1961-06-08");
        assertThat(s.value("ACSFNAM")).isEqualTo(PicX.of("Immanuel", 25));
        assertThat(s.value("ACSMNAM")).isEqualTo(PicX.of("Madeline", 25));
        assertThat(s.value("ACSLNAM")).isEqualTo(PicX.of("Kessler", 25));
        assertThat(s.value("ACSADL1")).isEqualTo(PicX.of("618 Deshaun Route", 50));
        assertThat(s.value("ACSADL2")).isEqualTo(PicX.of("Apt. 802", 50));
        assertThat(s.value("ACSCITY")).isEqualTo(PicX.of("Altenwerthshire", 50));
        assertThat(s.value("ACSSTTE")).isEqualTo("NC");
        // CUST-ADDR-ZIP X(10) -> ACSZIPCO X(5); CUST-PHONE-NUM-1 X(15) -> ACSPHN1O X(13): truncated
        assertThat(s.value("ACSZIPC")).isEqualTo("12546");
        assertThat(s.value("ACSCTRY")).isEqualTo("USA");
        assertThat(s.value("ACSPHN1")).isEqualTo("(908)119-8310");
        assertThat(s.value("ACSPHN2")).isEqualTo("(373)693-8684");
        assertThat(s.value("ACSGOVT")).isEqualTo("00000000000049368437");
        assertThat(s.value("ACSEFTC")).isEqualTo("0053581756");
        assertThat(s.value("ACSPFLG")).isEqualTo("Y");
        assertThat(s.value("ERRMSG")).isEqualTo(PicX.spaces(78));
        // WS-INFORM-OUTPUT is never SET: 9000 clears WS-INFO-MSG, 1200 lines 528-530 restore the prompt
        assertThat(s.value("INFOMSG")).isEqualTo(PROMPT);
        assertReturnsToSelf(r);
        CardDemoCommarea c = commarea(r);
        assertThat(CardDemoCommarea.ACCT_ID.raw(c)).isEqualTo("00000000001");
        assertThat(CardDemoCommarea.CUST_ID.raw(c)).isEqualTo("000000001");
        assertThat(CardDemoCommarea.CARD_NUM.raw(c)).isEqualTo("9680294154603697");
        assertThat(s.render().get(11)).contains("Customer id  :", "000000001", "020-97-3888");
    }

    @Test
    void validAccountScreenAttributes() {
        Coactvwc.Task t = runTask(AidKey.ENTER, reentryCommarea(), Map.of("ACCTSID", "00000000001"));
        assertThat(t.cactvwa.color("ACCTSID")).isEqualTo(Dfhbmsca.DFHDFCOL);
        assertThat(t.cactvwa.color("INFOMSG")).isEqualTo(Dfhbmsca.DFHNEUTR);
        assertThat(t.cactvwa.length("ACCTSID")).isEqualTo(-1);
        assertThat(t.wsEditAcctFlag).isEqualTo('1');
        assertThat(t.foundAcctInMaster()).isTrue();
        assertThat(t.foundCustInMaster()).isTrue();
        assertThat(t.ccardNextMap).isEqualTo("CACTVWA");
        assertThat(t.ccardNextProg).isEqualTo("COACTVWC");
    }

    // ------------------------------------------------------------------ input edits (2200 / 2210)

    @ParameterizedTest
    @ValueSource(strings = {"*", " ", "           "})
    void blankOrStarInputAsksForInput(String input) {
        // 2200 lines 628-633 ('*'/SPACES -> LOW-VALUES), 2210 lines 653-662 ('Account number not provided'),
        // then 2200 lines 640-642 overwrite it with 'No input received'. 1300 lines 561-565: '*' in red.
        CicsResponse r = enter(input);
        assertBlankInput(r);
    }

    @Test
    void noFieldsTransmittedIsMapfailAndBlank() {
        // RECEIVE MAP MAPFAIL: ACCTSIDI keeps LOW-VALUES -> moved to CC-ACCT-ID -> blank
        CicsResponse r = run(AidKey.ENTER, reentryCommarea(), null);
        assertBlankInput(r);
    }

    private void assertBlankInput(CicsResponse r) {
        BmsScreen s = r.lastMap().screen();
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("No input received"));
        assertThat(s.value("ACCTSID")).isEqualTo("*          ");
        assertThat(s.value("INFOMSG")).isEqualTo(PROMPT);
        assertThat(s.field("ACSFNAM").orElseThrow().sent()).isFalse();
        assertReturnsToSelf(r);
        assertThat(CardDemoCommarea.ACCT_ID.raw(commarea(r))).isEqualTo("00000000000");
    }

    @Test
    void blankInputColours() {
        Coactvwc.Task t = runTask(AidKey.ENTER, reentryCommarea(), Map.of("ACCTSID", "*"));
        assertThat(t.flgAcctfilterBlank()).isTrue();
        assertThat(t.inputError()).isTrue();
        assertThat(t.cactvwa.color("ACCTSID")).isEqualTo(Dfhbmsca.DFHRED);
    }

    @ParameterizedTest
    @ValueSource(strings = {"abc", "123", "0000000000A", "00000000000", "-0000000001", "1234567890 "})
    void nonNumericOrZeroAccountIsRejected(String input) {
        // 2210 lines 666-676: literal 'Account Filter must  be a non-zero 11 digit number' (two spaces);
        // 1200 line 468 echoes CC-ACCT-ID, 1300 lines 557-559 colours it red; CDEMO-ACCT-ID = 0
        CicsResponse r = enter(input);
        BmsScreen s = r.lastMap().screen();
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("Account Filter must  be a non-zero 11 digit number"));
        assertThat(s.value("ACCTSID")).isEqualTo(PicX.of(input, 11));
        assertThat(s.field("ACSTTUS").orElseThrow().sent()).isFalse();
        assertReturnsToSelf(r);
        assertThat(CardDemoCommarea.ACCT_ID.raw(commarea(r))).isEqualTo("00000000000");
    }

    @Test
    void inputLongerThanFieldIsTruncated() {
        // ACCTSID LENGTH=11: the 12th character never reaches the program
        CicsResponse r = enter("000000000012");
        assertThat(r.lastMap().screen().value("ACCTSID")).isEqualTo("00000000001");
        assertThat(r.lastMap().screen().value("ERRMSG")).isEqualTo(PicX.spaces(78));
    }

    // ------------------------------------------------------------------ NOTFND paths

    @Test
    void accountNotInXref() {
        // 9200 lines 741-757: STRING ... INTO WS-RETURN-MSG X(75); the 81-character result is truncated.
        // ERROR-RESP = MOVE 13 (S9(9) COMP) TO X(10) = '000000013 ', ERROR-RESP2 = '000000080 '
        CicsResponse r = enter("99999999999");
        BmsScreen s = r.lastMap().screen();
        assertThat(s.value("ERRMSG"))
                .isEqualTo(errmsg("Account:99999999999 not found in Cross ref file.  Resp:000000013  Reas:0000"));
        assertThat(s.value("ACCTSID")).isEqualTo("99999999999");
        assertThat(s.field("ACSTTUS").orElseThrow().sent()).isFalse();
        assertThat(s.field("ACSTNUM").orElseThrow().sent()).isFalse();
        assertReturnsToSelf(r);
    }

    @Test
    void accountInXrefButNotInAcctdatStillReadsCustomer() {
        // 9300 NOTFND lines 789-806. DID-NOT-FIND-ACCT-IN-ACCTDAT is never set (line 792 commented out) so
        // 9000 line 704 does not exit: 9400 reads customer 000000001, FOUND-CUST-IN-MASTER shows the
        // never-read ACCOUNT-RECORD (LOW-VALUES: amounts edit as zero, alphanumerics blank).
        ccxref.write(PicX.of("1111222233334444" + "000000001" + "00000000099", 50));
        CicsResponse r = enter("00000000099");
        BmsScreen s = r.lastMap().screen();
        assertThat(s.value("ERRMSG"))
                .isEqualTo(errmsg("Account:00000000099 not found in Acct Master file.Resp:000000013  Reas:0000"));
        assertThat(s.value("ACURBAL")).isEqualTo("+           .00");
        assertThat(s.value("ACSTTUS")).isEqualTo(" ");
        assertThat(s.value("ADTOPEN")).isEqualTo(PicX.spaces(10));
        assertThat(s.value("ACSFNAM")).isEqualTo(PicX.of("Immanuel", 25));
        assertThat(s.value("ACCTSID")).isEqualTo("00000000099");
        assertReturnsToSelf(r);
        assertThat(CardDemoCommarea.CARD_NUM.raw(commarea(r))).isEqualTo("1111222233334444");
    }

    @Test
    void customerNotFound() {
        // 9400 NOTFND lines 839-856: 'CustId:' + 9 + ' not found' + ' in customer master.Resp: ' + ... (truncated)
        ccxref.write(PicX.of("1111222233334444" + "999999999" + "00000000002", 50));
        ccxref.delete(findXrefCardFor("00000000002"));
        CicsResponse r = enter("00000000002");
        BmsScreen s = r.lastMap().screen();
        assertThat(s.value("ERRMSG"))
                .isEqualTo(errmsg("CustId:999999999 not found in customer master.Resp: 000000013  REAS:0000000"));
        // FOUND-ACCT-IN-MASTER: account part is shown, customer part is not
        assertThat(s.value("ACSTTUS")).isEqualTo("Y");
        assertThat(s.value("ADTOPEN")).isEqualTo("2013-06-19");
        assertThat(s.field("ACSTNUM").orElseThrow().sent()).isFalse();
        assertThat(s.field("ACSFNAM").orElseThrow().sent()).isFalse();
        assertReturnsToSelf(r);
    }

    @Test
    void accountAndCustomerBothMissingKeepsFirstMessage() {
        // 9400 line 845: the STRING is guarded by WS-RETURN-MSG-OFF, so the ACCTDAT message stays
        ccxref.write(PicX.of("1111222233334444" + "999999999" + "00000000099", 50));
        CicsResponse r = enter("00000000099");
        BmsScreen s = r.lastMap().screen();
        assertThat(s.value("ERRMSG"))
                .isEqualTo(errmsg("Account:00000000099 not found in Acct Master file.Resp:000000013  Reas:0000"));
        assertThat(s.field("ACSTTUS").orElseThrow().sent()).isFalse();
    }

    // ------------------------------------------------------------------ other RESP codes

    @Test
    void duplicateAlternateKeyIsAFileError() {
        // CXACAIX is NONUNIQUEKEY: two cards on one account -> READ returns DUPKEY (15) -> WHEN OTHER
        // (lines 759-767): WS-FILE-ERROR-MESSAGE (80 bytes) truncated to 75
        ccxref.write(PicX.of("0000000000000001" + "000000001" + "00000000001", 50));
        CicsResponse r = enter("00000000001");
        BmsScreen s = r.lastMap().screen();
        assertThat(s.value("ERRMSG"))
                .isEqualTo(errmsg("File Error: READ     on CXACAIX   returned RESP 000000015 ,RESP2 000000000"));
        assertThat(s.field("ACSTTUS").orElseThrow().sent()).isFalse();
        assertReturnsToSelf(r);
    }

    @Test
    void acctdatDisabledShowsFileErrorAndStillReadsCustomer() {
        // 9300 WHEN OTHER lines 809-816 (DISABLED = 84, RESP2 50); 9000 line 704 does not test it -> 9400 runs
        acctdat.setState(FileState.DISABLED);
        CicsResponse r = enter("00000000001");
        BmsScreen s = r.lastMap().screen();
        assertThat(s.value("ERRMSG"))
                .isEqualTo(errmsg("File Error: READ     on ACCTDAT   returned RESP 000000084 ,RESP2 000000050"));
        assertThat(s.value("ACSFNAM")).isEqualTo(PicX.of("Immanuel", 25));
        assertThat(s.value("ACRDLIM")).isEqualTo("+           .00");
    }

    @Test
    void custdatDisabledOverwritesMessage() {
        // 9400 WHEN OTHER lines 858-865 is not guarded by WS-RETURN-MSG-OFF
        custdat.setState(FileState.DISABLED);
        CicsResponse r = enter("00000000001");
        BmsScreen s = r.lastMap().screen();
        assertThat(s.value("ERRMSG"))
                .isEqualTo(errmsg("File Error: READ     on CUSTDAT   returned RESP 000000084 ,RESP2 000000050"));
        assertThat(s.value("ACURBAL")).isEqualTo("+        194.00");
        assertThat(s.field("ACSFNAM").orElseThrow().sent()).isFalse();
    }

    @Test
    void xrefFileNotDefinedIsAFileError() {
        FileControl noAix = new FileControl().register(acctdat).register(custdat);
        CicsResponse r = CicsDispatcher.run(program, CicsContext.builder().transid("CAVW").aid(AidKey.ENTER)
                .commarea(reentryCommarea()).terminalInput(Map.of("ACCTSID", "00000000001")).fileControl(noAix)
                .clock(CLOCK).build());
        assertThat(r.lastMap().screen().value("ERRMSG"))
                .isEqualTo(errmsg("File Error: READ     on CXACAIX   returned RESP 000000012 ,RESP2 000000001"));
    }

    // ------------------------------------------------------------------ ABEND-ROUTINE

    @Test
    void invalidNumericDataAbends9999() {
        // ACCT-CURR-BAL not numeric -> data exception in 1200 MOVE -> HANDLE ABEND -> ABEND-ROUTINE (916-937).
        // ABEND-MSG has VALUE SPACES (CSMSG02Y), never LOW-VALUES, so the 'UNEXPECTED ABEND' text is not used.
        String rec = acctdat.read("00000000001").dataString();
        acctdat.readForUpdate("00000000001");
        acctdat.rewrite(rec.substring(0, 12) + "XXXXXXXXXXXX" + rec.substring(24));
        CicsResponse r = enter("00000000001");
        assertThat(r.lastMap()).isNull();
        assertThat(r.lastText().text()).isEqualTo(PicX.of("    COACTVWC", 134));
        assertThat(r.lastText().erase()).isFalse();
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.ABEND);
        assertThat(r.outcome().abendCode()).isEqualTo("9999");
    }

    @Test
    void unknownTerminalFieldIsARequestErrorNotAnAbend() {
        org.assertj.core.api.Assertions.assertThatThrownBy(
                () -> run(AidKey.ENTER, reentryCommarea(), Map.of("UNKNOWN", "x")))
                .isExactlyInstanceOf(IllegalArgumentException.class).hasMessageContaining("UNKNOWN");
    }

    // ------------------------------------------------------------------ helpers / unit

    @Test
    void respTextIsNineDigitsUnsigned() {
        assertThat(Coactvwc.respText(13)).isEqualTo("000000013 ");
        assertThat(Coactvwc.respText(-5)).isEqualTo("000000005 ");
        assertThat(Coactvwc.respText(0)).isEqualTo("000000000 ");
    }

    @Test
    void numericClassTest() {
        assertThat(Coactvwc.isNumericText("01234567890")).isTrue();
        assertThat(Coactvwc.isNumericText("0123456789 ")).isFalse();
        assertThat(Coactvwc.isNumericText(PicX.lowValues(11))).isFalse();
    }

    @Test
    void amountEditing() {
        AccountRecord a = new AccountRecord();
        AccountRecord.CURR_BAL.set(a, new BigDecimal("-1234.56"));
        assertThat(Coactvwc.editAmount(AccountRecord.CURR_BAL, a)).isEqualTo("-      1,234.56");
        // S9(10)V99 has 10 integer digits, the PICOUT only 9: the high-order digit is lost
        AccountRecord.CURR_BAL.set(a, new BigDecimal("9876543210.99"));
        assertThat(Coactvwc.editAmount(AccountRecord.CURR_BAL, a)).isEqualTo("+876,543,210.99");
        assertThat(Coactvwc.editAmount(AccountRecord.CREDIT_LIMIT, new AccountRecord())).isEqualTo("+           .00");
    }

    @Test
    void initializeSetsSpacesAndZeros() {
        CardDemoCommarea c = new CardDemoCommarea();
        Coactvwc.initialize(CardDemoCommarea.LAYOUT, c);
        assertThat(c.userId()).isEqualTo(PicX.spaces(8));
        assertThat(c.isPgmEnter()).isTrue();
        assertThat(CardDemoCommarea.CARD_NUM.raw(c)).isEqualTo("0".repeat(16));
    }

    @Test
    void seedHasOneXrefPerAccount() {
        // Precondition for the DUPKEY test above: the shipped data never triggers DUPKEY
        List<String> accts = CardDemoDatasets.cardXrefs().stream().map(x -> x.substring(25, 36)).toList();
        assertThat(accts).doesNotHaveDuplicates().hasSize(50);
    }

    private String findXrefCardFor(String acctId) {
        return CardDemoDatasets.cardXrefs().stream().filter(x -> x.substring(25, 36).equals(acctId))
                .map(x -> x.substring(0, 16)).findFirst().orElseThrow();
    }

    /** Runs one task and returns its WORKING-STORAGE for white-box checks. */
    private Coactvwc.Task runTask(AidKey aid, byte[] commarea, Map<String, String> fields) {
        CicsContext ctx = CicsContext.builder().transid("CAVW").aid(aid).commarea(commarea).terminalInput(fields)
                .fileControl(fct).clock(CLOCK).build();
        Coactvwc.Task t = new Coactvwc.Task(ctx);
        try {
            t.main();
        } catch (RuntimeException expected) {
            // RETURN TRANSID ends the task by throwing
        }
        return t;
    }
}
