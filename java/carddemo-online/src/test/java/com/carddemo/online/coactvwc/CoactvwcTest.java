package com.carddemo.online.coactvwc;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.carddemo.common.copybook.AccountRecord;
import com.carddemo.common.copybook.AsciiSeedData;
import com.carddemo.common.copybook.CardDemoCommarea;
import com.carddemo.common.copybook.CardXrefRecord;
import com.carddemo.common.copybook.CustomerRecord;
import com.carddemo.runtime.bms.BmsScreen;
import com.carddemo.runtime.cics.AidKey;
import com.carddemo.runtime.cics.CicsContext;
import com.carddemo.runtime.cics.CicsControlTransfer;
import com.carddemo.runtime.cics.CicsDispatcher;
import com.carddemo.runtime.cics.CicsOutcome;
import com.carddemo.runtime.cics.CicsResponse;
import com.carddemo.runtime.cics.SystemInfo;
import com.carddemo.runtime.cobol.PicX;
import com.carddemo.runtime.vsam.FileControl;
import com.carddemo.runtime.vsam.FileState;
import com.carddemo.runtime.vsam.KsdsFile;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.Arrays;
import java.util.Map;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Behavioural parity tests for COACTVWC. Line numbers refer to app/cbl/COACTVWC.cbl. Files are the ASCII
 * seeds: account 00000000001 = acctdata.txt line 1, its xref = cardxref.txt line 49 (card 9680294154603697,
 * customer 000000001), customer 000000001 = custdata.txt line 1.
 */
class CoactvwcTest {

    /** 2024-03-05 14:07:09.25 UTC: CURDATE 03/05/24, CURTIME 14:07:09 (1100-SCREEN-INIT lines 433-456). */
    private static final Clock CLOCK = Clock.fixed(Instant.parse("2024-03-05T14:07:09.25Z"), ZoneOffset.UTC);

    private final Coactvwc program = new Coactvwc();
    private final KsdsFile acctdat = KsdsFile.inMemory("ACCTDAT", AccountRecord.LENGTH, 0, 11);
    private final KsdsFile custdat = KsdsFile.inMemory("CUSTDAT", CustomerRecord.LENGTH, 0, 9);
    private final KsdsFile ccxref = KsdsFile.inMemory("CCXREF", CardXrefRecord.LENGTH, 0, 16);
    private final FileControl fct = new FileControl().register(acctdat).register(custdat).register(ccxref)
            .register(ccxref.path("CXACAIX", CardXrefRecord.ACCT_AIX_OFFSET, CardXrefRecord.ACCT_AIX_LENGTH, false));

    CoactvwcTest() {
        acctdat.load(AsciiSeedData.records(AsciiSeedData.ACCTDATA, AccountRecord.LENGTH));
        custdat.load(AsciiSeedData.records(AsciiSeedData.CUSTDATA, CustomerRecord.LENGTH));
        ccxref.load(AsciiSeedData.records(AsciiSeedData.CARDXREF, CardXrefRecord.LENGTH));
    }

    private CicsContext ctx(AidKey aid, byte[] commarea, Map<String, String> fields) {
        return CicsContext.builder().transid("CAVW").aid(aid).commarea(commarea).terminalInput(fields)
                .fileControl(fct).clock(CLOCK).systemInfo(SystemInfo.defaults()).build();
    }

    private CicsResponse run(AidKey aid, byte[] commarea, Map<String, String> fields) {
        return CicsDispatcher.run(program, ctx(aid, commarea, fields));
    }

    /** Runs a task directly so its WORKING-STORAGE (colours, flags) can be inspected afterwards. */
    private Coactvwc.Task task(AidKey aid, byte[] commarea, Map<String, String> fields) {
        Coactvwc.Task t = new Coactvwc.Task(ctx(aid, commarea, fields));
        assertThatThrownBy(t::main0000).isInstanceOf(CicsControlTransfer.class);
        return t;
    }

    /** CARDDEMO-COMMAREA as COMEN01C would pass it (user USER0001, context 0 = enter, or 1 = re-enter). */
    private static byte[] commarea(int context) {
        CardDemoCommarea c = new CardDemoCommarea();
        c.initializeSpaces();
        c.setFromTranid("CM00");
        c.setFromProgram("COMEN01C");
        c.setUserId("USER0001");
        c.setUserType("A");
        CardDemoCommarea.CUST_ID.set(c, 0L);
        CardDemoCommarea.ACCT_ID.set(c, 0L);
        CardDemoCommarea.CARD_NUM.set(c, 0L);
        c.setPgmContext(context);
        return c.toBytes();
    }

    private CicsResponse view(String acctsid) {
        return run(AidKey.ENTER, commarea(1), acctsid == null ? Map.of() : Map.of("ACCTSID", acctsid));
    }

    private static CardDemoCommarea commareaOut(CicsResponse r) {
        return CardDemoCommarea.of(Arrays.copyOf(r.outcome().commarea(), CardDemoCommarea.LENGTH));
    }

    private static void assertReturnsToSelf(CicsResponse r) {
        // COMMON-RETURN lines 394-404: RETURN TRANSID(CAVW) COMMAREA(WS-COMMAREA) LENGTH(2000)
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.RETURN_TRANSID);
        assertThat(r.outcome().nextTransid()).isEqualTo("CAVW");
        assertThat(r.outcome().commarea()).hasSize(2000);
        // 1400-SEND-SCREEN line 581: SET CDEMO-PGM-REENTER
        assertThat(commareaOut(r).isPgmReenter()).isTrue();
    }

    private static void assertHeader(BmsScreen s) {
        assertThat(s.map()).isEqualTo("CACTVWA");
        assertThat(s.value("TRNNAME")).isEqualTo("CAVW");
        assertThat(s.value("PGMNAME")).isEqualTo("COACTVWC");
        assertThat(s.value("TITLE01")).isEqualTo("      AWS Mainframe Modernization       ");
        assertThat(s.value("CURDATE")).isEqualTo("03/05/24");
        assertThat(s.value("CURTIME")).isEqualTo("14:07:09");
        // 1300 lines 546-553: every branch moves -1 to ACCTSIDL -> cursor on the account id
        assertThat(s.cursorField()).isEqualTo("ACCTSID");
    }

    private static String err(CicsResponse r) {
        return r.lastMap().screen().value("ERRMSG");
    }

    private static String msg(String text) {
        return PicX.of(text, 78);
    }

    // ---- 0000-MAIN: first entry ---------------------------------------------------------------------

    @Test
    void firstEntryWithoutCommareaShowsEmptyScreenWithPrompt() {
        CicsResponse r = run(AidKey.ENTER, new byte[0], Map.of());
        assertReturnsToSelf(r);
        BmsScreen s = r.lastMap().screen();
        assertHeader(s);
        // 1200 line 462-463: EIBCALEN = 0 -> WS-PROMPT-FOR-INPUT; ACCTSIDO stays LOW-VALUES from 1100
        assertThat(s.value("INFOMSG")).isEqualTo(PicX.of("Enter or update id of account to display", 45));
        assertThat(s.field("ACCTSID").orElseThrow().sent()).isFalse();
        assertThat(err(r)).isEqualTo(msg(""));
        CardDemoCommarea out = commareaOut(r);
        // INITIALIZE CARDDEMO-COMMAREA (line 285): spaces / zeros
        assertThat(out.fromProgram()).isEqualTo("        ");
        assertThat(CardDemoCommarea.ACCT_ID.getLong(out)).isZero();
        // WS-THIS-PROGCOMMAREA initialized to spaces, rest of WS-COMMAREA spaces
        assertThat(PicX.fromBytes(r.outcome().commarea()).substring(160)).isBlank();
    }

    @Test
    void enterFromMenuShowsScreenAndKeepsCommarea() {
        CicsResponse r = run(AidKey.ENTER, commarea(0), Map.of());
        assertReturnsToSelf(r);
        CardDemoCommarea out = commareaOut(r);
        assertThat(out.userId()).isEqualTo("USER0001");
        assertThat(out.fromProgram()).isEqualTo("COMEN01C");
        assertThat(r.lastMap().screen().value("INFOMSG"))
                .isEqualTo(PicX.of("Enter or update id of account to display", 45));
    }

    @Test
    void firstEntryAttributes() {
        Coactvwc.Task t = task(AidKey.ENTER, commarea(0), Map.of());
        // 1300 lines 542-575: FSET attribute, default colour (blank filter, not re-enter), INFOMSG neutral
        assertThat(t.cactvwa.flag("ACCTSID")).isEqualTo(Coactvwc.DFHBMFSE);
        assertThat(t.cactvwa.color("ACCTSID")).isEqualTo(Coactvwc.DFHDFCOL);
        assertThat(t.cactvwa.color("INFOMSG")).isEqualTo(Coactvwc.DFHNEUTR);
    }

    // ---- PF keys (YYYY-STORE-PFKEY + lines 300-344) -------------------------------------------------

    @ParameterizedTest
    @EnumSource(value = AidKey.class, names = {"PF3", "PF15"})
    void pf3ReturnsToCallingProgram(AidKey aid) {
        CicsResponse r = run(aid, commarea(1), Map.of());
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.XCTL);
        assertThat(r.outcome().nextProgram()).isEqualTo("COMEN01C");
        assertThat(r.outcome().commarea()).hasSize(160);
        CardDemoCommarea out = commareaOut(r);
        assertThat(out.toTranid()).isEqualTo("CM00");
        assertThat(out.toProgram()).isEqualTo("COMEN01C");
        assertThat(out.fromTranid()).isEqualTo("CAVW");
        assertThat(out.fromProgram()).isEqualTo("COACTVWC");
        // Line 336: SET CDEMO-USRTYP-USER even though the caller was an admin ('A') - bug-for-bug
        assertThat(out.userType()).isEqualTo("U");
        assertThat(out.isPgmEnter()).isTrue();
        assertThat(out.lastMapset()).isEqualTo("COACTVW");
        assertThat(out.lastMap()).isEqualTo("CACTVWA");
        assertThat(r.terminalOutputs()).isEmpty();
    }

    @Test
    void pf3WithoutCallerGoesToMainMenu() {
        // EIBCALEN = 0: commarea initialized (FROM-TRANID/PROGRAM spaces) -> LIT-MENUTRANID / LIT-MENUPGM
        CicsResponse r = run(AidKey.PF3, new byte[0], Map.of());
        assertThat(r.outcome().nextProgram()).isEqualTo("COMEN01C");
        assertThat(commareaOut(r).toTranid()).isEqualTo("CM00");
    }

    @Test
    void pf3ReturnsToNonMenuCaller() {
        CardDemoCommarea c = CardDemoCommarea.of(commarea(1));
        c.setFromTranid("CCLI");
        c.setFromProgram("COCRDLIC");
        CicsResponse r = run(AidKey.PF3, c.toBytes(), Map.of());
        assertThat(r.outcome().nextProgram()).isEqualTo("COCRDLIC");
        assertThat(commareaOut(r).toTranid()).isEqualTo("CCLI");
    }

    @ParameterizedTest
    @EnumSource(value = AidKey.class, names = {"PF1", "PF7", "PF12", "PF24", "CLEAR", "PA1", "PA3"})
    void otherKeysActAsEnter(AidKey aid) {
        // Lines 303-309: PFK-INVALID -> SET CCARD-AID-ENTER
        CicsResponse r = run(aid, commarea(1), Map.of("ACCTSID", "00000000001"));
        assertReturnsToSelf(r);
        assertThat(r.lastMap().screen().value("ACSFNAM")).startsWith("Immanuel");
    }

    // ---- 2200-EDIT-MAP-INPUTS / 2210-EDIT-ACCOUNT ---------------------------------------------------

    @ParameterizedTest
    @ValueSource(strings = {"", "*", "           "})
    void blankAccountIdIsRejected(String input) {
        CicsResponse r = view(input);
        assertReturnsToSelf(r);
        // 2200 lines 629-633: '*'/SPACES -> LOW-VALUES; 2210 lines 653-661 set BLANK + WS-PROMPT-FOR-ACCT and
        // GO TO exit; 2200 lines 640-642 then SET NO-SEARCH-CRITERIA-RECEIVED, replacing the prompt.
        assertThat(err(r)).isEqualTo(msg("No input received"));
        assertThat(CardDemoCommarea.ACCT_ID.getLong(commareaOut(r))).isZero();
        // 1300 lines 562-566: BLANK + REENTER -> '*' in ACCTSIDO
        assertThat(r.lastMap().screen().value("ACCTSID")).isEqualTo("*          ");
        assertThat(r.lastMap().screen().field("ACCTSID").orElseThrow().color()).isEqualTo("RED");
    }

    @Test
    void notTransmittedAccountIdIsRejected() {
        assertThat(err(view(null))).isEqualTo(msg("No input received"));
    }

    @Test
    void blankAccountIdIsRedStar() {
        Coactvwc.Task t = task(AidKey.ENTER, commarea(1), Map.of("ACCTSID", "*"));
        // FLG-ACCTFILTER-BLANK survives 2210 (GO TO 2210-EDIT-ACCOUNT-EXIT, line 661)
        assertThat(t.wsEditAcctFlag).isEqualTo(" ");
        assertThat(t.cactvwa.color("ACCTSID")).isEqualTo(Coactvwc.DFHRED);
        assertThat(t.cactvwa.data("ACCTSID")).isEqualTo("*          ");
    }

    @ParameterizedTest
    @ValueSource(strings = {"abc", "123", "0000000000A", "00000000000", "-1", "1234567890 "})
    void nonNumericOrZeroAccountIdIsRejected(String input) {
        CicsResponse r = view(input);
        // 2210 lines 666-676 (literal has two spaces after 'must')
        assertThat(err(r)).isEqualTo(msg("Account Filter must  be a non-zero 11 digit number"));
        assertThat(CardDemoCommarea.ACCT_ID.getLong(commareaOut(r))).isZero();
        // 1200 line 467: CC-ACCT-ID is echoed back, 1300 line 558: in red
        assertThat(r.lastMap().screen().value("ACCTSID")).isEqualTo(PicX.of(input, 11));
        assertThat(task(AidKey.ENTER, commarea(1), Map.of("ACCTSID", input)).cactvwa.color("ACCTSID"))
                .isEqualTo(Coactvwc.DFHRED);
    }

    // ---- 9000-READ-ACCT and 1200-SETUP-SCREEN-VARS -----------------------------------------------------

    @Test
    void validAccountShowsAccountAndCustomer() {
        CicsResponse r = view("00000000001");
        assertReturnsToSelf(r);
        BmsScreen s = r.lastMap().screen();
        assertHeader(s);
        assertThat(err(r)).isEqualTo(msg(""));
        assertThat(s.value("INFOMSG")).isEqualTo(PicX.of("Enter or update id of account to display", 45));
        assertThat(s.value("ACCTSID")).isEqualTo("00000000001");
        // acctdata.txt line 1: Y 00000001940{ 00000020200{ 00000010200{ 2014-11-20 2025-05-20 2025-05-20 0{ 0{
        assertThat(s.value("ACSTTUS")).isEqualTo("Y");
        assertThat(s.value("ACURBAL")).isEqualTo("+        194.00");
        assertThat(s.value("ACRDLIM")).isEqualTo("+      2,020.00");
        assertThat(s.value("ACSHLIM")).isEqualTo("+      1,020.00");
        assertThat(s.value("ACRCYCR")).isEqualTo("+           .00");
        assertThat(s.value("ACRCYDB")).isEqualTo("+           .00");
        assertThat(s.value("ADTOPEN")).isEqualTo("2014-11-20");
        assertThat(s.value("AEXPDT")).isEqualTo("2025-05-20");
        assertThat(s.value("AREISDT")).isEqualTo("2025-05-20");
        assertThat(s.value("AADDGRP")).isEqualTo("          ");
        // custdata.txt line 1
        assertThat(s.value("ACSTNUM")).isEqualTo("000000001");
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
        assertThat(s.value("ACSZIPC")).isEqualTo("12546");
        assertThat(s.value("ACSCTRY")).isEqualTo("USA");
        assertThat(s.value("ACSPHN1")).isEqualTo("(908)119-8310");
        assertThat(s.value("ACSPHN2")).isEqualTo("(373)693-8684");
        assertThat(s.value("ACSGOVT")).isEqualTo("00000000000049368437");
        assertThat(s.value("ACSEFTC")).isEqualTo("0053581756");
        assertThat(s.value("ACSPFLG")).isEqualTo("Y");
        // 9200 lines 744-745: customer id and card number from the xref (cardxref.txt line 49)
        CardDemoCommarea out = commareaOut(r);
        assertThat(CardDemoCommarea.ACCT_ID.getLong(out)).isEqualTo(1L);
        assertThat(CardDemoCommarea.CUST_ID.getLong(out)).isEqualTo(1L);
        assertThat(CardDemoCommarea.CARD_NUM.raw(out)).isEqualTo("9680294154603697");
        assertThat(out.userId()).isEqualTo("USER0001");
    }

    @Test
    void validAccountAttributes() {
        Coactvwc.Task t = task(AidKey.ENTER, commarea(1), Map.of("ACCTSID", "00000000001"));
        assertThat(t.wsEditAcctFlag).isEqualTo("1");
        assertThat(t.foundAcctInMaster()).isTrue();
        assertThat(t.foundCustInMaster()).isTrue();
        assertThat(t.cactvwa.color("ACCTSID")).isEqualTo(Coactvwc.DFHDFCOL);
        assertThat(t.cactvwa.color("INFOMSG")).isEqualTo(Coactvwc.DFHNEUTR);
        // DFHDFCOL keeps the BMS COLOR=GREEN of ACCTSID; DFHNEUTR is rendered as NEUTRAL
        BmsScreen s = view("00000000001").lastMap().screen();
        assertThat(s.field("ACCTSID").orElseThrow().color()).isEqualTo("GREEN");
        assertThat(s.field("INFOMSG").orElseThrow().color()).isEqualTo("NEUTRAL");
    }

    @Test
    void xrefNotFound() {
        CicsResponse r = view("99999999999");
        assertReturnsToSelf(r);
        // 9200 lines 746-760: STRING truncated at WS-RETURN-MSG's 75 bytes; RESP 13 / RESP2 80 as X(10)
        assertThat(err(r)).isEqualTo(msg("Account:99999999999 not found in Cross ref file.  Resp:000000013  Reas:0000"));
        BmsScreen s = r.lastMap().screen();
        assertThat(s.value("ACCTSID")).isEqualTo("99999999999");
        assertThat(s.field("ACURBAL").orElseThrow().sent()).isFalse();
        assertThat(s.field("ACSFNAM").orElseThrow().sent()).isFalse();
        assertThat(CardDemoCommarea.ACCT_ID.getLong(commareaOut(r))).isEqualTo(99999999999L);
    }

    @Test
    void accountNotFoundStillReadsCustomer() {
        acctdat.delete("00000000001");
        CicsResponse r = view("00000000001");
        // 9300 lines 795-809
        assertThat(err(r)).isEqualTo(msg("Account:00000000001 not found in Acct Master file.Resp:000000013  Reas:0000"));
        BmsScreen s = r.lastMap().screen();
        // Line 710 tests DID-NOT-FIND-ACCT-IN-ACCTDAT, which is never set, so 9400 still runs: the customer is
        // shown, and FOUND-CUST makes 1200 move the never-read (LOW-VALUES) ACCOUNT-RECORD as well.
        assertThat(s.value("ACSFNAM")).isEqualTo(PicX.of("Immanuel", 25));
        assertThat(s.field("ACSTTUS").orElseThrow().sent()).isFalse();
        assertThat(s.value("ACURBAL")).isEqualTo("+           .00");
        assertThat(s.field("ADTOPEN").orElseThrow().sent()).isFalse();
    }

    @Test
    void customerNotFound() {
        custdat.delete("000000001");
        CicsResponse r = view("00000000001");
        // 9400 lines 845-856: 'CustId:' + 9-digit id, truncated at 75 bytes
        assertThat(err(r)).isEqualTo(msg("CustId:000000001 not found in customer master.Resp: 000000013  REAS:0000000"));
        BmsScreen s = r.lastMap().screen();
        assertThat(s.value("ACURBAL")).isEqualTo("+        194.00");
        assertThat(s.field("ACSFNAM").orElseThrow().sent()).isFalse();
        assertThat(s.field("ACSTSSN").orElseThrow().sent()).isFalse();
    }

    @Test
    void accountFileErrorUsesFileErrorMessage() {
        acctdat.setState(FileState.DISABLED);
        CicsResponse r = view("00000000001");
        // 9300 WHEN OTHER lines 811-818: WS-FILE-ERROR-MESSAGE (80 bytes) into WS-RETURN-MSG (75)
        assertThat(err(r)).isEqualTo(msg("File Error: READ     on ACCTDAT   returned RESP 000000084 ,RESP2 000000050 "));
    }

    @Test
    void duplicateAlternateKeyIsAFileError() {
        String dup = PicX.of("9999999999999999" + "000000001" + "00000000001", 50);
        ccxref.write(dup);
        CicsResponse r = view("00000000001");
        // CXACAIX is NONUNIQUEKEY (XREFFILE.jcl): READ returns DUPKEY (15), handled by WHEN OTHER (line 762)
        assertThat(err(r)).isEqualTo(msg("File Error: READ     on CXACAIX   returned RESP 000000015 ,RESP2 000000000 "));
    }

    @Test
    void customerFileErrorKeepsAccountData() {
        custdat.setState(FileState.CLOSED);
        CicsResponse r = view("00000000001");
        assertThat(err(r)).isEqualTo(msg("File Error: READ     on CUSTDAT   returned RESP 000000019 ,RESP2 000000060 "));
        assertThat(r.lastMap().screen().value("ACURBAL")).isEqualTo("+        194.00");
    }

    // ---- WHEN OTHER (lines 360-368) and helpers ------------------------------------------------------

    @Test
    void unexpectedContextSendsPlainText() {
        CardDemoCommarea c = CardDemoCommarea.of(commarea(1));
        CardDemoCommarea.PGM_CONTEXT.fill(c, PicX.LOW_VALUE);
        CicsResponse r = run(AidKey.ENTER, c.toBytes(), Map.of());
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.RETURN);
        assertThat(r.lastText().text()).isEqualTo(PicX.of("UNEXPECTED DATA SCENARIO", 75));
        assertThat(r.lastText().erase()).isTrue();
    }

    @Test
    void progCommareaIsPassedThrough() {
        byte[] in = Arrays.copyOf(commarea(1), 172);
        System.arraycopy(PicX.toBytes("COCRDLICCCLI"), 0, in, 160, 12);
        CicsResponse r = run(AidKey.ENTER, in, Map.of("ACCTSID", "00000000001"));
        assertThat(PicX.fromBytes(r.outcome().commarea()).substring(160, 172)).isEqualTo("COCRDLICCCLI");
    }

    @Test
    void respCodesAreMovedAsUnsignedNineDigitText() {
        assertThat(Coactvwc.respX(13)).isEqualTo("000000013 ");
        assertThat(Coactvwc.respX(-1)).isEqualTo("000000001 ");
    }

    @Test
    void invalidZonedDataIsEditedFromLowNibbles() {
        assertThat(Coactvwc.displayValue(PicX.lowValues(12), 2)).isEqualByComparingTo("0");
        assertThat(Coactvwc.displayValue("00000001940{", 2)).isEqualByComparingTo("194.00");
    }
}
