package com.carddemo.online.coactupc;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.common.copybook.AccountRecord;
import com.carddemo.common.copybook.CardDemoAsciiSeed;
import com.carddemo.common.copybook.CardDemoCommarea;
import com.carddemo.common.copybook.CardXrefRecord;
import com.carddemo.common.copybook.CustomerRecord;
import com.carddemo.runtime.bms.BmsAttributes;
import com.carddemo.runtime.bms.BmsScreen;
import com.carddemo.runtime.cics.AidKey;
import com.carddemo.runtime.cics.CicsContext;
import com.carddemo.runtime.cics.CicsDispatcher;
import com.carddemo.runtime.cics.CicsOutcome;
import com.carddemo.runtime.cics.CicsResponse;
import com.carddemo.runtime.cics.SystemInfo;
import com.carddemo.runtime.cobol.PicX;
import com.carddemo.runtime.vsam.FileControl;
import com.carddemo.runtime.vsam.KsdsFile;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.LinkedHashMap;
import java.util.Map;
import org.junit.jupiter.api.Test;

/**
 * Behavioural parity tests for COACTUPC driven through the CICS dispatcher (pseudo-conversation over several
 * tasks). Line numbers refer to app/cbl/COACTUPC.cbl. Data: app/data/ASCII - account 00000000001 is
 * xref'd (cardxref.txt line 49) to card 9680294154603697 / customer 000000001 (custdata.txt line 1:
 * Immanuel Madeline Kessler, NC 12546, (908)119-8310, SSN 020973888, DOB 1961-06-08, FICO 274);
 * acctdata.txt line 1: status Y, balance 00000001940{ = 194.00, credit limit 00000020200{ = 2020.00, cash
 * limit 00000010200{ = 1020.00, open 2014-11-20, expiry 2025-05-20, reissue 2025-05-20, group A000000000.
 */
class CoactupcTest {

    /** 2024-03-05 14:07:09 UTC: CURDATE 03/05/24 (3100-SCREEN-INIT). */
    static final Clock CLOCK = Clock.fixed(Instant.parse("2024-03-05T14:07:09.25Z"), ZoneOffset.UTC);

    static final String[] INPUT_FIELDS = {"ACCTSID", "ACSTTUS", "ACRDLIM", "ACSHLIM", "ACURBAL", "ACRCYCR",
        "ACRCYDB", "OPNYEAR", "OPNMON", "OPNDAY", "EXPYEAR", "EXPMON", "EXPDAY", "RISYEAR", "RISMON", "RISDAY",
        "AADDGRP", "ACSTNUM", "ACTSSN1", "ACTSSN2", "ACTSSN3", "ACSTFCO", "DOBYEAR", "DOBMON", "DOBDAY", "ACSFNAM",
        "ACSMNAM", "ACSLNAM", "ACSADL1", "ACSADL2", "ACSCITY", "ACSSTTE", "ACSZIPC", "ACSCTRY", "ACSPH1A",
        "ACSPH1B", "ACSPH1C", "ACSPH2A", "ACSPH2B", "ACSPH2C", "ACSGOVT", "ACSEFTC", "ACSPFLG"};

    final Coactupc program = new Coactupc();
    final KsdsFile acctdat = KsdsFile.inMemory("ACCTDAT", AccountRecord.LENGTH, 0, 11);
    final KsdsFile custdat = KsdsFile.inMemory("CUSTDAT", CustomerRecord.LENGTH, 0, 9);
    final KsdsFile ccxref = KsdsFile.inMemory("CCXREF", CardXrefRecord.LENGTH, 0, 16);
    final FileControl fct = new FileControl().register(acctdat).register(custdat).register(ccxref)
            .register(ccxref.path("CXACAIX", 25, 11, false));

    CoactupcTest() {
        acctdat.load(CardDemoAsciiSeed.records(AccountRecord.SEED_FILE, AccountRecord.LENGTH));
        custdat.load(CardDemoAsciiSeed.records(CustomerRecord.SEED_FILE, CustomerRecord.LENGTH));
        ccxref.load(CardDemoAsciiSeed.records(CardXrefRecord.SEED_FILE, CardXrefRecord.LENGTH));
    }

    CicsResponse run(AidKey aid, byte[] commarea, Map<String, String> fields) {
        return CicsDispatcher.run(program, CicsContext.builder().transid("CAUP").aid(aid).commarea(commarea)
                .terminalInput(fields).fileControl(fct).clock(CLOCK).systemInfo(SystemInfo.defaults()).build());
    }

    static BmsScreen screen(CicsResponse r) {
        return r.lastMap().screen();
    }

    static String err(CicsResponse r) {
        return screen(r).value("ERRMSG").stripTrailing();
    }

    static String info(CicsResponse r) {
        return screen(r).value("INFOMSG").stripTrailing();
    }

    static char action(CicsResponse r) {
        return (char) (r.outcome().commarea()[CardDemoCommarea.LENGTH] & 0xFF);
    }

    static CardDemoCommarea commarea(CicsResponse r) {
        return CardDemoCommarea.of(java.util.Arrays.copyOf(r.outcome().commarea(), CardDemoCommarea.LENGTH));
    }

    /** What the terminal sends back: every input field (all have FSET), trailing blanks dropped. */
    static Map<String, String> echo(CicsResponse r) {
        Map<String, String> m = new LinkedHashMap<>();
        for (String f : INPUT_FIELDS) {
            m.put(f, screen(r).value(f).stripTrailing());
        }
        return m;
    }

    CicsResponse first() {
        return run(AidKey.ENTER, new byte[0], Map.of());
    }

    CicsResponse fetch(String acct) {
        return run(AidKey.ENTER, first().outcome().commarea(), Map.of("ACCTSID", acct));
    }

    /**
     * Fetch account 18 and submit {@code changes}. Its customer (000000018) passes every 1200 edit except the
     * state: 'AP' is in VALID-US-STATE-ZIP-CD2-COMBO but not in VALID-US-STATE-CODE (CSLKPCDY), so the base
     * change is state CA / zip 90210 (combo CA90).
     */
    CicsResponse change(Map<String, String> changes) {
        Map<String, String> all = new LinkedHashMap<>(Map.of("ACSSTTE", "CA", "ACSZIPC", "90210"));
        all.putAll(changes);
        return change("00000000018", all);
    }

    CicsResponse change(String acct, Map<String, String> changes) {
        CicsResponse f = fetch(acct);
        Map<String, String> in = echo(f);
        in.putAll(changes);
        return run(AidKey.ENTER, f.outcome().commarea(), in);
    }

    /**
     * Every seed account has ACCT-GROUP-ID = spaces; the blank AADDGRP comes back as LOW-VALUES (1100 line
     * 1206-1210) and 1205 compares TRIM(LOW-VALUES) with TRIM(SPACES): always "changed". Tests that need
     * NO-CHANGES-DETECTED give account 1 a group id first.
     */
    void giveAccount1AGroupId() {
        String a = acct1();
        acctdat.store().update(PicX.toBytes("00000000001"), PicX.toBytes(a.substring(0, 112) + "GRP0000001"
                + a.substring(122)));
    }

    String acct18() {
        return PicX.fromBytes(acctdat.store().get(PicX.toBytes("00000000018")).orElseThrow());
    }

    String cust18() {
        return PicX.fromBytes(custdat.store().get(PicX.toBytes("000000018")).orElseThrow());
    }

    String acct1() {
        return PicX.fromBytes(acctdat.store().get(PicX.toBytes("00000000001")).orElseThrow());
    }

    String cust1() {
        return PicX.fromBytes(custdat.store().get(PicX.toBytes("000000001")).orElseThrow());
    }

    // ---------------------------------------------------------------- entry / search

    @Test
    void firstEntryShowsSearchScreen() {
        // 0000-MAIN lines 887-897 (EIBCALEN = 0) and WHEN ACUP-DETAILS-NOT-FETCHED AND CDEMO-PGM-ENTER (944-951)
        CicsResponse r = first();
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.RETURN_TRANSID);
        assertThat(r.outcome().nextTransid()).isEqualTo("CAUP");
        assertThat(r.outcome().commarea()).hasSize(2000);
        assertThat(action(r)).isEqualTo('\0');
        assertThat(commarea(r).isPgmReenter()).isTrue();
        BmsScreen s = screen(r);
        assertThat(s.value("TRNNAME")).isEqualTo("CAUP");
        assertThat(s.value("PGMNAME")).isEqualTo("COACTUPC");
        assertThat(s.value("CURDATE")).isEqualTo("03/05/24");
        assertThat(info(r)).isEqualTo("Enter or update id of account to update");
        assertThat(err(r)).isEmpty();
        assertThat(s.cursorField()).isEqualTo("ACCTSID");
        assertThat(s.field("ACCTSID").orElseThrow().attributeByte()).isEqualTo(BmsAttributes.DFHBMFSE);
        assertThat(s.field("ACSTTUS").orElseThrow().attributeByte()).isEqualTo(BmsAttributes.DFHBMPRF);
        assertThat(s.erase()).isTrue();
    }

    @Test
    void blankAccountId() {
        // 1210 lines 1800-1812 sets 'Account number not provided', 1200 lines 1446-1449 overrides with
        // NO-SEARCH-CRITERIA-RECEIVED (WS-RETURN-MSG is MOVEd, not STRINGed); CSSETATY-style '*' (3300 2032)
        CicsResponse r = run(AidKey.ENTER, first().outcome().commarea(), Map.of("ACCTSID", ""));
        assertThat(err(r)).isEqualTo("No input received");
        assertThat(screen(r).value("ACCTSID")).isEqualTo("*          ");
        assertThat(screen(r).cursorField()).isEqualTo("ACCTSID");
        assertThat(action(r)).isEqualTo('\0');
    }

    @Test
    void nonNumericAndZeroAccountId() {
        // 1210 lines 1816-1830
        for (String acct : new String[] {"ABC", "00000000000", "1234567890X"}) {
            CicsResponse r = run(AidKey.ENTER, first().outcome().commarea(), Map.of("ACCTSID", acct));
            assertThat(err(r)).isEqualTo("Account Number if supplied must be a 11 digit Non-Zero Number");
            assertThat(screen(r).cursorField()).isEqualTo("ACCTSID");
        }
    }

    @Test
    void accountNotInXref() {
        // 9200 WHEN DFHRESP(NOTFND) lines 3675-3687: RESP 13 (NOTFND), RESP2 as returned by the file
        CicsResponse r = fetch("00000000099");
        assertThat(err(r)).startsWith("Account:00000000099 not found in Cross ref file.  Resp:000000013  Reas:");
        assertThat(info(r)).isEqualTo("Enter or update id of account to update");
        assertThat(action(r)).isEqualTo('\0');
    }

    @Test
    void accountInXrefButNotInAcctdat() {
        // 9300 NOTFND lines 3720-3733; DID-NOT-FIND-ACCT-IN-ACCTDAT is never set so 9000 continues to 9400
        // and, the customer being found, 2000 sets ACUP-SHOW-DETAILS (lines 2573-2575)
        acctdat.store().delete(PicX.toBytes("00000000001"));
        CicsResponse r = fetch("00000000001");
        assertThat(err(r)).startsWith("Account:00000000001 not found in Acct Master file.Resp:000000013  Reas:");
        assertThat(action(r)).isEqualTo('S');
    }

    @Test
    void customerNotFound() {
        // 9400 NOTFND lines 3768-3781
        custdat.store().delete(PicX.toBytes("000000001"));
        CicsResponse r = fetch("00000000001");
        assertThat(err(r)).startsWith("CustId:000000001 not found in customer master.Resp: 000000013  REAS:");
        assertThat(action(r)).isEqualTo('\0');
    }

    @Test
    void fetchShowsAccountAndCustomer() {
        // 9500 lines 3801-3874 -> 3202-SHOW-ORIGINAL-VALUES lines 2786-2861, currency PIC +ZZZ,ZZZ,ZZZ.99
        CicsResponse r = fetch("00000000001");
        BmsScreen s = screen(r);
        assertThat(action(r)).isEqualTo('S');
        assertThat(info(r)).isEqualTo("Update account details presented above.");
        assertThat(s.value("ACSTTUS")).isEqualTo("Y");
        assertThat(s.value("ACURBAL")).isEqualTo("+        194.00");
        assertThat(s.value("ACRDLIM")).isEqualTo("+      2,020.00");
        assertThat(s.value("ACSHLIM")).isEqualTo("+      1,020.00");
        assertThat(s.value("ACRCYCR")).isEqualTo("+           .00");
        assertThat(s.value("OPNYEAR") + s.value("OPNMON") + s.value("OPNDAY")).isEqualTo("20141120");
        assertThat(s.value("EXPYEAR") + s.value("EXPMON") + s.value("EXPDAY")).isEqualTo("20250520");
        // ACCT-GROUP-ID (offset 112) is blank in the seed; 'A000000000' is ACCT-ADDR-ZIP
        assertThat(s.value("AADDGRP")).isEqualTo("          ");
        assertThat(s.value("ACSTNUM")).isEqualTo("000000001");
        assertThat(s.value("ACTSSN1") + s.value("ACTSSN2") + s.value("ACTSSN3")).isEqualTo("020973888");
        assertThat(s.value("DOBYEAR") + s.value("DOBMON") + s.value("DOBDAY")).isEqualTo("19610608");
        assertThat(s.value("ACSTFCO")).isEqualTo("274");
        assertThat(s.value("ACSFNAM").stripTrailing()).isEqualTo("Immanuel");
        assertThat(s.value("ACSSTTE")).isEqualTo("NC");
        assertThat(s.value("ACSZIPC")).isEqualTo("12546");
        assertThat(s.value("ACSCTRY")).isEqualTo("USA");
        assertThat(s.value("ACSPH1A") + s.value("ACSPH1B") + s.value("ACSPH1C")).isEqualTo("9081198310");
        assertThat(s.value("ACSPFLG")).isEqualTo("Y");
        assertThat(s.field("ACSFNAM").orElseThrow().attributeByte()).isEqualTo(BmsAttributes.DFHBMFSE);
        assertThat(s.field("ACSTNUM").orElseThrow().attributeByte()).isEqualTo(BmsAttributes.DFHBMPRF);
        CardDemoCommarea c = commarea(r);
        assertThat(CardDemoCommarea.CUST_ID.get(c).longValue()).isEqualTo(1);
        assertThat(CardDemoCommarea.CARD_NUM.get(c).toPlainString()).isEqualTo("9680294154603697");
    }

    // ---------------------------------------------------------------- edits

    @Test
    void noChangesDetected() {
        // 1205 lines 1677-1790 -> NO-CHANGES-DETECTED; cursor on ACSTTUS (3300 line 2995)
        giveAccount1AGroupId();
        CicsResponse r = change("00000000001", Map.of());
        assertThat(err(r)).isEqualTo("No change detected with respect to values fetched.");
        assertThat(screen(r).cursorField()).isEqualTo("ACSTTUS");
        assertThat(action(r)).isEqualTo('S');
    }

    @Test
    void caseOnlyChangeOfNameIsNotAChange() {
        // 1205 compares UPPER-CASE(TRIM(..)) for names
        giveAccount1AGroupId();
        CicsResponse r = change("00000000001", Map.of("ACSFNAM", "IMMANUEL", "AADDGRP", "grp0000001"));
        assertThat(err(r)).isEqualTo("No change detected with respect to values fetched.");
    }

    @Test
    void seedDataOfAccount1FailsTheEdits() {
        // Customer 1 seed: FICO 274 (< 300), phone 2 area code 373 (not in VALID-GENERAL-PURP-CODE) and NC +
        // '12' not in VALID-US-STATE-ZIP-CD2-COMBO (CSLKPCDY has NC27/NC28). First message wins (STRING only
        // when WS-RETURN-MSG-OFF); cursor follows the 3300 EVALUATE order (STATE before PHONE).
        CicsResponse r = change("00000000001", Map.of("ACSTFCO", "700"));
        assertThat(err(r)).isEqualTo("Phone Number 2: Not valid North America general purpose area code");
        assertThat(action(r)).isEqualTo('E');
        assertThat(screen(r).cursorField()).isEqualTo("ACSSTTE");
    }

    @Test
    void validChangeIsConfirmedThenSaved() {
        CicsResponse r = change(Map.of("ACRDLIM", "3,000.50"));
        // 1200 line 1672 SET ACUP-CHANGES-OK-NOT-CONFIRMED; 3250 PROMPT-FOR-CONFIRMATION; 3390 F5/F12 bright
        assertThat(err(r)).isEmpty();
        assertThat(action(r)).isEqualTo('N');
        assertThat(info(r)).isEqualTo("Changes validated.Press F5 to save");
        assertThat(screen(r).value("ACRDLIM")).isEqualTo("+      3,000.50");
        assertThat(screen(r).field("FKEY05").orElseThrow().attributeByte()).isEqualTo(BmsAttributes.DFHBMASB);
        assertThat(screen(r).field("ACSZIPC").orElseThrow().attributeByte()).isEqualTo(BmsAttributes.DFHBMPRF);

        CicsResponse saved = run(AidKey.PF5, r.outcome().commarea(), echo(r));
        // 2000 WHEN ACUP-CHANGES-OK-NOT-CONFIRMED AND CCARD-AID-PFK05 -> 9600 -> ACUP-CHANGES-OKAYED-AND-DONE
        assertThat(action(saved)).isEqualTo('C');
        assertThat(info(saved)).isEqualTo("Changes committed to database");
        assertThat(err(saved)).isEmpty();
        String acct = acct18();
        assertThat(AccountRecord.ACCT_CREDIT_LIMIT.get(AccountRecord.of(acct)).toPlainString()).isEqualTo("3000.50");
        assertThat(acct.substring(0, 102)).isEqualTo(
                "00000000018Y00000001440{00000030005{00000014960{2018-11-152023-09-102023-09-1000000000000{00000000000{");
        // ACCT-UPDATE-RECORD has no ADDR-ZIP: GROUP-ID (blank on screen -> LOW-VALUES) lands at offset 102 over
        // the old ACCT-ADDR-ZIP 'A000000000', and FILLER (never initialised) is LOW-VALUES (lines 418-433)
        assertThat(acct.substring(102)).isEqualTo(PicX.lowValues(198));
        String cust = cust18();
        assertThat(CustomerRecord.CUST_ADDR_ZIP.get(CustomerRecord.of(cust))).isEqualTo("90210     ");
        assertThat(CustomerRecord.CUST_ADDR_STATE_CD.get(CustomerRecord.of(cust))).isEqualTo("CA");
        assertThat(CustomerRecord.CUST_PHONE_NUM_1.get(CustomerRecord.of(cust))).isEqualTo("(303)654-3323  ");
        assertThat(CustomerRecord.CUST_FICO_CREDIT_SCORE.get(CustomerRecord.of(cust)).intValue()).isEqualTo(340);
        assertThat(cust.substring(332)).isEqualTo(PicX.lowValues(168));

        // next ENTER: 0000 WHEN ACUP-CHANGES-OKAYED-AND-DONE (lines 967-981) -> fresh search screen
        CicsResponse next = run(AidKey.ENTER, saved.outcome().commarea(), echo(saved));
        assertThat(info(next)).isEqualTo("Enter or update id of account to update");
        assertThat(action(next)).isEqualTo('\0');
    }

    @Test
    void fieldEditMessagesAndCursor() {
        Object[][] cases = {
            {Map.of("ACSTTUS", "X"), "Account Status must be Y or N.", "ACSTTUS"},
            {Map.of("ACSTTUS", ""), "Account Status must be supplied.", "ACSTTUS"},
            {Map.of("OPNMON", "13"), "Open Date: Month must be a number between 1 and 12.", "OPNMON"},
            {Map.of("OPNYEAR", "1814"), "Open Date : Century is not valid.", "OPNYEAR"},
            {Map.of("OPNDAY", "31", "OPNMON", "11"), "Open Date:Cannot have 31 days in this month.", "OPNMON"},
            {Map.of("ACRDLIM", "12AB"), "Credit Limit is not valid", "ACRDLIM"},
            {Map.of("ACRCYCR", ""), "Current Cycle Credit Limi must be supplied.", "ACRCYCR"},
            {Map.of("ACTSSN1", "666"), "SSN: First 3 chars: should not be 000, 666, or between 900 and 999",
                "ACTSSN1"},
            {Map.of("ACTSSN2", "00"), "SSN 4th & 5th chars must not be zero.", "ACTSSN2"},
            {Map.of("DOBYEAR", "2025"), "Date of Birth:cannot be in the future", "DOBYEAR"},
            {Map.of("ACSTFCO", "299"), "FICO Score: should be between 300 and 850", "ACSTFCO"},
            {Map.of("ACSTFCO", "851"), "FICO Score: should be between 300 and 850", "ACSTFCO"},
            {Map.of("ACSFNAM", "Imm4nuel"), "First Name can have alphabets only.", "ACSFNAM"},
            {Map.of("ACSMNAM", "M."), "Middle Name can have alphabets only.", "ACSMNAM"},
            {Map.of("ACSADL1", ""), "Address Line 1 must be supplied.", "ACSADL1"},
            {Map.of("ACSSTTE", "XX"), "State: is not a valid state code", "ACSSTTE"},
            {Map.of("ACSZIPC", "1254A"), "Zip must be all numeric.", "ACSZIPC"},
            {Map.of("ACSSTTE", "AP", "ACSZIPC", "96003"), "State: is not a valid state code", "ACSSTTE"},
            {Map.of("ACSZIPC", "27546"), "Invalid zip code for state", "ACSSTTE"},
            {Map.of("ACSPH1A", "000"), "Phone Number 1: Area code cannot be zero", "ACSPH1A"},
            {Map.of("ACSPH1A", "211"), "Phone Number 1: Not valid North America general purpose area code",
                "ACSPH1A"},
            {Map.of("ACSPH2C", "86A4"), "Phone Number 2: Line number code must be A 4 digit number.", "ACSPH2C"},
            {Map.of("ACSEFTC", "ABC"), "EFT Account Id must be all numeric.", "ACSEFTC"},
            {Map.of("ACSPFLG", "Q"), "Primary Card Holder must be Y or N.", "ACSPFLG"},
        };
        for (Object[] c : cases) {
            @SuppressWarnings("unchecked")
            Map<String, String> changes = (Map<String, String>) c[0];
            CicsResponse r = change(changes);
            assertThat(err(r)).as(changes.toString()).isEqualTo(c[1]);
            assertThat(screen(r).cursorField()).as(changes.toString()).isEqualTo(c[2]);
            assertThat(action(r)).isEqualTo('E');
            assertThat(info(r)).isEqualTo("Update account details presented above.");
        }
    }

    @Test
    void blankFieldsAreShownAsStarOnReentry() {
        // CSSETATY: (NOT-OK OR BLANK) AND CDEMO-PGM-REENTER -> red; BLANK -> '*'
        CicsResponse r = change(Map.of("ACSLNAM", ""));
        assertThat(err(r)).isEqualTo("Last Name must be supplied.");
        assertThat(screen(r).value("ACSLNAM").stripTrailing()).isEqualTo("*");
    }

    // ---------------------------------------------------------------- PF keys

    @Test
    void pf5BeforeValidationIsTreatedAsEnter() {
        // 0000 lines 905-920: PF5 is only valid in ACUP-CHANGES-OK-NOT-CONFIRMED
        giveAccount1AGroupId();
        CicsResponse f = fetch("00000000001");
        CicsResponse r = run(AidKey.PF5, f.outcome().commarea(), echo(f));
        assertThat(err(r)).isEqualTo("No change detected with respect to values fetched.");
        assertThat(acct1().substring(24, 36)).isEqualTo("00000020200{");
    }

    @Test
    void pf3ReturnsToMenuOrCaller() {
        // 0000 WHEN CCARD-AID-PFK03 lines 924-943: SYNCPOINT then XCTL; PF15 folds to PF3 (CSSTRPFY)
        CicsResponse f = fetch("00000000001");
        for (AidKey key : new AidKey[] {AidKey.PF3, AidKey.PF15}) {
            CicsResponse r = run(key, f.outcome().commarea(), echo(f));
            assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.XCTL);
            assertThat(r.outcome().nextProgram()).isEqualTo("COMEN01C");
            CardDemoCommarea c = CardDemoCommarea.of(r.outcome().commarea());
            assertThat(c.toTranid()).isEqualTo("CM00");
            assertThat(c.fromProgram()).isEqualTo("COACTUPC");
            assertThat(c.fromTranid()).isEqualTo("CAUP");
            assertThat(c.lastMapset()).isEqualTo("COACTUP");
            assertThat(c.isPgmEnter()).isTrue();
        }
        byte[] fromList = f.outcome().commarea().clone();
        CardDemoCommarea c = commarea(f);
        c.setFromProgram("COCRDLIC");
        c.setFromTranid("CCLI");
        System.arraycopy(c.toBytes(), 0, fromList, 0, CardDemoCommarea.LENGTH);
        CicsResponse r = run(AidKey.PF3, fromList, Map.of());
        assertThat(r.outcome().nextProgram()).isEqualTo("COCRDLIC");
    }

    @Test
    void pf12DiscardsChangesAndRefetches() {
        CicsResponse r = change(Map.of("ACSTFCO", "700"));
        assertThat(action(r)).isEqualTo('N');
        CicsResponse back = run(AidKey.PF12, r.outcome().commarea(), echo(r));
        assertThat(action(back)).isEqualTo('S');
        assertThat(screen(back).value("ACSTFCO")).isEqualTo("340");
        assertThat(screen(back).value("ACSSTTE")).isEqualTo("AP");
        assertThat(screen(back).value("ACSZIPC")).isEqualTo("96003");
    }

    @Test
    void invalidKeyIsTreatedAsEnter() {
        CicsResponse r = run(AidKey.PF7, first().outcome().commarea(), Map.of("ACCTSID", "00000000001"));
        assertThat(action(r)).isEqualTo('S');
    }

    // ---------------------------------------------------------------- update path

    CicsResponse confirmed() {
        CicsResponse r = change(Map.of("ACSTFCO", "700"));
        assertThat(err(r)).isEmpty();
        assertThat(action(r)).isEqualTo('N');
        return r;
    }

    @Test
    void recordChangedBySomeoneElse() {
        // 9700 lines 4110-4192: compare the READ UPDATE image with ACUP-OLD-DETAILS
        CicsResponse r = confirmed();
        String acct = acct18();
        acctdat.store().update(PicX.toBytes("00000000018"), PicX.toBytes(acct.substring(0, 11) + "N" + acct.substring(12)));
        String custBefore = cust18();
        CicsResponse s = run(AidKey.PF5, r.outcome().commarea(), echo(r));
        assertThat(err(s)).isEqualTo("Record changed by some one else. Please review");
        assertThat(action(s)).isEqualTo('S');
        assertThat(cust18()).isEqualTo(custBefore);
    }

    @Test
    void accountLockFailure() {
        // 9600 lines 3888-3898: READ UPDATE fails -> COULD-NOT-LOCK-ACCT-FOR-UPDATE -> 'L'
        CicsResponse r = confirmed();
        acctdat.store().delete(PicX.toBytes("00000000018"));
        CicsResponse s = run(AidKey.PF5, r.outcome().commarea(), echo(r));
        assertThat(err(s)).isEqualTo("Could not lock account record for update");
        assertThat(action(s)).isEqualTo('L');
        assertThat(info(s)).isEqualTo("Changes unsuccessful. Please try again");
    }

    @Test
    void customerLockFailureFallsThroughToSuccessBranch() {
        // Oddity: 2000 has no WHEN for COULD-NOT-LOCK-CUST-FOR-UPDATE, so WHEN OTHER sets 'C' (lines 2600-2611)
        CicsResponse r = confirmed();
        custdat.store().delete(PicX.toBytes("000000018"));
        String acctBefore = acct18();
        CicsResponse s = run(AidKey.PF5, r.outcome().commarea(), echo(r));
        assertThat(err(s)).isEqualTo("Could not lock customer record for update");
        assertThat(action(s)).isEqualTo('C');
        assertThat(info(s)).isEqualTo("Changes committed to database");
        assertThat(acct18()).isEqualTo(acctBefore);
    }

    @Test
    void customerRewriteFailureRollsBackAccountRewrite() {
        // 9600 lines 4085-4103: REWRITE CUSTDAT fails -> LOCKED-BUT-UPDATE-FAILED + SYNCPOINT ROLLBACK, which
        // backs out the ACCTDAT REWRITE. Forced here by a changed customer id (REWRITE key != held key: INVREQ).
        CicsResponse r = change(Map.of("ACRDLIM", "9999"));
        String acctBefore = acct18();
        Map<String, String> in = echo(r);
        in.put("ACSTNUM", "000000002");
        CicsResponse s = run(AidKey.PF5, r.outcome().commarea(), in);
        assertThat(err(s)).isEqualTo("Update of record failed");
        assertThat(action(s)).isEqualTo('F');
        assertThat(acct18()).isEqualTo(acctBefore);
        assertThat(CustomerRecord.CUST_ADDR_ZIP.get(CustomerRecord.of(cust18()))).isEqualTo("96003-5867");
    }

    @Test
    void accountRewriteFailureHasNoRollback() {
        // 9600 lines 4070-4083: REWRITE ACCTDAT fails -> LOCKED-BUT-UPDATE-FAILED, CUSTDAT not written
        CicsResponse r = confirmed();
        Map<String, String> in = echo(r);
        in.put("ACCTSID", "00000000002");
        String custBefore = cust18();
        CicsResponse s = run(AidKey.PF5, r.outcome().commarea(), in);
        // READ UPDATE uses the screen account id (00000000002), whose record differs from ACUP-OLD-DETAILS
        assertThat(err(s)).isEqualTo("Record changed by some one else. Please review");
        assertThat(cust18()).isEqualTo(custBefore);
    }

    @Test
    void unvalidatedChangesAfterConfirmationAreSaved() {
        // Oddity: with ACUP-CHANGES-OK-NOT-CONFIRMED, 1200 (line 1474) skips the edits, so data changed
        // between ENTER and F5 is written unvalidated
        CicsResponse r = confirmed();
        Map<String, String> in = echo(r);
        in.put("ACSFNAM", "123");
        CicsResponse s = run(AidKey.PF5, r.outcome().commarea(), in);
        assertThat(action(s)).isEqualTo('C');
        assertThat(CustomerRecord.CUST_FIRST_NAME.get(CustomerRecord.of(cust18())).stripTrailing()).isEqualTo("123");
    }

    @Test
    void unexpectedActionAbends() {
        // 2000 WHEN OTHER lines 2622-2629 -> ABEND-ROUTINE: SEND ABEND-DATA, ABEND '9999'
        giveAccount1AGroupId();
        CicsResponse f = fetch("00000000001");
        byte[] forged = f.outcome().commarea().clone();
        forged[CardDemoCommarea.LENGTH] = 'X';
        CicsResponse r = run(AidKey.ENTER, forged, echo(f));
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.ABEND);
        assertThat(r.outcome().abendCode()).isEqualTo("9999");
        assertThat(r.lastText().text()).startsWith("0001COACTUPC");
        assertThat(r.lastText().text()).contains("UNEXPECTED DATA SCENARIO");
    }
}
