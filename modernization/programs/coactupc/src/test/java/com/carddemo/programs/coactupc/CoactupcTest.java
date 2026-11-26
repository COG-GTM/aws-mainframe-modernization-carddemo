package com.carddemo.programs.coactupc;

import static org.junit.jupiter.api.Assertions.*;

import com.carddemo.programs.coactupc.copybook.*;
import com.carddemo.programs.coactupc.gateway.*;
import com.carddemo.programs.coactupc.gateway.InMemoryDataStore.Operation;
import java.math.BigDecimal;
import java.nio.file.Path;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.Map;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/** Behaviour of COACTUPC pinned against the CardDemo ASCII sample data (account 00000000001). */
class CoactupcTest {

    static final Path DATA = Path.of(System.getProperty("carddemo.data.dir", "../../../app/data/ASCII"));
    static final Clock CLOCK = Clock.fixed(Instant.parse("2026-11-26T10:15:30Z"), ZoneOffset.UTC);
    static final String ACCT = "00000000001";

    InMemoryDataStore store;
    Coactupc program;

    @BeforeEach
    void setUp() throws Exception {
        store = new InMemoryDataStore().loadAsciiExtracts(DATA.resolve("acctdata.txt"),
                DATA.resolve("custdata.txt"), DATA.resolve("cardxref.txt"));
        program = newProgram(new LeDateValidator());
    }

    Coactupc newProgram(DateValidator validator) {
        return new Coactupc(store, store.accounts(), store.customers(), store, validator, CLOCK);
    }

    static CactupaInput echo(CactupaOutput screen, Map<CactupaField, String> overrides) {
        CactupaInput in = CactupaInput.empty();
        for (CactupaField f : CactupaField.values()) {
            if (f.ordinal() < CactupaField.ACCTSID.ordinal() || f.ordinal() > CactupaField.ACSPFLG.ordinal()) {
                continue;
            }
            String d = screen.data(f);
            if (!d.chars().allMatch(c -> c == 0)) {
                in.with(f, d);
            }
        }
        overrides.forEach(in::with);
        return in;
    }

    CicsResult start() {
        return program.execute(CicsRequest.initial());
    }

    CicsResult fetch() {
        return program.execute(start().next(AidKey.DFHENTER, CactupaInput.empty().with(CactupaField.ACCTSID, ACCT)));
    }

    CicsResult change(CicsResult shown, Map<CactupaField, String> overrides) {
        return program.execute(shown.next(AidKey.DFHENTER, echo(shown.screen(), overrides)));
    }

    static Map<CactupaField, String> validChange(String creditLimit) {
        return Map.of(CactupaField.ACRDLIM, creditLimit, CactupaField.ACSTFCO, "700", CactupaField.ACSZIPC, "27514",
                CactupaField.ACSPH2A, "908");
    }

    static String err(CicsResult r) {
        return r.screen().text(CactupaField.ERRMSG);
    }

    static String info(CicsResult r) {
        return r.screen().text(CactupaField.INFOMSG);
    }

    // 0000-MAIN: EIBCALEN = 0 -> 3000-SEND-MAP with search prompt
    @Test
    void initialEntryPromptsForAccount() {
        CicsResult r = start();
        assertEquals(CicsResult.Kind.RETURN, r.kind());
        assertEquals("CAUP", r.transId());
        assertEquals("Enter or update id of account to update", info(r));
        assertEquals(CactupaField.ACCTSID, r.screen().cursorField());
        assertEquals(BmsAttribute.DFHBMFSE, r.screen().attribute(CactupaField.ACCTSID));
        assertTrue(r.commarea().isPgmReenter());
        assertTrue(r.programCommarea().isDetailsNotFetched());
        assertEquals("CACTUPA", r.screen().mapName());
        assertEquals("COACTUP", r.screen().mapsetName());
        assertEquals("11/26/26", r.screen().text(CactupaField.CURDATE));
        assertEquals("10:15:30", r.screen().text(CactupaField.CURTIME));
    }

    // 0000-MAIN WHEN CCARD-AID-PFK03: SYNCPOINT + XCTL to caller / menu
    @Test
    void pf3TransfersToMenu() {
        CicsResult r = program.execute(start().next(AidKey.DFHPF3, CactupaInput.empty()));
        assertEquals(CicsResult.Kind.XCTL, r.kind());
        assertEquals("COMEN01C", r.program());
        assertEquals("CM00", r.commarea().toTranid);
        assertEquals("CAUP", r.commarea().fromTranid);
        assertEquals("COACTUPC", r.commarea().fromProgram);
        assertEquals("COACTUP", r.commarea().lastMapset);
        assertEquals("CACTUPA", r.commarea().lastMap);
        assertTrue(r.commarea().isUsrtypUser());
        assertTrue(r.commarea().isPgmEnter());
    }

    // PF15 folds onto PF3 (CSSTRPFY); caller program is honoured
    @Test
    void pf15ReturnsToCallingProgram() {
        CardDemoCommarea c = CardDemoCommarea.initialized();
        c.fromProgram = "COCRDLIC";
        c.fromTranid = "CCLI";
        c.setPgmReenter();
        CicsResult r = program.execute(new CicsRequest(AidKey.DFHPF15, c, null, CactupaInput.empty()));
        assertEquals("COCRDLIC", r.program());
        assertEquals("CCLI", r.commarea().toTranid);
    }

    // 1210-EDIT-ACCOUNT blank + 1200 NO-SEARCH-CRITERIA-RECEIVED + 3300 '*' in red
    @Test
    void blankAccountIdIsRejected() {
        CicsResult r = program.execute(start().next(AidKey.DFHENTER, CactupaInput.empty().with(CactupaField.ACCTSID, "")));
        assertEquals("No input received", err(r));
        assertEquals("*", r.screen().text(CactupaField.ACCTSID));
        assertEquals(BmsColor.DFHRED, r.screen().color(CactupaField.ACCTSID));
        assertEquals(CactupaField.ACCTSID, r.screen().cursorField());
    }

    @Test
    void nonNumericAccountIdIsRejected() {
        CicsResult r = program.execute(start().next(AidKey.DFHENTER, CactupaInput.empty().with(CactupaField.ACCTSID, "123")));
        assertEquals("Account Number if supplied must be a 11 digit Non-Zero Number", err(r));
        assertEquals(BmsColor.DFHRED, r.screen().color(CactupaField.ACCTSID));
        assertEquals(0, r.commarea().acctId);
    }

    @Test
    void zeroAccountIdIsRejected() {
        CicsResult r = program.execute(start().next(AidKey.DFHENTER,
                CactupaInput.empty().with(CactupaField.ACCTSID, "00000000000")));
        assertEquals("Account Number if supplied must be a 11 digit Non-Zero Number", err(r));
    }

    // 9200-GETCARDXREF-BYACCT NOTFND message (STRING truncated to WS-RETURN-MSG X(75))
    @Test
    void accountMissingFromXref() {
        CicsResult r = program.execute(start().next(AidKey.DFHENTER,
                CactupaInput.empty().with(CactupaField.ACCTSID, "00000099999")));
        assertEquals("Account:00000099999 not found in Cross ref file.  Resp:000000013  Reas:0000", err(r));
        assertTrue(r.programCommarea().isDetailsNotFetched());
    }

    // 9200 WHEN OTHER -> WS-FILE-ERROR-MESSAGE
    @Test
    void xrefIoErrorShowsFileErrorMessage() {
        store.forceNext(Operation.XREF_READ, CicsResp.IOERR, 0);
        CicsResult r = program.execute(start().next(AidKey.DFHENTER, CactupaInput.empty().with(CactupaField.ACCTSID, ACCT)));
        assertEquals("File Error: READ     on CXACAIX   returned RESP 000000017 ,RESP2 000000000", err(r));
    }

    // 9300 NOTFND: 88 DID-NOT-FIND-ACCT-IN-ACCTDAT is never set, so 9400/9500 still run
    @Test
    void accountMissingFromMasterStillReadsCustomer() {
        store.forceNext(Operation.ACCT_READ, CicsResp.NOTFND, 0);
        CicsResult r = program.execute(start().next(AidKey.DFHENTER, CactupaInput.empty().with(CactupaField.ACCTSID, ACCT)));
        assertEquals("Account:00000000001 not found in Acct Master file.Resp:000000013  Reas:0000", err(r));
        assertTrue(r.programCommarea().isShowDetails());
        assertEquals(0, r.commarea().acctId);
        assertEquals(1, r.commarea().custId);
    }

    @Test
    void customerMissingFromMaster() {
        store.forceNext(Operation.CUST_READ, CicsResp.NOTFND, 0);
        CicsResult r = program.execute(start().next(AidKey.DFHENTER, CactupaInput.empty().with(CactupaField.ACCTSID, ACCT)));
        assertEquals("CustId:000000001 not found in customer master.Resp: 000000013  REAS:0000000", err(r));
        assertTrue(r.programCommarea().isDetailsNotFetched());
    }

    // 2000 -> 9000-READ-ACCT -> 9500, 3202-SHOW-ORIGINAL-VALUES, 3320 unprotect
    @Test
    void fetchShowsAccountAndCustomerFromFixture() {
        CicsResult r = fetch();
        CactupaOutput s = r.screen();
        assertTrue(r.programCommarea().isShowDetails());
        assertEquals("Update account details presented above.", info(r));
        assertEquals("Y", s.text(CactupaField.ACSTTUS));
        assertEquals("+        194.00", s.text(CactupaField.ACURBAL));
        assertEquals("+      2,020.00", s.text(CactupaField.ACRDLIM));
        assertEquals("+      1,020.00", s.text(CactupaField.ACSHLIM));
        assertEquals("2014", s.text(CactupaField.OPNYEAR));
        assertEquals("11", s.text(CactupaField.OPNMON));
        assertEquals("20", s.text(CactupaField.OPNDAY));
        assertEquals("000000001", s.text(CactupaField.ACSTNUM));
        CustomerRecord c = store.customer("000000001");
        String ssn = String.format("%09d", c.ssn());
        assertEquals(ssn.substring(0, 3), s.text(CactupaField.ACTSSN1));
        assertEquals(ssn.substring(3, 5), s.text(CactupaField.ACTSSN2));
        assertEquals(ssn.substring(5), s.text(CactupaField.ACTSSN3));
        assertEquals(c.phoneNum1().substring(1, 4), s.text(CactupaField.ACSPH1A));
        assertEquals(c.phoneNum1().substring(5, 8), s.text(CactupaField.ACSPH1B));
        assertEquals(c.phoneNum1().substring(9, 13), s.text(CactupaField.ACSPH1C));
        assertEquals("274", s.text(CactupaField.ACSTFCO));
        assertEquals("1961", s.text(CactupaField.DOBYEAR));
        assertEquals(BmsAttribute.DFHBMFSE, s.attribute(CactupaField.ACSTTUS));
        assertEquals(BmsAttribute.DFHBMPRF, s.attribute(CactupaField.ACSTNUM));
        assertEquals(BmsAttribute.DFHBMPRF, s.attribute(CactupaField.ACSCTRY));
        assertEquals(BmsAttribute.DFHBMPRF, s.attribute(CactupaField.ACCTSID));
        assertEquals(1, r.commarea().acctId);
        assertEquals(1, r.commarea().custId);
        assertEquals(9680294154603697L, r.commarea().cardNum);
        assertEquals(c.lastName().trim(), r.commarea().custLname.trim());
    }

    /** Gives the fixture account a non-blank ACCT-GROUP-ID so an unchanged echo compares equal. */
    void giveAccountAGroupId() {
        AccountRecord a = store.account(ACCT);
        store.replaceAccount(new AccountRecord(a.acctId(), a.activeStatus(), a.currBal(), a.creditLimit(),
                a.cashCreditLimit(), a.openDate(), a.expirationDate(), a.reissueDate(), a.currCycCredit(),
                a.currCycDebit(), a.addrZip(), "GOLD      ", a.filler()));
    }

    // 1100 moves LOW-VALUES for a blank AADDGRPI while the fixture's ACCT-GROUP-ID is SPACES; TRIM keeps
    // the nulls, so 1205 reports a change and 1200 goes on to edit every field (FICO 274 fails).
    @Test
    void blankGroupIdOnFixtureIsSeenAsAChange() {
        CicsResult r = change(fetch(), Map.of());
        assertEquals("FICO Score: should be between 300 and 850", err(r));
        assertTrue(r.programCommarea().isChangesNotOk());
    }

    // 1205-COMPARE-OLD-NEW: screen echoed unchanged
    @Test
    void unchangedScreenReportsNoChange() {
        giveAccountAGroupId();
        CicsResult r = change(fetch(), Map.of());
        assertEquals("No change detected with respect to values fetched.", err(r));
        assertEquals(CactupaField.ACSTTUS, r.screen().cursorField());
        assertTrue(r.programCommarea().isShowDetails());
    }

    // lower-case group id / status compare equal after UPPER-CASE
    @Test
    void caseOnlyDifferencesAreNotChanges() {
        giveAccountAGroupId();
        CicsResult f = fetch();
        CicsResult r = change(f, Map.of(CactupaField.ACSFNAM, f.screen().text(CactupaField.ACSFNAM).toUpperCase(),
                CactupaField.AADDGRP, "gold"));
        assertEquals("No change detected with respect to values fetched.", err(r));
    }

    // 1220-EDIT-YESNO + CSSETATY highlighting
    @Test
    void accountStatusMustBeYesOrNo() {
        CicsResult r = change(fetch(), Map.of(CactupaField.ACSTTUS, "X"));
        assertEquals("Account Status must be Y or N.", err(r));
        assertTrue(r.programCommarea().isChangesNotOk());
        assertEquals(CactupaField.ACSTTUS, r.screen().cursorField());
        assertEquals(BmsColor.DFHRED, r.screen().color(CactupaField.ACSTTUS));
        assertEquals(BmsAttribute.DFHBMASB, r.screen().attribute(CactupaField.FKEY12));
        assertEquals("X", r.screen().text(CactupaField.ACSTTUS));
    }

    @Test
    void blankAccountStatusShowsStar() {
        CicsResult r = change(fetch(), Map.of(CactupaField.ACSTTUS, " "));
        assertEquals("Account Status must be supplied.", err(r));
        assertEquals("*", r.screen().text(CactupaField.ACSTTUS));
    }

    // 1250-EDIT-SIGNED-9V2
    @Test
    void creditLimitBlankAndInvalid() {
        assertEquals("Credit Limit must be supplied.", err(change(fetch(), Map.of(CactupaField.ACRDLIM, "*"))));
        CicsResult r = change(fetch(), Map.of(CactupaField.ACRDLIM, "12A"));
        assertEquals("Credit Limit is not valid", err(r));
        assertEquals("12A", r.screen().text(CactupaField.ACRDLIM));
        assertEquals(CactupaField.ACRDLIM, r.screen().cursorField());
    }

    // EDIT-DATE-CCYYMMDD messages
    @Test
    void openDateEdits() {
        assertEquals("Open Date : Century is not valid.", err(change(fetch(), Map.of(CactupaField.OPNYEAR, "1814"))));
        assertEquals("Open Date must be 4 digit number.", err(change(fetch(), Map.of(CactupaField.OPNYEAR, "20X4"))));
        assertEquals("Open Date : Year must be supplied.", err(change(fetch(), Map.of(CactupaField.OPNYEAR, ""))));
        // JUSTIFY=(RIGHT): '*' arrives as "   *", so the "= '*'" blank test in 1100 does not fire
        assertEquals("Open Date must be 4 digit number.", err(change(fetch(), Map.of(CactupaField.OPNYEAR, "*"))));
        assertEquals("Open Date: Month must be a number between 1 and 12.",
                err(change(fetch(), Map.of(CactupaField.OPNMON, "13"))));
        assertEquals("Open Date : Month must be supplied.", err(change(fetch(), Map.of(CactupaField.OPNMON, ""))));
        assertEquals("Open Date:day must be a number between 1 and 31.",
                err(change(fetch(), Map.of(CactupaField.OPNDAY, "32"))));
        assertEquals("Open Date : Day must be supplied.", err(change(fetch(), Map.of(CactupaField.OPNDAY, ""))));
        assertEquals("Open Date:Cannot have 31 days in this month.",
                err(change(fetch(), Map.of(CactupaField.OPNMON, "04", CactupaField.OPNDAY, "31"))));
        assertEquals("Open Date:Cannot have 30 days in this month.",
                err(change(fetch(), Map.of(CactupaField.OPNMON, "02", CactupaField.OPNDAY, "30"))));
        assertEquals("Open Date:Not a leap year.Cannot have 29 days in this month.",
                err(change(fetch(), Map.of(CactupaField.OPNYEAR, "1900", CactupaField.OPNMON, "02",
                        CactupaField.OPNDAY, "29"))));
        CicsResult r = change(fetch(), Map.of(CactupaField.OPNMON, "13"));
        assertEquals(CactupaField.OPNMON, r.screen().cursorField());
    }

    @Test
    void expiryAndReissueDateEdits() {
        assertEquals("Expiry Date: Month must be a number between 1 and 12.",
                err(change(fetch(), Map.of(CactupaField.EXPMON, "00"))));
        assertEquals("Reissue Date:day must be a number between 1 and 31.",
                err(change(fetch(), Map.of(CactupaField.RISDAY, "AB"))));
    }

    // JUSTIFY=(RIGHT) date fields: a one-digit month arrives as " 2" and passes EDIT-MONTH
    @Test
    void oneDigitMonthIsRightJustified() {
        CicsResult r = change(fetch(), Map.of(CactupaField.OPNMON, "2", CactupaField.OPNDAY, "30"));
        assertEquals("Open Date:Cannot have 30 days in this month.", err(r));
        assertEquals(" 2", CactupaInput.empty().with(CactupaField.OPNMON, "2").get(CactupaField.OPNMON));
        assertEquals("AB   ", CactupaInput.empty().with(CactupaField.ACSZIPC, "AB").get(CactupaField.ACSZIPC));
    }

    // FUNCTION CURRENT-DATE digits are locale independent
    @Test
    void dateOfBirthCheckIgnoresDefaultLocale() {
        java.util.Locale saved = java.util.Locale.getDefault();
        try {
            java.util.Locale.setDefault(java.util.Locale.forLanguageTag("bn-BD-u-nu-beng"));
            CicsResult r = change(fetch(), Map.of(CactupaField.DOBYEAR, "2027"));
            assertEquals("Date of Birth:cannot be in the future", err(r));
            assertEquals("Phone Number 2: Not valid North America general purpose area code",
                    err(change(fetch(), Map.of(CactupaField.ACSTFCO, "700"))));
        } finally {
            java.util.Locale.setDefault(saved);
        }
    }

    // EDIT-DATE-OF-BIRTH
    @Test
    void dateOfBirthCannotBeInTheFuture() {
        CicsResult r = change(fetch(), Map.of(CactupaField.DOBYEAR, "2027"));
        assertEquals("Date of Birth:cannot be in the future", err(r));
        assertEquals(CactupaField.DOBYEAR, r.screen().cursorField());
    }

    // EDIT-DATE-LE: CSUTLDTC failure message; flags are reset to valid by the following sentence
    @Test
    void languageEnvironmentDateErrorMessage() {
        program = newProgram((d, f) -> new DateValidationResult(3, 2508, "Datevalue error"));
        CicsResult r = change(fetch(), validChange("2020"));
        assertEquals("Open Date validation error Sev code: 0003 Message code: 2508", err(r));
        assertNull(r.screen().color(CactupaField.OPNYEAR));
        assertTrue(r.programCommarea().isChangesNotOk());
    }

    // 1265-EDIT-US-SSN
    @Test
    void ssnEdits() {
        assertEquals("SSN: First 3 chars: should not be 000, 666, or between 900 and 999",
                err(change(fetch(), Map.of(CactupaField.ACTSSN1, "666"))));
        assertEquals("SSN: First 3 chars must not be zero.", err(change(fetch(), Map.of(CactupaField.ACTSSN1, "000"))));
        assertEquals("SSN 4th & 5th chars must be all numeric.", err(change(fetch(), Map.of(CactupaField.ACTSSN2, "9A"))));
        assertEquals("SSN Last 4 chars must be supplied.", err(change(fetch(), Map.of(CactupaField.ACTSSN3, "*"))));
    }

    // missing END-IF in 1265: parts 2/3 are not edited when part 1 fails 1245
    @Test
    void ssnParts2And3SkippedWhenPart1NotNumeric() {
        CicsResult r = change(fetch(), Map.of(CactupaField.ACTSSN1, "ABC", CactupaField.ACTSSN2, "*"));
        assertEquals("SSN: First 3 chars must be all numeric.", err(r));
        assertEquals(BmsColor.DFHRED, r.screen().color(CactupaField.ACTSSN1));
        assertNull(r.screen().color(CactupaField.ACTSSN2));
        assertEquals(CactupaField.ACTSSN1, r.screen().cursorField());
    }

    // 1245 + 1275 FICO (fixture customer has FICO 274)
    @Test
    void ficoMustBeInRange() {
        CicsResult r = change(fetch(), Map.of(CactupaField.ACSTTUS, "N"));
        assertEquals("FICO Score: should be between 300 and 850", err(r));
        assertEquals(CactupaField.ACSTFCO, r.screen().cursorField());
        assertEquals("FICO Score must be all numeric.", err(change(fetch(), Map.of(CactupaField.ACSTFCO, "7X0"))));
    }

    // 1225 / 1235 / 1215 name and address edits
    @Test
    void nameAndAddressEdits() {
        Map<CactupaField, String> base = Map.of(CactupaField.ACSTFCO, "700");
        assertEquals("First Name can have alphabets only.",
                err(change(fetch(), with(base, CactupaField.ACSFNAM, "J0hn"))));
        assertEquals("First Name must be supplied.", err(change(fetch(), with(base, CactupaField.ACSFNAM, "*"))));
        assertEquals("Middle Name can have alphabets only.",
                err(change(fetch(), with(base, CactupaField.ACSMNAM, "M."))));
        assertEquals("Last Name must be supplied.", err(change(fetch(), with(base, CactupaField.ACSLNAM, " "))));
        assertEquals("Address Line 1 must be supplied.", err(change(fetch(), with(base, CactupaField.ACSADL1, "*"))));
        assertEquals("City can have alphabets only.", err(change(fetch(), with(base, CactupaField.ACSCITY, "Town 9"))));
    }

    // 1270 / 1245 zip / 1280 state-zip
    @Test
    void stateAndZipEdits() {
        Map<CactupaField, String> base = Map.of(CactupaField.ACSTFCO, "700");
        assertEquals("State: is not a valid state code", err(change(fetch(), with(base, CactupaField.ACSSTTE, "XX"))));
        assertEquals("Zip must be all numeric.", err(change(fetch(), with(base, CactupaField.ACSZIPC, "1234A"))));
        CicsResult r = change(fetch(), with(base, CactupaField.ACSPH2A, "908"));
        assertEquals("Invalid zip code for state", err(r));
        assertEquals(BmsColor.DFHRED, r.screen().color(CactupaField.ACSSTTE));
        assertEquals(BmsColor.DFHRED, r.screen().color(CactupaField.ACSZIPC));
    }

    // 1260-EDIT-US-PHONE-NUM
    @Test
    void phoneEdits() {
        Map<CactupaField, String> base = Map.of(CactupaField.ACSTFCO, "700");
        assertEquals("Phone Number 1: Area code cannot be zero",
                err(change(fetch(), with(base, CactupaField.ACSPH1A, "000"))));
        assertEquals("Phone Number 1: Area code must be A 3 digit number.",
                err(change(fetch(), with(base, CactupaField.ACSPH1A, "9X8"))));
        assertEquals("Phone Number 1: Not valid North America general purpose area code",
                err(change(fetch(), with(base, CactupaField.ACSPH1A, "211"))));
        assertEquals("Phone Number 1: Prefix code must be supplied.",
                err(change(fetch(), with(base, CactupaField.ACSPH1B, ""))));
        assertEquals("Phone Number 1: Line number code cannot be zero",
                err(change(fetch(), with(base, CactupaField.ACSPH1C, "0000"))));
        assertEquals("Phone Number 2: Not valid North America general purpose area code",
                err(change(fetch(), base)));
    }

    // all-blank phone is accepted (the third test checks NUMA, not NUMC, against SPACES)
    @Test
    void blankPhoneNumberIsAccepted() {
        Map<CactupaField, String> m = new java.util.EnumMap<>(validChange("2020"));
        m.put(CactupaField.ACSPH2A, "");
        m.put(CactupaField.ACSPH2B, "");
        m.put(CactupaField.ACSPH2C, "");
        CicsResult r = change(fetch(), m);
        assertEquals("", err(r));
        assertTrue(r.programCommarea().isChangesOkNotConfirmed());
    }

    // 1245 EFT, 1220 primary card holder
    @Test
    void eftAndPrimaryHolderEdits() {
        Map<CactupaField, String> base = validChange("2020");
        assertEquals("EFT Account Id must not be zero.",
                err(change(fetch(), with(base, CactupaField.ACSEFTC, "0000000000"))));
        assertEquals("Primary Card Holder must be Y or N.",
                err(change(fetch(), with(base, CactupaField.ACSPFLG, "y"))));
    }

    // Happy path: validate -> PF5 -> 9600 REWRITE -> C; next ENTER resets
    @Test
    void validChangeIsConfirmedAndCommitted() {
        CustomerRecord before = store.customer("000000001");
        CicsResult validated = change(fetch(), validChange("3,000.50"));
        assertEquals("", err(validated));
        assertTrue(validated.programCommarea().isChangesOkNotConfirmed());
        assertEquals("Changes validated.Press F5 to save", info(validated));
        assertEquals("+      3,000.50", validated.screen().text(CactupaField.ACRDLIM));
        assertEquals(BmsAttribute.DFHBMASB, validated.screen().attribute(CactupaField.FKEY05));
        assertEquals(BmsAttribute.DFHBMPRF, validated.screen().attribute(CactupaField.ACSTTUS));

        CicsResult saved = program.execute(validated.next(AidKey.DFHPF5, echo(validated.screen(), Map.of())));
        assertTrue(saved.programCommarea().isChangesOkayedAndDone());
        assertEquals("Changes committed to database", info(saved));

        AccountRecord a = store.account(ACCT);
        assertEquals(new BigDecimal("3000.50"), a.creditLimit());
        assertEquals(new BigDecimal("194.00"), a.currBal());
        assertEquals("2014-11-20", a.openDate());
        // ACCT-UPDATE-RECORD has no ACCT-ADDR-ZIP: the (blank -> LOW-VALUES) group id lands in the ADDR-ZIP slot
        assertEquals("\0".repeat(10), a.addrZip());
        assertEquals("          ", a.groupId());
        CustomerRecord c = store.customer("000000001");
        assertEquals(700, c.ficoCreditScore());
        assertEquals("27514     ", c.addrZip());
        assertEquals(before.phoneNum1(), c.phoneNum1());
        assertEquals("(908)" + before.phoneNum2().substring(5), c.phoneNum2());
        assertEquals(before.dobYyyyMmDd(), c.dobYyyyMmDd());
        assertEquals(before.ssn(), c.ssn());

        CicsResult reset = program.execute(saved.next(AidKey.DFHENTER, CactupaInput.empty()));
        assertTrue(reset.programCommarea().isDetailsNotFetched());
        assertEquals("Enter or update id of account to update", info(reset));
        assertEquals(0, reset.commarea().acctId);
    }

    // PF5 is only valid in state N; elsewhere it is treated as ENTER
    @Test
    void pf5BeforeValidationActsAsEnter() {
        giveAccountAGroupId();
        CicsResult r = program.execute(fetch().next(AidKey.DFHPF5, echo(fetch().screen(), Map.of())));
        assertEquals("No change detected with respect to values fetched.", err(r));
    }

    // PF12 re-reads the account, discarding edits
    @Test
    void pf12DiscardsChanges() {
        CicsResult bad = change(fetch(), Map.of(CactupaField.ACSTTUS, "X"));
        CicsResult r = program.execute(bad.next(AidKey.DFHPF12, echo(bad.screen(), Map.of())));
        assertTrue(r.programCommarea().isShowDetails());
        assertEquals("Y", r.screen().text(CactupaField.ACSTTUS));
        assertEquals("", err(r));
    }

    CicsResult validatedChange() {
        return change(fetch(), validChange("3000"));
    }

    CicsResult pf5(CicsResult validated) {
        return program.execute(validated.next(AidKey.DFHPF5, echo(validated.screen(), Map.of())));
    }

    @Test
    void accountLockFailure() {
        CicsResult v = validatedChange();
        store.forceNext(Operation.ACCT_READ_UPDATE, CicsResp.NOTFND, 0);
        CicsResult r = pf5(v);
        assertTrue(r.programCommarea().isChangesOkayedLockError());
        assertEquals("Could not lock account record for update", err(r));
        assertEquals("Changes unsuccessful. Please try again", info(r));
        CicsResult reset = program.execute(r.next(AidKey.DFHENTER, CactupaInput.empty()));
        assertTrue(reset.programCommarea().isDetailsNotFetched());
    }

    // COULD-NOT-LOCK-CUST-FOR-UPDATE is not tested in 2000-DECIDE-ACTION: falls to WHEN OTHER
    @Test
    void customerLockFailureIsReportedAsCommitted() {
        CicsResult v = validatedChange();
        store.forceNext(Operation.CUST_READ_UPDATE, CicsResp.LOCKED, 0);
        CicsResult r = pf5(v);
        assertTrue(r.programCommarea().isChangesOkayedAndDone());
        assertEquals("Could not lock customer record for update", err(r));
        assertEquals("Changes committed to database", info(r));
        assertEquals(new BigDecimal("2020.00"), store.account(ACCT).creditLimit());
    }

    @Test
    void accountRewriteFailure() {
        CicsResult v = validatedChange();
        store.forceNext(Operation.ACCT_REWRITE, CicsResp.IOERR, 0);
        CicsResult r = pf5(v);
        assertTrue(r.programCommarea().isChangesOkayedButFailed());
        assertEquals("Update of record failed", err(r));
    }

    @Test
    void customerRewriteFailureRollsBackAccount() {
        CicsResult v = validatedChange();
        int rollbacks = store.rollbackCount();
        store.forceNext(Operation.CUST_REWRITE, CicsResp.IOERR, 0);
        CicsResult r = pf5(v);
        assertTrue(r.programCommarea().isChangesOkayedButFailed());
        assertEquals(rollbacks + 1, store.rollbackCount());
        assertEquals(new BigDecimal("2020.00"), store.account(ACCT).creditLimit());
    }

    // 9700-CHECK-CHANGE-IN-REC
    @Test
    void concurrentChangeIsDetected() {
        CicsResult v = validatedChange();
        AccountRecord a = store.account(ACCT);
        store.replaceAccount(new AccountRecord(a.acctId(), a.activeStatus(), new BigDecimal("999.99"), a.creditLimit(),
                a.cashCreditLimit(), a.openDate(), a.expirationDate(), a.reissueDate(), a.currCycCredit(),
                a.currCycDebit(), a.addrZip(), a.groupId(), a.filler()));
        CicsResult r = pf5(v);
        assertTrue(r.programCommarea().isShowDetails());
        assertEquals("Record changed by some one else. Please review", err(r));
    }

    // 2000 WHEN OTHER -> ABEND-ROUTINE
    @Test
    void unexpectedStateAbends() {
        giveAccountAGroupId();
        CicsResult shown = fetch();
        AcctUpdateCommarea p = shown.programCommarea().copy();
        p.setChangeAction('X');
        CicsResult r = program.execute(new CicsRequest(AidKey.DFHENTER, shown.commarea(), p,
                echo(shown.screen(), Map.of())));
        assertEquals(CicsResult.Kind.ABEND, r.kind());
        assertEquals("9999", r.abendCode());
        assertEquals("0001", r.abendData().abendCode);
        assertEquals("COACTUPC", r.abendData().abendCulprit);
        assertEquals("UNEXPECTED DATA SCENARIO", r.abendData().abendMsg.trim());
    }

    // HANDLE ABEND: ABEND-MSG is SPACES (not LOW-VALUES), so the default text is not moved
    @Test
    void runtimeFailureGoesToAbendRoutine() {
        program = new Coactupc(acct -> {
            throw new IllegalStateException("boom");
        }, store.accounts(), store.customers(), store, new LeDateValidator(), CLOCK);
        CicsResult r = program.execute(start().next(AidKey.DFHENTER, CactupaInput.empty().with(CactupaField.ACCTSID, ACCT)));
        assertEquals(CicsResult.Kind.ABEND, r.kind());
        assertEquals("9999", r.abendCode());
        assertEquals("", r.abendData().abendMsg.trim());
    }

    // invalid AID (PF7) is treated as ENTER
    @Test
    void invalidKeyActsAsEnter() {
        CicsResult r = program.execute(start().next(AidKey.DFHPF7, CactupaInput.empty().with(CactupaField.ACCTSID, ACCT)));
        assertTrue(r.programCommarea().isShowDetails());
    }

    // transfer from the menu with commarea -> fresh screen
    @Test
    void entryFromMenuShowsSearchScreen() {
        CardDemoCommarea c = CardDemoCommarea.initialized();
        c.fromProgram = "COMEN01C";
        CicsResult r = program.execute(CicsRequest.fromProgram(c));
        assertEquals("Enter or update id of account to update", info(r));
        assertTrue(r.commarea().isPgmReenter());
    }

    static Map<CactupaField, String> with(Map<CactupaField, String> base, CactupaField f, String v) {
        Map<CactupaField, String> m = new java.util.EnumMap<>(CactupaField.class);
        m.putAll(base);
        m.put(f, v);
        return m;
    }
}
