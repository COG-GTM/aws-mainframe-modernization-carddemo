package com.carddemo.online.coactupc;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.common.copybook.AccountRecord;
import com.carddemo.common.copybook.CardDemoCommarea;
import com.carddemo.common.copybook.CardXrefRecord;
import com.carddemo.common.copybook.CustomerRecord;
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
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import org.junit.jupiter.api.Test;

/**
 * End-to-end pseudo-conversations through COACTUPC (CAUP). Line numbers refer to app/cbl/COACTUPC.cbl. Seed data:
 * app/data/ASCII/acctdata.txt line 1 (account 00000000001, balance 194.00, credit limit 2020.00, opened
 * 2014-11-20), cardxref.txt line 49 (card 9680294154603697 -> customer 000000001 / account 00000000001) and
 * custdata.txt line 1 (Immanuel Madeline Kessler, NC 12546, phones (908)119-8310 / (373)693-8684, SSN 020973888).
 */
class CoactupcFlowTest {

    private static final Clock CLOCK = Clock.fixed(Instant.parse("2024-03-05T14:07:09.25Z"), ZoneOffset.UTC);
    private static final String ACCT = "00000000001";
    private static final String CUST = "000000001";

    /** Every unprotected / FSET field 1100-RECEIVE-MAP reads (lines 1039-1330). */
    private static final List<String> INPUT_FIELDS = List.of("ACCTSID", "ACSTTUS", "ACRDLIM", "ACSHLIM", "ACURBAL",
            "ACRCYCR", "ACRCYDB", "OPNYEAR", "OPNMON", "OPNDAY", "EXPYEAR", "EXPMON", "EXPDAY", "RISYEAR", "RISMON",
            "RISDAY", "AADDGRP", "ACSTNUM", "ACTSSN1", "ACTSSN2", "ACTSSN3", "DOBYEAR", "DOBMON", "DOBDAY",
            "ACSTFCO", "ACSFNAM", "ACSMNAM", "ACSLNAM", "ACSADL1", "ACSADL2", "ACSCITY", "ACSSTTE", "ACSCTRY",
            "ACSZIPC", "ACSPH1A", "ACSPH1B", "ACSPH1C", "ACSPH2A", "ACSPH2B", "ACSPH2C", "ACSGOVT", "ACSEFTC",
            "ACSPFLG");

    /** In-memory store whose updates can be made to fail (REWRITE then returns NOTFND, as a lost record would). */
    static final class FlakyStore implements KsdsStore {
        final InMemoryKsdsStore delegate = new InMemoryKsdsStore();
        boolean failUpdates;

        @Override public Optional<byte[]> get(byte[] key) { return delegate.get(key); }
        @Override public boolean insert(byte[] key, byte[] record) { return delegate.insert(key, record); }
        @Override public boolean update(byte[] key, byte[] record) {
            return !failUpdates && delegate.update(key, record);
        }
        @Override public boolean delete(byte[] key) { return delegate.delete(key); }
        @Override public Optional<Map.Entry<byte[], byte[]>> ceiling(byte[] key) { return delegate.ceiling(key); }
        @Override public Optional<Map.Entry<byte[], byte[]>> higher(byte[] key) { return delegate.higher(key); }
        @Override public Optional<Map.Entry<byte[], byte[]>> lower(byte[] key) { return delegate.lower(key); }
        @Override public Optional<Map.Entry<byte[], byte[]>> last() { return delegate.last(); }
        @Override public List<Map.Entry<byte[], byte[]>> all() { return delegate.all(); }
        @Override public long count() { return delegate.count(); }
        @Override public void clear() { delegate.clear(); }
    }

    private final Coactupc program = new Coactupc();
    private final FlakyStore acctStore = new FlakyStore();
    private final FlakyStore custStore = new FlakyStore();
    private final KsdsFile acct = new KsdsFile(AccountRecord.FILE_NAME, AccountRecord.LENGTH, 0, 11, acctStore);
    private final KsdsFile cust = new KsdsFile(CustomerRecord.FILE_NAME, CustomerRecord.LENGTH, 0, 9, custStore);
    private final KsdsFile xref = KsdsFile.inMemory(CardXrefRecord.FILE_NAME, CardXrefRecord.LENGTH, 0, 16);
    private final FileControl fct = new FileControl().register(acct).register(cust).register(xref);
    private byte[] commarea;
    private BmsScreen screen;

    CoactupcFlowTest() {
        acct.load(AccountRecord.seed());
        cust.load(CustomerRecord.seed());
        xref.load(CardXrefRecord.seed());
        fct.register(xref.path(CardXrefRecord.ACCT_PATH_NAME, CardXrefRecord.ACCT_AIX_OFFSET,
                CardXrefRecord.ACCT_AIX_LENGTH, false));
    }

    private CicsResponse run(AidKey aid, Map<String, String> fields) {
        CicsResponse r = CicsDispatcher.run(program, CicsContext.builder().transid("CAUP").aid(aid).commarea(commarea)
                .terminalInput(fields).fileControl(fct).clock(CLOCK).systemInfo(SystemInfo.defaults()).build());
        if (r.outcome().type() == CicsOutcome.Type.RETURN_TRANSID) {
            // COMMON-RETURN lines 1007-1019: RETURN TRANSID(CAUP) COMMAREA(WS-COMMAREA)
            assertThat(r.outcome().nextTransid()).isEqualTo("CAUP");
            commarea = r.outcome().commarea();
        }
        if (r.lastMap() != null) {
            screen = r.lastMap().screen();
        }
        return r;
    }

    private Map<String, String> form() {
        Map<String, String> m = new LinkedHashMap<>();
        for (String f : INPUT_FIELDS) {
            m.put(f, PicX.rtrim(screen.value(f)));
        }
        return m;
    }

    private String err() {
        return PicX.rtrim(screen.value("ERRMSG"));
    }

    private String info() {
        return screen.value("INFOMSG").trim();
    }

    private void fetchAccount1() {
        run(AidKey.ENTER, Map.of());
        run(AidKey.ENTER, Map.of("ACCTSID", ACCT));
    }

    /** A change that passes every 1200 edit: customer 1 has FICO 274, NC/12546 and area code 373, all rejected. */
    private Map<String, String> validChange() {
        Map<String, String> m = form();
        m.put("ACSTFCO", "700");
        m.put("ACSFNAM", "Jane");
        m.put("ACSSTTE", "NY");
        m.put("ACSZIPC", "10001");
        m.put("ACSPH2A", "212");
        return m;
    }

    private void validateChange(Map<String, String> m) {
        run(AidKey.ENTER, m);
        assertThat(info()).isEqualTo(Coactupc.PROMPT_FOR_CONFIRMATION);
    }

    private String acctRecord() {
        return acct.read(ACCT).dataString();
    }

    private String custRecord() {
        return cust.read(CUST).dataString();
    }

    @Test
    void firstEntryPromptsForAccountId() {
        // 0000-MAIN lines 873-890 (EIBCALEN 0) then 3250-SETUP-INFOMSG line 2683: PROMPT-FOR-SEARCH-KEYS
        CicsResponse r = run(AidKey.ENTER, Map.of());
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.RETURN_TRANSID);
        assertThat(info()).isEqualTo(Coactupc.PROMPT_FOR_SEARCH_KEYS);
        assertThat(err()).isEmpty();
        assertThat(screen.value("TRNNAME")).isEqualTo("CAUP");
        assertThat(screen.value("PGMNAME").trim()).isEqualTo("COACTUPC");
    }

    @Test
    void blankAccountIdIsRejected() {
        // 1210-EDIT-ACCOUNT lines 1789-1800 sets WS-PROMPT-FOR-ACCT, then 1200 lines 1441-1442 unconditionally
        // SETs NO-SEARCH-CRITERIA-RECEIVED over it
        run(AidKey.ENTER, Map.of());
        run(AidKey.ENTER, Map.of("ACCTSID", ""));
        assertThat(err()).isEqualTo(Coactupc.NO_SEARCH_CRITERIA_RECEIVED);
    }

    @Test
    void nonNumericOrZeroAccountIdIsRejected() {
        // 1210-EDIT-ACCOUNT lines 1804-1816
        run(AidKey.ENTER, Map.of());
        run(AidKey.ENTER, Map.of("ACCTSID", "12345abcdef"));
        assertThat(err()).isEqualTo("Account Number if supplied must be a 11 digit Non-Zero Number");
        run(AidKey.ENTER, Map.of("ACCTSID", "00000000000"));
        assertThat(err()).isEqualTo("Account Number if supplied must be a 11 digit Non-Zero Number");
    }

    @Test
    void unknownAccountIsNotFoundInCrossReference() {
        // 9200-GETCARDXREF-BYACCT NOTFND lines 3668-3685 (RESP 13 = NOTFND)
        run(AidKey.ENTER, Map.of());
        run(AidKey.ENTER, Map.of("ACCTSID", "99999999999"));
        assertThat(err()).startsWith("Account:99999999999 not found in Cross ref file.  Resp:");
        assertThat(screen.value("ACSFNAM").trim()).isEmpty();
    }

    @Test
    void accountInXrefButMissingFromAccountMaster() {
        // 9300-GETACCTDATA-BYACCT NOTFND lines 3716-3734
        acct.store().delete(PicX.toBytes(ACCT));
        fetchAccount1();
        assertThat(err()).startsWith("Account:00000000001 not found in Acct Master file.Resp:");
    }

    @Test
    void customerMissingFromCustomerMaster() {
        // 9400-GETCUSTDATA-BYCUST NOTFND lines 3766-3784
        cust.store().delete(PicX.toBytes(CUST));
        fetchAccount1();
        assertThat(err()).startsWith("CustId:000000001 not found in customer master.Resp: ");
    }

    @Test
    void validAccountShowsAccountAndCustomerDetails() {
        // 9500-STORE-FETCHED-DATA lines 3801-3884, 3202-SHOW-ORIGINAL-VALUES lines 2512-2589
        fetchAccount1();
        assertThat(err()).isEmpty();
        assertThat(info()).isEqualTo(Coactupc.PROMPT_FOR_CHANGES);
        assertThat(screen.value("ACCTSID")).isEqualTo(ACCT);
        assertThat(screen.value("ACSTTUS")).isEqualTo("Y");
        assertThat(screen.value("ACURBAL").trim()).endsWith("194.00");
        assertThat(screen.value("ACRDLIM").trim()).endsWith("2,020.00");
        assertThat(screen.value("OPNYEAR") + screen.value("OPNMON") + screen.value("OPNDAY")).isEqualTo("20141120");
        assertThat(screen.value("ACSTNUM")).isEqualTo(CUST);
        assertThat(screen.value("ACTSSN1") + screen.value("ACTSSN2") + screen.value("ACTSSN3"))
                .isEqualTo("020973888");
        assertThat(screen.value("ACSFNAM").trim()).isEqualTo("Immanuel");
        assertThat(screen.value("ACSLNAM").trim()).isEqualTo("Kessler");
        assertThat(screen.value("ACSSTTE")).isEqualTo("NC");
        assertThat(screen.value("ACSPH1A") + screen.value("ACSPH1B") + screen.value("ACSPH1C"))
                .isEqualTo("9081198310");
        CardDemoCommarea c = CardDemoCommarea.of(commarea);
        assertThat(CardDemoCommarea.CARD_NUM.raw(c)).isEqualTo("9680294154603697");
    }

    private void setGroupId(String group) {
        AccountRecord a = AccountRecord.of(PicX.toBytes(acctRecord()));
        AccountRecord.GROUP_ID.set(a, group);
        acctStore.delegate.update(PicX.toBytes(ACCT), a.toBytes());
    }

    @Test
    void blankGroupIdIsNeverSeenAsUnchanged() {
        // COBOL oddity: 1100 lines 1213-1218 move LOW-VALUES for a blank AADDGRP, but ACUP-OLD-GROUP-ID holds
        // SPACES; FUNCTION TRIM (1205 lines 1697-1700) only strips spaces, so the compare fails and the edits run.
        fetchAccount1();
        run(AidKey.ENTER, form());
        assertThat(err()).isEqualTo("FICO Score: should be between 300 and 850");
    }

    @Test
    void unchangedDataIsReportedAsNoChange() {
        // 1205-COMPARE-OLD-NEW lines 1681-1773 -> NO-CHANGES-DETECTED (line 1430); group made non-blank (see above)
        setGroupId("GOLD");
        fetchAccount1();
        run(AidKey.ENTER, form());
        assertThat(err()).isEqualTo(Coactupc.NO_CHANGES_DETECTED);
        assertThat(info()).isEqualTo(Coactupc.PROMPT_FOR_CHANGES);
    }

    @Test
    void invalidEditStopsAtFirstErrorAndKeepsShowingDetails() {
        // 1200: first failing edit owns WS-RETURN-MSG; here the state/zip cross edit (1280, lines 2496-2516)
        fetchAccount1();
        Map<String, String> m = form();
        m.put("ACSFNAM", "Jane");
        m.put("ACSTFCO", "700");
        m.put("ACSPH2A", "212");
        run(AidKey.ENTER, m);
        assertThat(err()).isEqualTo("Invalid zip code for state");
        assertThat(info()).isEqualTo(Coactupc.PROMPT_FOR_CHANGES);
    }

    @Test
    void pf5BeforeValidationIsTreatedAsEnter() {
        // 0000-MAIN lines 920-931: PF05 is only valid when ACUP-CHANGES-OK-NOT-CONFIRMED
        fetchAccount1();
        String before = custRecord();
        run(AidKey.PF5, validChange());
        assertThat(info()).isEqualTo(Coactupc.PROMPT_FOR_CONFIRMATION);
        assertThat(custRecord()).isEqualTo(before);
    }

    @Test
    void validatedChangesAreCommittedOnPf5() {
        // 2000-DECIDE-ACTION lines 2562-2590 then 9600-WRITE-PROCESSING lines 3888-4103
        fetchAccount1();
        String acctBefore = acctRecord();
        validateChange(validChange());
        // 3310-PROTECT-ALL-ATTRS: DFHBMPRF while awaiting confirmation
        assertThat(screen.field("ACSFNAM").orElseThrow().attributeByte()).isEqualTo(0x61);
        run(AidKey.PF5, form());
        assertThat(info()).isEqualTo(Coactupc.CONFIRM_UPDATE_SUCCESS);
        String c = custRecord();
        assertThat(CustomerRecord.FIRST_NAME.get(CustomerRecord.of(PicX.toBytes(c))).trim()).isEqualTo("Jane");
        assertThat(CustomerRecord.ADDR_STATE_CD.get(CustomerRecord.of(PicX.toBytes(c)))).isEqualTo("NY");
        assertThat(CustomerRecord.ADDR_ZIP.get(CustomerRecord.of(PicX.toBytes(c))).trim()).isEqualTo("10001");
        // lines 4027-4041: phones rebuilt as '(' aaa ')' bbb '-' cccc
        assertThat(CustomerRecord.PHONE_NUM_2.get(CustomerRecord.of(PicX.toBytes(c)))).isEqualTo("(212)693-8684  ");
        assertThat(CustomerRecord.PHONE_NUM_1.get(CustomerRecord.of(PicX.toBytes(c)))).isEqualTo("(908)119-8310  ");
        // the account was rewritten from unchanged screen values. COBOL oddity: ACCT-UPDATE-RECORD (lines 418-433)
        // has no ADDR-ZIP, so ACCT-UPDATE-GROUP-ID lands on ACCT-ADDR-ZIP's offset. The blank group was received as
        // LOW-VALUES (1100 lines 1213-1218), so 'A000000000' (acctdata.txt line 1) becomes LOW-VALUES.
        assertThat(acctRecord().substring(0, 102)).isEqualTo(acctBefore.substring(0, 102));
        assertThat(acctBefore.substring(102, 112)).isEqualTo("A000000000");
        assertThat(acctRecord().substring(102, 112)).isEqualTo(PicX.lowValues(10));
        // next ENTER after a successful update starts over (0000-MAIN lines 961-972)
        run(AidKey.ENTER, Map.of());
        assertThat(info()).isEqualTo(Coactupc.PROMPT_FOR_SEARCH_KEYS);
    }

    @Test
    void changeByAnotherUserIsDetected() {
        // 9700-CHECK-CHANGE-IN-REC lines 4115-4145 -> DATA-WAS-CHANGED-BEFORE-UPDATE
        fetchAccount1();
        validateChange(validChange());
        AccountRecord a = AccountRecord.of(PicX.toBytes(acctRecord()));
        AccountRecord.CURR_BAL.set(a, new java.math.BigDecimal("999.99"));
        acctStore.delegate.update(PicX.toBytes(ACCT), a.toBytes());
        String custBefore = custRecord();
        run(AidKey.PF5, form());
        assertThat(err()).isEqualTo(Coactupc.DATA_WAS_CHANGED_BEFORE_UPDATE);
        assertThat(custRecord()).isEqualTo(custBefore);
        assertThat(AccountRecord.CURR_BAL.get(AccountRecord.of(PicX.toBytes(acctRecord())))).isEqualByComparingTo("999.99");
    }

    @Test
    void customerChangedByAnotherUserIsDetected() {
        // 9700-CHECK-CHANGE-IN-REC lines 4152-4191 (case-insensitive name compare)
        fetchAccount1();
        validateChange(validChange());
        CustomerRecord c = CustomerRecord.of(PicX.toBytes(custRecord()));
        CustomerRecord.FIRST_NAME.set(c, "IMMANUEL");
        custStore.delegate.update(PicX.toBytes(CUST), c.toBytes());
        run(AidKey.PF5, form());
        assertThat(err()).isEmpty();
        assertThat(info()).isEqualTo(Coactupc.CONFIRM_UPDATE_SUCCESS);

        fetchAccount1();
        Map<String, String> again = validChange();
        again.put("ACSFNAM", "Janet");
        validateChange(again);
        c = CustomerRecord.of(PicX.toBytes(custRecord()));
        CustomerRecord.FICO_CREDIT_SCORE.set(c, 301);
        custStore.delegate.update(PicX.toBytes(CUST), c.toBytes());
        run(AidKey.PF5, form());
        assertThat(err()).isEqualTo(Coactupc.DATA_WAS_CHANGED_BEFORE_UPDATE);
    }

    @Test
    void accountThatCannotBeReadForUpdateFailsTheLock() {
        // 9600 lines 3907-3915: COULD-NOT-LOCK-ACCT-FOR-UPDATE; 2000 sets ACUP-CHANGES-OKAYED-LOCK-ERROR
        fetchAccount1();
        validateChange(validChange());
        acctStore.delegate.delete(PicX.toBytes(ACCT));
        run(AidKey.PF5, form());
        assertThat(err()).isEqualTo(Coactupc.COULD_NOT_LOCK_ACCT_FOR_UPDATE);
        assertThat(info()).isEqualTo(Coactupc.INFORM_FAILURE);
    }

    @Test
    void customerThatCannotBeReadForUpdateFailsTheLock() {
        // 9600 lines 3934-3942: COULD-NOT-LOCK-CUST-FOR-UPDATE. COBOL oddity: the 2000 EVALUATE (lines 2607-2614)
        // has no WHEN for it, so WHEN OTHER sets ACUP-CHANGES-OKAYED-AND-DONE and the success info message shows
        fetchAccount1();
        String acctBefore = acctRecord();
        validateChange(validChange());
        custStore.delegate.delete(PicX.toBytes(CUST));
        run(AidKey.PF5, form());
        assertThat(err()).isEqualTo(Coactupc.COULD_NOT_LOCK_CUST_FOR_UPDATE);
        assertThat(info()).isEqualTo(Coactupc.CONFIRM_UPDATE_SUCCESS);
        assertThat(acctRecord()).isEqualTo(acctBefore);
    }

    @Test
    void accountRewriteFailureLeavesCustomerUntouched() {
        // 9600 lines 4076-4081: LOCKED-BUT-UPDATE-FAILED, no SYNCPOINT ROLLBACK on this branch
        fetchAccount1();
        validateChange(validChange());
        String custBefore = custRecord();
        acctStore.failUpdates = true;
        run(AidKey.PF5, form());
        assertThat(err()).isEqualTo(Coactupc.LOCKED_BUT_UPDATE_FAILED);
        assertThat(info()).isEqualTo(Coactupc.INFORM_FAILURE);
        assertThat(custRecord()).isEqualTo(custBefore);
    }

    private Map<String, String> creditLimitAndCustomerChange() {
        Map<String, String> m = validChange();
        m.put("ACRDLIM", "3000.00");
        return m;
    }

    @Test
    void customerRewriteFailureRollsBackRecoverableAccountUpdate() {
        // 9600 lines 4095-4103: LOCKED-BUT-UPDATE-FAILED + EXEC CICS SYNCPOINT ROLLBACK
        acct.setRecoverable(true);
        fetchAccount1();
        validateChange(creditLimitAndCustomerChange());
        custStore.failUpdates = true;
        run(AidKey.PF5, form());
        assertThat(err()).isEqualTo(Coactupc.LOCKED_BUT_UPDATE_FAILED);
        assertThat(AccountRecord.CREDIT_LIMIT.get(AccountRecord.of(PicX.toBytes(acctRecord())))).isEqualByComparingTo("2020.00");
    }

    @Test
    void customerRewriteFailureCannotBackOutNonRecoverableAccount() {
        // CSD DEFINE FILE(ACCTDAT) RECOVERY(NONE): SYNCPOINT ROLLBACK has nothing to back out
        fetchAccount1();
        validateChange(creditLimitAndCustomerChange());
        custStore.failUpdates = true;
        run(AidKey.PF5, form());
        assertThat(err()).isEqualTo(Coactupc.LOCKED_BUT_UPDATE_FAILED);
        assertThat(AccountRecord.CREDIT_LIMIT.get(AccountRecord.of(PicX.toBytes(acctRecord())))).isEqualByComparingTo("3000.00");
        // after a failed update the next ENTER starts over (0000-MAIN lines 961-972)
        run(AidKey.ENTER, Map.of());
        assertThat(info()).isEqualTo(Coactupc.PROMPT_FOR_SEARCH_KEYS);
    }

    @Test
    void pf12DiscardsChangesAndRefetches() {
        // 2000-DECIDE-ACTION lines 2536-2546: PFK12 re-reads the account
        fetchAccount1();
        Map<String, String> m = validChange();
        run(AidKey.ENTER, m);
        run(AidKey.PF12, m);
        assertThat(screen.value("ACSFNAM").trim()).isEqualTo("Immanuel");
        assertThat(info()).isEqualTo(Coactupc.PROMPT_FOR_CHANGES);
    }

    @Test
    void pf3ReturnsToTheMainMenu() {
        // 0000-MAIN lines 936-956: XCTL to CDEMO-FROM-PROGRAM, or COMEN01C when blank
        fetchAccount1();
        CicsResponse r = run(AidKey.PF3, Map.of());
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.XCTL);
        assertThat(r.outcome().nextProgram()).isEqualTo("COMEN01C");
        CardDemoCommarea c = CardDemoCommarea.of(r.outcome().commarea());
        assertThat(c.fromProgram()).isEqualTo("COACTUPC");
        assertThat(c.fromTranid()).isEqualTo("CAUP");
        assertThat(c.toTranid()).isEqualTo("CM00");
    }

    @Test
    void pf15FoldsToPf3() {
        // CSSTRPFY: PF15 -> PFK03
        run(AidKey.ENTER, Map.of());
        assertThat(run(AidKey.PF15, Map.of()).outcome().nextProgram()).isEqualTo("COMEN01C");
    }

    @Test
    void unsupportedKeyIsTreatedAsEnter() {
        // 0000-MAIN lines 920-931: invalid AID -> ENTER
        run(AidKey.ENTER, Map.of());
        run(AidKey.PF7, Map.of("ACCTSID", ACCT));
        assertThat(screen.value("ACSFNAM").trim()).isEqualTo("Immanuel");
    }
}
