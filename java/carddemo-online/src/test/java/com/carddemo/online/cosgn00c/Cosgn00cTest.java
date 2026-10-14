package com.carddemo.online.cosgn00c;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.common.copybook.CardDemoCommarea;
import com.carddemo.common.copybook.UserSecurityRecord;
import com.carddemo.common.copybook.UserSecuritySeed;
import com.carddemo.runtime.bms.BmsScreen;
import com.carddemo.runtime.cics.AidKey;
import com.carddemo.runtime.cics.CicsContext;
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
import java.util.Map;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

/**
 * Behavioural parity tests for COSGN00C. Line numbers refer to app/cbl/COSGN00C.cbl; user data is the
 * USRSEC seed (app/data/EBCDIC/AWS.M2.CARDDEMO.USRSEC.PS = DUSRSECJ.jcl lines 35-44): ADMIN001-005 type A,
 * USER0001-0005 type U, all with SEC-USR-PWD = 'PASSWORD' (the 'A'/'U' after it is SEC-USR-TYPE).
 */
class Cosgn00cTest {

    /** 2024-03-05 14:07:09.25 UTC: CURDATE 03/05/24, CURTIME 14:07:09 (POPULATE-HEADER-INFO lines 179-196). */
    private static final Clock CLOCK = Clock.fixed(Instant.parse("2024-03-05T14:07:09.25Z"), ZoneOffset.UTC);
    private static final String LOW8 = PicX.lowValues(8);

    private final Cosgn00c program = new Cosgn00c();
    private final KsdsFile usrsec = KsdsFile.inMemory("USRSEC", UserSecurityRecord.LENGTH, 0, 8);
    private final FileControl fct = new FileControl().register(usrsec);

    Cosgn00cTest() {
        usrsec.load(UserSecuritySeed.records());
    }

    private CicsResponse run(AidKey aid, byte[] commarea, Map<String, String> fields) {
        return CicsDispatcher.run(program, CicsContext.builder().transid("CC00").aid(aid).commarea(commarea)
                .terminalInput(fields).fileControl(fct).clock(CLOCK).systemInfo(SystemInfo.defaults()).build());
    }

    /** Any non-empty commarea: COSGN00C only tests EIBCALEN, it never reads DFHCOMMAREA. */
    private static byte[] reentry() {
        return new byte[CardDemoCommarea.LENGTH];
    }

    private CicsResponse enter(String userId, String password) {
        Map<String, String> fields = new java.util.LinkedHashMap<>();
        if (userId != null) {
            fields.put("USERID", userId);
        }
        if (password != null) {
            fields.put("PASSWD", password);
        }
        return run(AidKey.ENTER, reentry(), fields);
    }

    private static CardDemoCommarea commarea(CicsResponse r) {
        return CardDemoCommarea.of(r.outcome().commarea());
    }

    private static void assertPseudoConversationContinues(CicsResponse r) {
        // MAIN-PARA lines 98-102: RETURN TRANSID(WS-TRANID) COMMAREA(CARDDEMO-COMMAREA) LENGTH(160)
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.RETURN_TRANSID);
        assertThat(r.outcome().nextTransid()).isEqualTo("CC00");
        assertThat(r.outcome().commarea()).hasSize(160);
    }

    private static void assertHeader(BmsScreen s) {
        // POPULATE-HEADER-INFO lines 181-204; COTTL01Y titles; ASSIGN APPLID/SYSID (defaults CARDDEMO / CICS)
        assertThat(s.value("TRNNAME")).isEqualTo("CC00");
        assertThat(s.value("PGMNAME")).isEqualTo("COSGN00C");
        assertThat(s.value("TITLE01")).isEqualTo("      AWS Mainframe Modernization       ");
        assertThat(s.value("TITLE02")).isEqualTo("              CardDemo                  ");
        assertThat(s.value("CURDATE")).isEqualTo("03/05/24");
        assertThat(s.value("CURTIME")).isEqualTo("14:07:09 ");
        assertThat(s.value("APPLID")).isEqualTo("CARDDEMO");
        assertThat(s.value("SYSID")).isEqualTo("CICS    ");
        assertThat(s.erase()).isTrue();
    }

    private static String errmsg(String text) {
        return PicX.of(text, 78);
    }

    // ------------------------------------------------------------------ MAIN-PARA

    @Test
    void firstEntrySendsEmptySignonScreenWithCursorOnUserId() {
        // lines 80-83: EIBCALEN = 0 -> LOW-VALUES TO COSGN0AO, -1 TO USERIDL, SEND-SIGNON-SCREEN
        CicsResponse r = run(AidKey.ENTER, null, null);
        assertThat(r.eib().eibcalen()).isZero();
        BmsScreen s = r.lastMap().screen();
        assertHeader(s);
        assertThat(s.map()).isEqualTo("COSGN0A");
        assertThat(s.mapset()).isEqualTo("COSGN00");
        assertThat(s.value("ERRMSG")).isEqualTo(PicX.spaces(78));
        assertThat(s.field("USERID").orElseThrow().sent()).isFalse();
        assertThat(s.value("USERID")).isEqualTo(PicX.spaces(8));
        assertThat(s.cursorField()).isEqualTo("USERID");
        assertThat(s.cursorRow()).isEqualTo(19);
        assertThat(s.cursorColumn()).isEqualTo(44);
        assertThat(r.terminalOutputs()).hasSize(1);
        assertPseudoConversationContinues(r);
        // CARDDEMO-COMMAREA is never initialised on this path: all LOW-VALUES
        assertThat(PicX.isLowValues(PicX.fromBytes(r.outcome().commarea()))).isTrue();
        assertThat(s.render().get(18)).contains("User ID     :");
        assertThat(s.render().get(23)).contains("ENTER=Sign-on  F3=Exit");
    }

    @Test
    void firstEntryIgnoresAidKey() {
        CicsResponse r = run(AidKey.PF3, null, null);
        assertThat(r.lastMap()).isNotNull();
        assertThat(r.lastMap().screen().value("ERRMSG")).isEqualTo(PicX.spaces(78));
        assertPseudoConversationContinues(r);
    }

    @Test
    void pf3SendsThankYouTextAndEndsTheConversation() {
        // lines 88-90 + SEND-PLAIN-TEXT 164-172: CCDA-MSG-THANK-YOU (CSMSG01Y) in 80-byte WS-MESSAGE, RETURN
        CicsResponse r = run(AidKey.PF3, reentry(), null);
        assertThat(r.lastMap()).isNull();
        assertThat(r.lastText().text()).isEqualTo(PicX.of("Thank you for using CardDemo application...", 80));
        assertThat(r.lastText().erase()).isTrue();
        assertThat(r.lastText().freekb()).isTrue();
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.RETURN);
        assertThat(r.outcome().commarea()).isNull();
    }

    @ParameterizedTest
    @EnumSource(value = AidKey.class, names = {"ENTER", "PF3"}, mode = EnumSource.Mode.EXCLUDE)
    void anyOtherKeyShowsInvalidKeyMessage(AidKey aid) {
        // lines 91-94: CCDA-MSG-INVALID-KEY; USERIDL not set so the cursor goes to the IC field (USERID)
        CicsResponse r = run(aid, reentry(), Map.of("USERID", "ADMIN001", "PASSWD", "PASSWORD"));
        BmsScreen s = r.lastMap().screen();
        assertHeader(s);
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("Invalid key pressed. Please see below..."));
        assertThat(s.cursorField()).isEqualTo("USERID");
        // no RECEIVE MAP on this path, so typed data is not echoed
        assertThat(s.field("USERID").orElseThrow().sent()).isFalse();
        assertPseudoConversationContinues(r);
        assertThat(PicX.isLowValues(PicX.fromBytes(r.outcome().commarea()))).isTrue();
    }

    // ------------------------------------------------------------------ PROCESS-ENTER-KEY

    @Test
    void blankUserIdAsksForUserId() {
        // lines 118-122; then lines 132-134 still move UPPER-CASE(USERIDI) to CDEMO-USER-ID; READ skipped (138)
        CicsResponse r = enter("        ", "PASSWORD");
        BmsScreen s = r.lastMap().screen();
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("Please enter User ID ..."));
        assertThat(s.cursorField()).isEqualTo("USERID");
        assertThat(r.eib().eibresp()).isZero();
        assertThat(r.terminalOutputs()).hasSize(1);
        assertPseudoConversationContinues(r);
        assertThat(commarea(r).userId()).isEqualTo(PicX.spaces(8));
    }

    @Test
    void noFieldsTransmittedIsMapfailAndAsksForUserId() {
        // RECEIVE MAP -> MAPFAIL (RESP 36, captured in WS-RESP-CD and never tested); USERIDI stays LOW-VALUES
        CicsResponse r = enter(null, null);
        assertThat(r.lastMap().screen().value("ERRMSG")).isEqualTo(errmsg("Please enter User ID ..."));
        assertThat(commarea(r).userId()).isEqualTo(LOW8);
    }

    @Test
    void clearedUserIdFieldAsksForUserId() {
        CicsResponse r = enter("", "PASSWORD");
        assertThat(r.lastMap().screen().value("ERRMSG")).isEqualTo(errmsg("Please enter User ID ..."));
    }

    @Test
    void blankPasswordAsksForPasswordAndEchoesUserIdAsTyped() {
        // lines 123-127: cursor on PASSWD; USERIDI (shared storage with USERIDO) is echoed un-uppercased
        CicsResponse r = enter("admin001", null);
        BmsScreen s = r.lastMap().screen();
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("Please enter Password ..."));
        assertThat(s.cursorField()).isEqualTo("PASSWD");
        assertThat(s.value("USERID")).isEqualTo("admin001");
        assertThat(commarea(r).userId()).isEqualTo("ADMIN001");
        assertPseudoConversationContinues(r);
    }

    @Test
    void spacesPasswordAsksForPassword() {
        CicsResponse r = enter("USER0001", "    ");
        assertThat(r.lastMap().screen().value("ERRMSG")).isEqualTo(errmsg("Please enter Password ..."));
    }

    // ------------------------------------------------------------------ READ-USER-SEC-FILE

    @Test
    void adminSignOnXctlsToAdminMenuWithCommarea() {
        // lines 223-234: password match, CDEMO-USRTYP-ADMIN -> XCTL COADM01C; input is upper-cased (132-136)
        CicsResponse r = enter("admin001", "password");
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.XCTL);
        assertThat(r.outcome().nextProgram()).isEqualTo("COADM01C");
        assertThat(r.terminalOutputs()).isEmpty();
        CardDemoCommarea c = commarea(r);
        assertThat(c.fromTranid()).isEqualTo("CC00");
        assertThat(c.fromProgram()).isEqualTo("COSGN00C");
        assertThat(c.userId()).isEqualTo("ADMIN001");
        assertThat(c.userType()).isEqualTo("A");
        assertThat(c.isAdmin()).isTrue();
        assertThat(c.pgmContextRaw()).isEqualTo("0");
        assertThat(c.isPgmEnter()).isTrue();
        // fields COSGN00C never sets stay as uninitialised WORKING-STORAGE (LOW-VALUES)
        assertThat(c.toTranid()).isEqualTo(PicX.lowValues(4));
        assertThat(c.toProgram()).isEqualTo(LOW8);
        assertThat(PicX.isLowValues(CardDemoCommarea.CUSTOMER_INFO.get(c))).isTrue();
        assertThat(PicX.isLowValues(CardDemoCommarea.MORE_INFO.get(c))).isTrue();
        assertThat(r.outcome().commarea()).hasSize(160);
    }

    @Test
    void regularUserSignOnXctlsToMainMenu() {
        // lines 235-239: SEC-USR-TYPE 'U' -> XCTL COMEN01C
        CicsResponse r = enter("USER0003", "PASSWORD");
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.XCTL);
        assertThat(r.outcome().nextProgram()).isEqualTo("COMEN01C");
        assertThat(commarea(r).userType()).isEqualTo("U");
        assertThat(commarea(r).userId()).isEqualTo("USER0003");
    }

    @Test
    void everySeededUserCanSignOn() {
        for (String rec : UserSecuritySeed.records()) {
            UserSecurityRecord u = UserSecurityRecord.of(rec);
            CicsResponse r = enter(u.userId().toLowerCase(), "Password");
            assertThat(r.outcome().nextProgram()).isEqualTo("A".equals(u.userType()) ? "COADM01C" : "COMEN01C");
        }
    }

    @Test
    void nonAdminUserTypeOtherThanUAlsoGoesToMainMenu() {
        // line 230: anything but 'A' takes the ELSE branch
        usrsec.write(PicX.of("ODDUSER1FIRST               LAST                SECRET  X", 80));
        CicsResponse r = enter("odduser1", "secret");
        assertThat(r.outcome().nextProgram()).isEqualTo("COMEN01C");
        assertThat(commarea(r).userType()).isEqualTo("X");
    }

    @Test
    void lowercaseStoredPasswordCanNeverMatch() {
        // entered password is upper-cased (line 135) but SEC-USR-PWD is compared as stored (line 223)
        usrsec.write(PicX.of("LOWERPW1FIRST               LAST                secret  U", 80));
        CicsResponse r = enter("LOWERPW1", "secret");
        assertThat(r.lastMap().screen().value("ERRMSG")).isEqualTo(errmsg("Wrong Password. Try again ..."));
    }

    @Test
    void inputLongerThanFieldIsTruncatedByTheMap() {
        // USERID / PASSWD are LENGTH=8 (COSGN00.bms); extra characters never reach the program
        CicsResponse r = enter("ADMIN0019", "PASSWORDXYZ");
        assertThat(r.outcome().nextProgram()).isEqualTo("COADM01C");
        assertThat(commarea(r).userId()).isEqualTo("ADMIN001");
    }

    @Test
    void wrongPasswordRedisplaysWithCursorOnPassword() {
        // lines 241-245 (note: WS-ERR-FLG is not set on this path, which has no visible effect)
        CicsResponse r = enter("admin001", "wrongpwd");
        BmsScreen s = r.lastMap().screen();
        assertHeader(s);
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("Wrong Password. Try again ..."));
        assertThat(s.cursorField()).isEqualTo("PASSWD");
        assertThat(s.value("USERID")).isEqualTo("admin001");
        assertThat(s.field("PASSWD").orElseThrow().dark()).isTrue();
        assertThat(String.join("\n", s.render())).doesNotContain("wrongpwd");
        assertThat(r.eib().eibresp()).isZero();
        assertPseudoConversationContinues(r);
        CardDemoCommarea c = commarea(r);
        assertThat(c.userId()).isEqualTo("ADMIN001");
        assertThat(c.fromProgram()).isEqualTo(LOW8);
    }

    @Test
    void partialPasswordIsWrong() {
        CicsResponse r = enter("USER0001", "PASSWOR");
        assertThat(r.lastMap().screen().value("ERRMSG")).isEqualTo(errmsg("Wrong Password. Try again ..."));
    }

    @Test
    void unknownUserIsNotFound() {
        // lines 247-251: RESP 13 (NOTFND)
        CicsResponse r = enter("nobody", "password");
        BmsScreen s = r.lastMap().screen();
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("User not found. Try again ..."));
        assertThat(s.cursorField()).isEqualTo("USERID");
        assertThat(s.value("USERID")).isEqualTo("nobody  ");
        assertPseudoConversationContinues(r);
        assertThat(commarea(r).userId()).isEqualTo("NOBODY  ");
    }

    @Test
    void keyBoundaryJustAboveLastRecordIsNotFound() {
        CicsResponse r = enter("USER0006", "PASSWORD");
        assertThat(r.lastMap().screen().value("ERRMSG")).isEqualTo(errmsg("User not found. Try again ..."));
    }

    @Test
    void fileUnavailableIsUnableToVerify() {
        // lines 252-256: any other RESP (here DISABLED = 84)
        usrsec.setState(FileState.DISABLED);
        CicsResponse r = enter("ADMIN001", "PASSWORD");
        assertThat(r.lastMap().screen().value("ERRMSG")).isEqualTo(errmsg("Unable to verify the User ..."));
        assertThat(r.lastMap().screen().cursorField()).isEqualTo("USERID");
        assertPseudoConversationContinues(r);
    }

    @Test
    void fileNotDefinedIsUnableToVerify() {
        CicsResponse r = CicsDispatcher.run(program, CicsContext.builder().transid("CC00").aid(AidKey.ENTER)
                .commarea(reentry()).terminalInput(Map.of("USERID", "ADMIN001", "PASSWD", "PASSWORD"))
                .fileControl(new FileControl()).clock(CLOCK).build());
        assertThat(r.lastMap().screen().value("ERRMSG")).isEqualTo(errmsg("Unable to verify the User ..."));
    }

    // ------------------------------------------------------------------ POPULATE-HEADER-INFO

    @Test
    void assignSysidOnlyFillsFirstFourBytesOfSysido() {
        CicsResponse r = CicsDispatcher.run(program, CicsContext.builder().transid("CC00")
                .systemInfo(new SystemInfo("APPLID01", "SYSA")).clock(CLOCK).build());
        assertThat(r.lastMap().screen().value("APPLID")).isEqualTo("APPLID01");
        assertThat(r.lastMap().screen().value("SYSID")).isEqualTo("SYSA    ");
    }

    @Test
    void yearIsTwoDigitsAndTimeDropsHundredths() {
        Clock clock = Clock.fixed(Instant.parse("1999-12-31T23:59:59.99Z"), ZoneOffset.UTC);
        CicsResponse r = CicsDispatcher.run(program, CicsContext.builder().transid("CC00").clock(clock).build());
        assertThat(r.lastMap().screen().value("CURDATE")).isEqualTo("12/31/99");
        assertThat(r.lastMap().screen().value("CURTIME")).isEqualTo("23:59:59 ");
    }

    @Test
    void programMetadata() {
        assertThat(program.programName()).isEqualTo("COSGN00C");
        assertThat(program.transactionId()).contains("CC00");
        assertThat(program.commareaLayout()).contains(CardDemoCommarea.LAYOUT);
    }
}
