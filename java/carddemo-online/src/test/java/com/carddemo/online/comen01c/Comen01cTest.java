package com.carddemo.online.comen01c;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.catchThrowableOfType;

import com.carddemo.common.copybook.CardDemoCommarea;
import com.carddemo.common.copybook.CommonMessages;
import com.carddemo.runtime.bms.BmsScreen;
import com.carddemo.runtime.cics.AidKey;
import com.carddemo.runtime.cics.CicsContext;
import com.carddemo.runtime.cics.CicsControlTransfer;
import com.carddemo.runtime.cics.CicsDispatcher;
import com.carddemo.runtime.cics.CicsOutcome;
import com.carddemo.runtime.cics.CicsResponse;
import com.carddemo.runtime.cics.InstalledPrograms;
import com.carddemo.runtime.cics.TerminalOutput;
import com.carddemo.runtime.cobol.PicX;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.List;
import java.util.Map;
import java.util.Set;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.EnumSource;

/**
 * Behavioural parity tests for COMEN01C. Line numbers refer to app/cbl/COMEN01C.cbl; menu data is COMEN02Y
 * (app/cpy/COMEN02Y.cpy lines 21-98). Expected values are hand-traced (CICS programs cannot run under
 * GnuCOBOL); the pure-COBOL option parsing and STRING statements are cross-checked against GnuCOBOL in
 * {@link Comen01cGnuCobolGoldenTest}.
 */
class Comen01cTest {

    /** 2024-03-05 14:07:09.25 UTC: CURDATE 03/05/24, CURTIME 14:07:09 (POPULATE-HEADER-INFO lines 238-257). */
    private static final Clock CLOCK = Clock.fixed(Instant.parse("2024-03-05T14:07:09.25Z"), ZoneOffset.UTC);

    private static final List<String> OPTION_LINES = List.of(
            "01. Account View                           ",
            "02. Account Update                         ",
            "03. Credit Card List                       ",
            "04. Credit Card View                       ",
            "05. Credit Card Update                     ",
            "06. Transaction List                       ",
            "07. Transaction View                       ",
            "08. Transaction Add                        ",
            "09. Transaction Reports                    ",
            "10. Bill Payment                           ",
            "11. Pending Authorization View             ");

    private CicsResponse run(Comen01c program, AidKey aid, byte[] commarea, Map<String, String> fields,
                             InstalledPrograms installed) {
        return CicsDispatcher.run(program, CicsContext.builder().transid("CM00").aid(aid).commarea(commarea)
                .terminalInput(fields).clock(CLOCK).installedPrograms(installed).build());
    }

    private CicsResponse run(AidKey aid, byte[] commarea, Map<String, String> fields) {
        return run(new Comen01c(), aid, commarea, fields, InstalledPrograms.carddemoCsd());
    }

    /** Commarea as COSGN00C leaves it for a regular user (COSGN00C READ-USER-SEC-FILE lines 230-240). */
    private static CardDemoCommarea signedOn(String userType, int pgmContext) {
        CardDemoCommarea c = new CardDemoCommarea();
        c.setFromTranid("CC00");
        c.setFromProgram("COSGN00C");
        c.setUserId("USER0001");
        c.setUserType(userType);
        c.setPgmContext(pgmContext);
        return c;
    }

    private static byte[] reenter(String userType) {
        return signedOn(userType, 1).toBytes();
    }

    private CicsResponse enter(String option) {
        return run(AidKey.ENTER, reenter("U"), option == null ? Map.of() : Map.of("OPTION", option));
    }

    private static CardDemoCommarea commarea(CicsResponse r) {
        return CardDemoCommarea.of(r.outcome().commarea());
    }

    private static String errmsg(String text) {
        return PicX.of(text, 78);
    }

    private static void assertMenuScreen(BmsScreen s) {
        // SEND-MENU-SCREEN lines 210-218 (ERASE); POPULATE-HEADER-INFO lines 240-255 with COTTL01Y titles
        assertThat(s.mapset()).isEqualTo("COMEN01");
        assertThat(s.map()).isEqualTo("COMEN1A");
        assertThat(s.erase()).isTrue();
        assertThat(s.value("TRNNAME")).isEqualTo("CM00");
        assertThat(s.value("PGMNAME")).isEqualTo("COMEN01C");
        assertThat(s.value("TITLE01")).isEqualTo("      AWS Mainframe Modernization       ");
        assertThat(s.value("TITLE02")).isEqualTo("              CardDemo                  ");
        assertThat(s.value("CURDATE")).isEqualTo("03/05/24");
        assertThat(s.value("CURTIME")).isEqualTo("14:07:09");
        // BUILD-MENU-OPTIONS lines 264-301: STRING num '. ' name INTO WS-MENU-OPT-TXT X(40) -> OPTNnnnO
        for (int i = 1; i <= 11; i++) {
            String name = String.format("OPTN%03d", i);
            assertThat(s.value(name)).as(name).isEqualTo(OPTION_LINES.get(i - 1).substring(0, 40));
            assertThat(s.field(name).orElseThrow().sent()).isTrue();
        }
        // only 11 options (COMEN02Y line 22): OPTN012 keeps LOW-VALUES and shows the map's empty initial value
        assertThat(s.field("OPTN012").orElseThrow().sent()).isFalse();
        assertThat(s.value("OPTN012")).isEqualTo(PicX.spaces(40));
        // ATTRB=(FSET,IC,...) on OPTION (COMEN01.bms line 145); SEND MAP has no CURSOR option
        assertThat(s.cursorField()).isEqualTo("OPTION");
        assertThat(s.cursorRow()).isEqualTo(20);
        assertThat(s.cursorColumn()).isEqualTo(42);
    }

    private static void assertPseudoConversationContinues(CicsResponse r) {
        // MAIN-PARA lines 107-110: RETURN TRANSID(WS-TRANID) COMMAREA(CARDDEMO-COMMAREA)
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.RETURN_TRANSID);
        assertThat(r.outcome().nextTransid()).isEqualTo("CM00");
        assertThat(r.outcome().commarea()).hasSize(CardDemoCommarea.LENGTH);
    }

    private static int mapsSent(CicsResponse r) {
        return (int) r.terminalOutputs().stream().filter(o -> o instanceof TerminalOutput.MapOutput).count();
    }

    // ------------------------------------------------------------------ MAIN-PARA

    @ParameterizedTest
    @EnumSource(value = AidKey.class, names = {"ENTER", "PF3", "CLEAR"})
    void noCommareaReturnsToSignon(AidKey aid) {
        // lines 82-84: EIBCALEN = 0 -> 'COSGN00C' TO CDEMO-FROM-PROGRAM, RETURN-TO-SIGNON-SCREEN;
        // lines 198-202: CDEMO-TO-PROGRAM is LOW-VALUES -> 'COSGN00C', XCTL PROGRAM(...) with no COMMAREA
        CicsResponse r = run(aid, null, null);
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.XCTL);
        assertThat(r.outcome().nextProgram()).isEqualTo("COSGN00C");
        assertThat(r.outcome().commarea()).isNull();
        assertThat(r.terminalOutputs()).isEmpty();
    }

    @ParameterizedTest
    @EnumSource(value = AidKey.class, names = {"ENTER", "PF3", "PF5", "CLEAR"})
    void firstEntryFromSignonSendsMenuWhateverTheKey(AidKey aid) {
        // lines 86-90: commarea present, NOT CDEMO-PGM-REENTER -> SET REENTER, LOW-VALUES TO COMEN1AO,
        // SEND-MENU-SCREEN; EIBAID is not examined on this path
        CicsResponse r = run(aid, signedOn("U", 0).toBytes(), Map.of("OPTION", "99"));
        BmsScreen s = r.lastMap().screen();
        assertMenuScreen(s);
        assertThat(s.value("ERRMSG")).isEqualTo(PicX.spaces(78));
        assertThat(s.field("OPTION").orElseThrow().sent()).isFalse();
        assertPseudoConversationContinues(r);
        CardDemoCommarea c = commarea(r);
        assertThat(c.isPgmReenter()).isTrue();
        // the rest of the commarea is returned unchanged (still CC00 / COSGN00C as the sign-on left it)
        assertThat(c.fromTranid()).isEqualTo("CC00");
        assertThat(c.fromProgram()).isEqualTo("COSGN00C");
        assertThat(c.userId()).isEqualTo("USER0001");
        assertThat(c.userType()).isEqualTo("U");
    }

    @Test
    void shortCommareaIsSpacePaddedSoPgmContextIsNotReenter() {
        // line 86: MOVE DFHCOMMAREA(1:EIBCALEN) TO CARDDEMO-COMMAREA pads with spaces; CDEMO-PGM-CONTEXT (offset
        // 50) becomes ' ', which is neither 0 nor 1 -> treated as first entry, then SET REENTER moves 1
        CicsResponse r = run(AidKey.ENTER, PicX.toBytes("CC00COSGN00C"), Map.of("OPTION", "1"));
        assertMenuScreen(r.lastMap().screen());
        CardDemoCommarea c = commarea(r);
        assertThat(c.isPgmReenter()).isTrue();
        assertThat(c.fromTranid()).isEqualTo("CC00");
        assertThat(c.fromProgram()).isEqualTo("COSGN00C");
        assertThat(c.userId()).isEqualTo(PicX.spaces(8));
    }

    @Test
    void pf3ReturnsToSignonWithoutCommarea() {
        // lines 95-97: WHEN DFHPF3 -> 'COSGN00C' TO CDEMO-TO-PROGRAM, RETURN-TO-SIGNON-SCREEN (XCTL, no COMMAREA)
        CardDemoCommarea c = signedOn("U", 1);
        c.setToProgram("COADM01C");
        CicsResponse r = run(AidKey.PF3, c.toBytes(), Map.of("OPTION", "1"));
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.XCTL);
        assertThat(r.outcome().nextProgram()).isEqualTo("COSGN00C");
        assertThat(r.outcome().commarea()).isNull();
        assertThat(r.terminalOutputs()).isEmpty();
    }

    @ParameterizedTest
    @EnumSource(value = AidKey.class, names = {"PF1", "PF2", "PF4", "PF12", "PF24", "CLEAR", "PA1", "PA2"})
    void otherKeysShowInvalidKeyMessage(AidKey aid) {
        // lines 98-101: WHEN OTHER -> 'Y' TO WS-ERR-FLG, CCDA-MSG-INVALID-KEY (CSMSG01Y) TO WS-MESSAGE, SEND
        CicsResponse r = run(aid, reenter("U"), Map.of("OPTION", "1"));
        BmsScreen s = r.lastMap().screen();
        assertMenuScreen(s);
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("Invalid key pressed. Please see below..."));
        assertThat(s.value("ERRMSG")).startsWith(CommonMessages.CCDA_MSG_INVALID_KEY);
        assertThat(s.field("ERRMSG").orElseThrow().color()).isEqualTo("RED");
        // the received option is echoed back unchanged (RECEIVE INTO COMEN1AI shares storage with COMEN1AO)
        assertThat(s.value("OPTION")).isEqualTo(" 1");
        assertPseudoConversationContinues(r);
        assertThat(commarea(r).isPgmReenter()).isTrue();
    }

    @Test
    void enterWithoutAnyFieldsIsMapfailAndInvalidOption() {
        // RECEIVE-MENU-SCREEN lines 227-233 RESP(WS-RESP-CD) absorbs MAPFAIL; OPTIONI stays LOW-VALUES, the
        // right-to-left scan (lines 117-120) stops at once (LOW-VALUE is not SPACE), INSPECT finds no spaces,
        // WS-OPTION = X'0000' is NOT NUMERIC -> invalid option (lines 127-134)
        CicsResponse r = run(AidKey.ENTER, reenter("U"), null);
        BmsScreen s = r.lastMap().screen();
        assertMenuScreen(s);
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("Please enter a valid option number..."));
        assertPseudoConversationContinues(r);
    }

    // ------------------------------------------------------------------ PROCESS-ENTER-KEY: parsing / validation

    @ParameterizedTest(name = "OPTION ''{0}'' -> invalid, echoed as ''{1}''")
    @CsvSource(delimiter = '|', value = {
            "00|00",     // zero (line 129)
            "0|00",      // ' 0' -> '00'
            "' '|00",    // all spaces -> INSPECT REPLACING ALL ' ' BY '0' -> zero
            "12|12",     // > CDEMO-MENU-OPT-COUNT (11)
            "99|99",
            "123|12",    // RECEIVE truncates to LENGTH=2
            "A1|00",     // MOVE X(2) TO 9(2) stops at the letter: '00' (see moveToUnsignedDisplay)
            "'.1'|00"})  // '.' ends the integer part
    void invalidOptions(String option, String echoed) {
        // lines 127-134: WS-OPTION IS NOT NUMERIC OR > CDEMO-MENU-OPT-COUNT OR = ZEROS ->
        // 'Please enter a valid option number...', SEND-MENU-SCREEN; ERR-FLG-ON skips lines 145-187
        CicsResponse r = enter(option);
        assertThat(mapsSent(r)).isEqualTo(1);
        BmsScreen s = r.lastMap().screen();
        assertMenuScreen(s);
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("Please enter a valid option number..."));
        // line 125: MOVE WS-OPTION TO OPTIONO
        assertThat(s.value("OPTION")).isEqualTo(echoed);
        assertPseudoConversationContinues(r);
        CardDemoCommarea c = commarea(r);
        assertThat(c.fromProgram()).isEqualTo("COSGN00C");
        assertThat(c.isPgmReenter()).isTrue();
    }

    @ParameterizedTest(name = "OPTION ''{0}'' -> option {1}")
    @CsvSource(delimiter = '|', value = {
            "1|01",
            "01|01",
            "' 1'|01",
            "'1 '|01",  // trailing space trimmed by the right-to-left scan (lines 117-120), then right-justified
            "10|10",
            "1A|01",    // bug-for-bug (GnuCOBOL MOVE X TO 9): the scan stops at 'A' -> 01, a valid option
            "-1|01",    // '-' is a sign and skipped; NUM fields on a 3270 accept '-' and '.'
            "1.|01"})
    void optionParsing(String option, String expected) {
        // lines 117-125: scan OPTIONI from the right past trailing spaces, MOVE OPTIONI(1:WS-IDX) TO WS-OPTION-X
        // (JUSTIFIED RIGHT), spaces -> '0', MOVE WS-OPTION-X TO WS-OPTION, MOVE WS-OPTION TO OPTIONO
        CicsResponse r = enter(option);
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.XCTL);
        assertThat(r.outcome().nextProgram())
                .isEqualTo(MenuOptions.comen02y().pgmName(Integer.parseInt(expected)));
    }

    // ------------------------------------------------------------------ PROCESS-ENTER-KEY: navigation

    @ParameterizedTest(name = "option {0} -> XCTL {1}")
    @CsvSource({
            "1,COACTVWC", "2,COACTUPC", "3,COCRDLIC", "4,COCRDSLC", "5,COCRDUPC", "6,COTRN00C",
            "7,COTRN01C", "8,COTRN02C", "9,CORPT00C", "10,COBIL00C"})
    void validOptionTransfersToProgram(String option, String program) {
        // lines 175-183: WHEN OTHER -> WS-TRANID / WS-PGMNAME TO CDEMO-FROM-TRANID / -PROGRAM, ZEROS TO
        // CDEMO-PGM-CONTEXT, XCTL PROGRAM(CDEMO-MENU-OPT-PGMNAME(WS-OPTION)) COMMAREA(CARDDEMO-COMMAREA)
        CicsResponse r = enter(option);
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.XCTL);
        assertThat(r.outcome().nextProgram()).isEqualTo(program);
        assertThat(r.terminalOutputs()).isEmpty();
        CardDemoCommarea c = commarea(r);
        assertThat(c.fromTranid()).isEqualTo("CM00");
        assertThat(c.fromProgram()).isEqualTo("COMEN01C");
        assertThat(c.isPgmEnter()).isTrue();
        // untouched fields carry through
        assertThat(c.userId()).isEqualTo("USER0001");
        assertThat(c.userType()).isEqualTo("U");
        assertThat(c.toProgram()).isEqualTo(PicX.lowValues(8));
    }

    @Test
    void pendingAuthorizationNotInstalled() {
        // lines 146-166: COPAUS0C -> INQUIRE PROGRAM NOHANDLE; base CSD (app/csd/CARDDEMO.CSD) has no COPAUS0C
        // (it is in app-authorization-ims-db2-mq/csd/CRDDEMO2.csd) -> PGMIDERR -> DFHRED TO ERRMSGC,
        // STRING 'This option ' name DELIMITED BY '  ' ' is not installed...' INTO WS-MESSAGE, SEND (line 187)
        CicsResponse r = enter("11");
        BmsScreen s = r.lastMap().screen();
        assertMenuScreen(s);
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("This option Pending Authorization View is not installed..."));
        assertThat(s.field("ERRMSG").orElseThrow().color()).isEqualTo("RED");
        assertThat(s.value("OPTION")).isEqualTo("11");
        assertPseudoConversationContinues(r);
        assertThat(commarea(r).fromProgram()).isEqualTo("COSGN00C");
    }

    @Test
    void pendingAuthorizationInstalled() {
        // lines 151-159: INQUIRE PROGRAM EIBRESP = DFHRESP(NORMAL) -> from-tranid/-program, ZEROS context, XCTL
        CicsResponse r = run(new Comen01c(), AidKey.ENTER, reenter("U"), Map.of("OPTION", "11"),
                InstalledPrograms.carddemoCsd().with(Set.of("COPAUS0C")));
        assertThat(r.outcome().type()).isEqualTo(CicsOutcome.Type.XCTL);
        assertThat(r.outcome().nextProgram()).isEqualTo("COPAUS0C");
        assertThat(r.terminalOutputs()).isEmpty();
        CardDemoCommarea c = commarea(r);
        assertThat(c.fromTranid()).isEqualTo("CM00");
        assertThat(c.fromProgram()).isEqualTo("COMEN01C");
        assertThat(c.isPgmEnter()).isTrue();
    }

    @Test
    void adminUserIsNotRestrictedButShippedTableHasNoAdminOption() {
        // line 136: the admin check only applies to CDEMO-USRTYP-USER; every COMEN02Y entry is 'U' anyway
        CicsResponse r = run(AidKey.ENTER, reenter("A"), Map.of("OPTION", "1"));
        assertThat(r.outcome().nextProgram()).isEqualTo("COACTVWC");
        assertThat(MenuOptions.comen02y().count()).isEqualTo(11);
        for (int i = 1; i <= 11; i++) {
            assertThat(MenuOptions.comen02y().usrType(i)).isEqualTo("U");
        }
    }

    // ---------------------------------------------- table-driven paths the shipped COMEN02Y never takes

    private static MenuOptions table(int count, MenuOptions.Entry... entries) {
        return MenuOptions.of(count, List.of(entries));
    }

    @Test
    void adminOnlyOptionDeniedToRegularUser() {
        // lines 136-143: CDEMO-USRTYP-USER AND CDEMO-MENU-OPT-USRTYPE(WS-OPTION) = 'A' -> 'Y' TO WS-ERR-FLG,
        // 'No access - Admin Only option... ' TO WS-MESSAGE, SEND-MENU-SCREEN
        Comen01c program = new Comen01c(table(2, new MenuOptions.Entry(1, "Account View", "COACTVWC", "U"),
                new MenuOptions.Entry(2, "User List", "COUSR00C", "A")));
        CicsResponse r = run(program, AidKey.ENTER, reenter("U"), Map.of("OPTION", "2"), InstalledPrograms.none());
        assertThat(mapsSent(r)).isEqualTo(1);
        BmsScreen s = r.lastMap().screen();
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("No access - Admin Only option... "));
        assertThat(s.value("OPTN002")).isEqualTo(PicX.of("02. User List", 40));
        assertThat(s.field("OPTN003").orElseThrow().sent()).isFalse();
        assertPseudoConversationContinues(r);

        // an admin user (CDEMO-USRTYP-ADMIN) is let through to the admin option
        CicsResponse admin = run(program, AidKey.ENTER, reenter("A"), Map.of("OPTION", "2"),
                InstalledPrograms.none());
        assertThat(admin.outcome().nextProgram()).isEqualTo("COUSR00C");
    }

    @Test
    void invalidAndAdminOnlyBothSendTheScreen() {
        // bug-for-bug: the invalid-option IF (lines 127-134) and the admin IF (lines 136-143) are independent,
        // so option 3 > count 2 whose (unused) table entry 3 is 'A' sends the menu twice, ending with the
        // admin message
        Comen01c program = new Comen01c(table(2, new MenuOptions.Entry(1, "Account View", "COACTVWC", "U"),
                new MenuOptions.Entry(2, "Account Update", "COACTUPC", "U"),
                new MenuOptions.Entry(3, "User List", "COUSR00C", "A")));
        CicsResponse r = run(program, AidKey.ENTER, reenter("U"), Map.of("OPTION", "3"), InstalledPrograms.none());
        assertThat(mapsSent(r)).isEqualTo(2);
        BmsScreen first = ((TerminalOutput.MapOutput) r.terminalOutputs().get(0)).screen();
        assertThat(first.value("ERRMSG")).isEqualTo(errmsg("Please enter a valid option number..."));
        assertThat(r.lastMap().screen().value("ERRMSG")).isEqualTo(errmsg("No access - Admin Only option... "));
        assertPseudoConversationContinues(r);
    }

    @Test
    void dummyProgramIsComingSoon() {
        // lines 167-174: CDEMO-MENU-OPT-PGMNAME(WS-OPTION)(1:5) = 'DUMMY' -> DFHGREEN TO ERRMSGC,
        // STRING 'This option ' name DELIMITED BY SPACE 'is coming soon ...' (no space before 'is': only the
        // first word of the name is kept and runs into the literal)
        Comen01c program = new Comen01c(table(1, new MenuOptions.Entry(1, "Card Rewards", "DUMMY01C", "U")));
        CicsResponse r = run(program, AidKey.ENTER, reenter("U"), Map.of("OPTION", "1"), InstalledPrograms.none());
        BmsScreen s = r.lastMap().screen();
        assertThat(s.value("ERRMSG")).isEqualTo(errmsg("This option Cardis coming soon ..."));
        assertThat(s.field("ERRMSG").orElseThrow().color()).isEqualTo("GREEN");
        assertPseudoConversationContinues(r);
        assertThat(commarea(r).fromProgram()).isEqualTo("COSGN00C");
    }

    @Test
    void notInstalledMessageKeepsNameUpToDoubleSpace() {
        // line 160-163: DELIMITED BY '  ' keeps single spaces inside the name; a full 35-character name has
        // no double space and is kept whole: 12 + 35 + 20 = 67 characters, fits WS-MESSAGE / ERRMSG
        String longName = "Pending Authorization View Extended";
        Comen01c program = new Comen01c(table(1, new MenuOptions.Entry(1, longName, "COPAUS0C", "U")));
        CicsResponse r = run(program, AidKey.ENTER, reenter("U"), Map.of("OPTION", "1"), InstalledPrograms.none());
        assertThat(r.lastMap().screen().value("ERRMSG"))
                .isEqualTo(errmsg("This option " + longName + " is not installed..."));
        assertThat(r.lastMap().screen().value("OPTN001")).isEqualTo("01. " + longName + " ");
    }

    @Test
    void outOfRangeSubscriptsInAdminCheck() {
        // bug-for-bug: line 137 subscripts with WS-OPTION even when it failed validation. Option 00 addresses
        // the byte before entry 1 = second digit of CDEMO-MENU-OPT-COUNT ('1'); 13-99 address storage past
        // the 01 item (LOW-VALUES here). Neither is 'A', so such options show the invalid message only once.
        assertThat(MenuOptions.comen02y().usrType(0)).isEqualTo("1");
        assertThat(MenuOptions.comen02y().usrType(99)).isEqualTo(PicX.lowValues(1));
        assertThat(mapsSent(enter("00"))).isEqualTo(1);
        assertThat(mapsSent(enter("99"))).isEqualTo(1);
    }

    // ------------------------------------------------------------------ RETURN-TO-SIGNON-SCREEN

    @Test
    void returnToSignonDefaultsTargetOnlyWhenEmpty() {
        // lines 198-202: CDEMO-TO-PROGRAM = LOW-VALUES OR SPACES -> 'COSGN00C'; otherwise XCTL to it as is.
        // Through MAIN-PARA the target is always COSGN00C (lines 83/96), so this is a paragraph-level test.
        for (String to : List.of(PicX.lowValues(8), PicX.spaces(8), "COADM01C")) {
            Comen01c.Task task = new Comen01c.Task(CicsContext.builder().clock(CLOCK).build(),
                    MenuOptions.comen02y());
            task.commarea.setToProgram(to);
            CicsControlTransfer xctl = catchThrowableOfType(CicsControlTransfer.class, task::returnToSignonScreen);
            assertThat(xctl.outcome().type()).isEqualTo(CicsOutcome.Type.XCTL);
            assertThat(xctl.outcome().nextProgram()).isEqualTo("COADM01C".equals(to) ? "COADM01C" : "COSGN00C");
            assertThat(xctl.outcome().commarea()).isNull();
        }
    }

    // ------------------------------------------------------------------ BUILD-MENU-OPTIONS

    @Test
    void buildMenuOptionsUsesFixedWidthNames() {
        // lines 264-301: STRING CDEMO-MENU-OPT-NUM(i) '. ' CDEMO-MENU-OPT-NAME(i) DELIMITED BY SIZE INTO a
        // 40-byte field: 2 + 2 + 35 = 39 characters, the 40th keeps the space from MOVE SPACES (line 266)
        Comen01c.Task task = new Comen01c.Task(CicsContext.builder().clock(CLOCK).build(), MenuOptions.comen02y());
        task.buildMenuOptions();
        assertThat(task.wsIdx).isEqualTo(12);
        assertThat(task.wsMenuOptTxt).isEqualTo("11. Pending Authorization View          ");
        assertThat(task.wsMenuOptTxt).hasSize(40);
    }

    @Test
    void stringIntoOverlaysAndTruncates() {
        assertThat(Comen01c.Task.stringInto("XXXXXX", "ab", "c")).isEqualTo("abcXXX");
        assertThat(Comen01c.Task.stringInto("XXX", "abcd")).isEqualTo("abc");
        assertThat(Comen01c.Task.delimitedBy("Bill Payment   ", "  ")).isEqualTo("Bill Payment");
        assertThat(Comen01c.Task.delimitedBy("Bill Payment   ", " ")).isEqualTo("Bill");
        assertThat(Comen01c.Task.delimitedBy("NoDelimiter", " ")).isEqualTo("NoDelimiter");
    }
}
