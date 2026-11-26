package com.carddemo.programs.cousr02c;

import com.carddemo.programs.cousr02c.copybook.CardDemoCommarea;
import com.carddemo.programs.cousr02c.copybook.CicsResp;
import com.carddemo.programs.cousr02c.copybook.Dfhaid;
import com.carddemo.programs.cousr02c.copybook.Dfhbmsca;
import com.carddemo.programs.cousr02c.copybook.Pic;
import com.carddemo.programs.cousr02c.copybook.SecUserData;
import com.carddemo.programs.cousr02c.gateway.InMemoryUserSecurityFile;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class Cousr02cServiceTest {

    private static final Clock CLOCK = Clock.fixed(Instant.parse("2026-11-26T17:22:05.37Z"), ZoneOffset.UTC);

    private InMemoryUserSecurityFile usrsec;
    private Cousr02cService service;

    @BeforeEach
    void setUp() {
        usrsec = new InMemoryUserSecurityFile(List.of(
                SecUserData.of("USER0001", "TEST", "USERONE", "PASSWRDU", "U"),
                SecUserData.of("ADMIN001", "ADMIN", "TESTER", "PASSWRDA", "A")));
        service = new Cousr02cService(usrsec, CLOCK);
    }

    private static CardDemoCommarea reenterCommarea() {
        CardDemoCommarea c = CardDemoCommarea.blank();
        c.setCdemoFromTranid("CU00");
        c.setCdemoFromProgram("COUSR00C");
        c.setCdemoUserId("ADMIN001");
        c.setCdemoUserType("A");
        c.setPgmContext(CardDemoCommarea.PgmContext.REENTER);
        return c;
    }

    private Cousr02cResponse reenter(Dfhaid aid, MapInput input) {
        return service.execute(Cousr02cRequest.reenter(reenterCommarea(), aid, input));
    }

    private static MapInput full(String id, String fname, String lname, String pwd, String type) {
        return new MapInput(id, fname, lname, pwd, type);
    }

    private static String msg(String s) {
        return Pic.x(s, 78);
    }

    @Nested
    class MainPara {

        @Test
        void eibcalenZeroTransfersToSignon() {
            Cousr02cResponse r = service.execute(Cousr02cRequest.noCommarea());

            TaskOutcome.Xctl xctl = assertInstanceOf(TaskOutcome.Xctl.class, r.outcome());
            assertEquals("COSGN00C", xctl.program());
            assertTrue(r.screens().isEmpty());
            CardDemoCommarea c = xctl.commarea();
            assertEquals("CU02", c.getCdemoFromTranid());
            assertEquals("COUSR02C", c.getCdemoFromProgram());
            assertTrue(c.isCdemoPgmEnter());
            assertEquals(CardDemoCommarea.LENGTH, xctl.commareaBytes().length());
        }

        @Test
        void emptyCommareaStringIsEibcalenZero() {
            Cousr02cResponse r = service.execute(new Cousr02cRequest("", Dfhaid.DFHENTER, null));
            assertEquals("COSGN00C", ((TaskOutcome.Xctl) r.outcome()).program());
        }

        @Test
        void firstEntryWithoutSelectionSendsEmptyScreen() {
            CardDemoCommarea c = CardDemoCommarea.blank();
            c.setCdemoFromProgram("COADM01C");
            c.setPgmContext(CardDemoCommarea.PgmContext.ENTER);

            Cousr02cResponse r = service.execute(Cousr02cRequest.firstEntry(c));

            assertEquals(1, r.screens().size());
            ScreenOutput s = r.finalScreen();
            assertEquals(Pic.lowValues(8), s.usrid());
            assertEquals(Pic.lowValues(20), s.fname());
            assertEquals(msg(""), s.errmsg());
            assertEquals("USRIDIN", s.cursorField());
            assertEquals(Dfhbmsca.DFHDFCOL, s.errmsgColor());
            TaskOutcome.Return ret = assertInstanceOf(TaskOutcome.Return.class, r.outcome());
            assertEquals("CU02", ret.transId());
            assertTrue(ret.commarea().isCdemoPgmReenter());
            assertEquals(0, usrsec.rewriteCount());
        }

        @Test
        void headerIsPopulatedOnEverySend() {
            CardDemoCommarea c = CardDemoCommarea.blank();
            ScreenOutput s = service.execute(Cousr02cRequest.firstEntry(c)).finalScreen();

            assertEquals("CU02", s.trnname());
            assertEquals("COUSR02C", s.pgmname());
            assertEquals("      AWS Mainframe Modernization       ", s.title01());
            assertEquals("              CardDemo                  ", s.title02());
            assertEquals("11/26/26", s.curdate());
            assertEquals("17:22:05", s.curtime());
        }

        @Test
        void firstEntryWithSelectedUserFetchesIt() {
            CardDemoCommarea c = CardDemoCommarea.blank();
            c.setCdemoCu02UsrSelected("USER0001");

            Cousr02cResponse r = service.execute(Cousr02cRequest.firstEntry(c));

            // READ NORMAL sends once (names still blank), PROCESS-ENTER-KEY sends, MAIN-PARA sends again.
            assertEquals(3, r.screens().size());
            ScreenOutput fromRead = r.screens().get(0);
            assertEquals(Pic.spaces(20), fromRead.fname());
            assertEquals(msg("Press PF5 key to save your updates ..."), fromRead.errmsg());

            ScreenOutput s = r.finalScreen();
            assertEquals("USER0001", s.usrid());
            assertEquals(Pic.x("TEST", 20), s.fname());
            assertEquals(Pic.x("USERONE", 20), s.lname());
            assertEquals("PASSWRDU", s.passwd());
            assertEquals("U", s.usrtype());
            assertEquals("Press PF5 key to save your updates ...", s.message());
            assertEquals(Dfhbmsca.DFHNEUTR, s.errmsgColor());
            assertEquals("USRIDIN", s.cursorField());
            assertInstanceOf(TaskOutcome.Return.class, r.outcome());
        }

        @Test
        void firstEntryWithUnknownSelectedUser() {
            CardDemoCommarea c = CardDemoCommarea.blank();
            c.setCdemoCu02UsrSelected("NOBODY");

            Cousr02cResponse r = service.execute(Cousr02cRequest.firstEntry(c));

            assertEquals(2, r.screens().size());
            ScreenOutput s = r.finalScreen();
            assertEquals("User ID NOT found...", s.message());
            assertEquals("USRIDIN", s.cursorField());
            // PROCESS-ENTER-KEY blanks the name fields before the READ.
            assertEquals(Pic.spaces(20), s.fname());
        }

        @Test
        void selectionOfLowValuesIsIgnored() {
            CardDemoCommarea c = CardDemoCommarea.blank();
            String raw = c.toBytes();
            raw = raw.substring(0, CardDemoCommarea.LENGTH - 8) + Pic.lowValues(8);

            Cousr02cResponse r = service.execute(new Cousr02cRequest(raw, Dfhaid.DFHENTER, null));

            assertEquals(1, r.screens().size());
            assertEquals(msg(""), r.finalScreen().errmsg());
        }

        @Test
        void shortCommareaIsSpacePadded() {
            Cousr02cResponse r = service.execute(new Cousr02cRequest("CU00COUSR00C", Dfhaid.DFHENTER, null));

            assertEquals(1, r.screens().size());
            CardDemoCommarea out = r.outcome().commarea();
            assertEquals("CU00", out.getCdemoFromTranid());
            assertEquals("COUSR00C", out.getCdemoFromProgram());
            assertEquals(Pic.spaces(8), out.getCdemoCu02UsrSelected());
            assertTrue(out.isCdemoPgmReenter());
        }

        @ParameterizedTest
        @EnumSource(value = Dfhaid.class, names = {"DFHPF1", "DFHPF7", "DFHPF8", "DFHCLEAR", "DFHPA1"})
        void otherKeysShowInvalidKeyMessageAndEchoInput(Dfhaid aid) {
            Cousr02cResponse r = reenter(aid, full("USER0001", "NEW", "NAME", "SECRET", "A"));

            ScreenOutput s = r.finalScreen();
            assertEquals(msg("Invalid key pressed. Please see below..."), s.errmsg());
            assertEquals("USER0001", s.usrid());
            assertEquals(Pic.x("NEW", 20), s.fname());
            assertInstanceOf(TaskOutcome.Return.class, r.outcome());
            assertEquals(0, usrsec.rewriteCount());
        }

        @Test
        void pf12CancelsWithoutUpdating() {
            Cousr02cResponse r = reenter(Dfhaid.DFHPF12, full("USER0001", "CHANGED", "USERONE", "PASSWRDU", "U"));

            TaskOutcome.Xctl xctl = assertInstanceOf(TaskOutcome.Xctl.class, r.outcome());
            assertEquals("COADM01C", xctl.program());
            assertTrue(r.screens().isEmpty());
            assertEquals(0, usrsec.rewriteCount());
            assertEquals("COUSR02C", xctl.commarea().getCdemoFromProgram());
            assertEquals("CU02", xctl.commarea().getCdemoFromTranid());
            assertTrue(xctl.commarea().isCdemoPgmEnter());
        }

        @Test
        void pf4ClearsScreen() {
            Cousr02cResponse r = reenter(Dfhaid.DFHPF4, full("USER0001", "TEST", "USERONE", "PASSWRDU", "U"));

            ScreenOutput s = r.finalScreen();
            assertEquals(1, r.screens().size());
            assertEquals(Pic.spaces(8), s.usrid());
            assertEquals(Pic.spaces(20), s.fname());
            assertEquals(Pic.spaces(20), s.lname());
            assertEquals(Pic.spaces(8), s.passwd());
            assertEquals(" ", s.usrtype());
            assertEquals(msg(""), s.errmsg());
            assertEquals("USRIDIN", s.cursorField());
            assertInstanceOf(TaskOutcome.Return.class, r.outcome());
        }
    }

    @Nested
    class ProcessEnterKey {

        @Test
        void blankUserIdIsRejected() {
            Cousr02cResponse r = reenter(Dfhaid.DFHENTER, MapInput.userId("        "));

            assertEquals(1, r.screens().size());
            assertEquals("User ID can NOT be empty...", r.finalScreen().message());
            assertEquals("USRIDIN", r.finalScreen().cursorField());
        }

        @Test
        void untransmittedUserIdIsLowValuesAndRejected() {
            Cousr02cResponse r = reenter(Dfhaid.DFHENTER, MapInput.empty());
            assertEquals("User ID can NOT be empty...", r.finalScreen().message());
        }

        @Test
        void mixedSpacesAndLowValuesIsNotEmpty() {
            Cousr02cResponse r = reenter(Dfhaid.DFHENTER, MapInput.userId(" \u0000\u0000\u0000\u0000\u0000\u0000\u0000"));
            assertEquals("User ID NOT found...", r.finalScreen().message());
        }

        @Test
        void knownUserIsDisplayed() {
            Cousr02cResponse r = reenter(Dfhaid.DFHENTER, full("ADMIN001", "X", "Y", "Z", "U"));

            assertEquals(2, r.screens().size());
            ScreenOutput s = r.finalScreen();
            assertEquals(Pic.x("ADMIN", 20), s.fname());
            assertEquals(Pic.x("TESTER", 20), s.lname());
            assertEquals("PASSWRDA", s.passwd());
            assertEquals("A", s.usrtype());
            assertEquals("Press PF5 key to save your updates ...", s.message());
            assertEquals(Dfhbmsca.DFHNEUTR, s.errmsgColor());
            assertEquals(0, usrsec.rewriteCount());
        }

        @Test
        void unknownUserClearsNameFields() {
            Cousr02cResponse r = reenter(Dfhaid.DFHENTER, full("ZZZ", "OLD", "VALUES", "PW", "A"));

            assertEquals(1, r.screens().size());
            ScreenOutput s = r.finalScreen();
            assertEquals("User ID NOT found...", s.message());
            assertEquals(Pic.x("ZZZ", 8), s.usrid());
            assertEquals(Pic.spaces(20), s.fname());
            assertEquals(Pic.spaces(8), s.passwd());
            assertEquals("USRIDIN", s.cursorField());
            assertEquals(Dfhbmsca.DFHDFCOL, s.errmsgColor());
        }

        @Test
        void inMemoryFileRejectsInjectedNormalResponses() {
            assertThrows(IllegalArgumentException.class, () -> usrsec.injectNextRead(CicsResp.NORMAL, 0));
            assertThrows(IllegalArgumentException.class, () -> usrsec.injectNextRewrite(CicsResp.NORMAL, 0));
        }

        @Test
        void readFailureShowsLookupErrorAndDisplaysResp() {
            usrsec.injectNextRead(CicsResp.NOTOPEN, 50);

            Cousr02cResponse r = reenter(Dfhaid.DFHENTER, MapInput.userId("USER0001"));

            ScreenOutput s = r.finalScreen();
            assertEquals("Unable to lookup User...", s.message());
            // FNAMEL is set to -1 but USRIDINL (set earlier) wins: first -1 field in map order gets the cursor.
            assertEquals("USRIDIN", s.cursorField());
            assertEquals(List.of("RESP:+0000000019REAS:+0000000050"), r.displayLines());
        }
    }

    @Nested
    class UpdateUserInfo {

        @Test
        void validationOrderAndMessages() {
            assertValidation(full(" ", " ", " ", " ", " "), "User ID can NOT be empty...", "USRIDIN");
            assertValidation(full("USER0001", " ", " ", " ", " "), "First Name can NOT be empty...", "FNAME");
            assertValidation(full("USER0001", "A", null, " ", " "), "Last Name can NOT be empty...", "LNAME");
            assertValidation(full("USER0001", "A", "B", "", " "), "Password can NOT be empty...", "PASSWD");
            assertValidation(full("USER0001", "A", "B", "C", " "), "User Type can NOT be empty...", "USRTYPE");
        }

        private void assertValidation(MapInput in, String message, String cursor) {
            Cousr02cResponse r = reenter(Dfhaid.DFHPF5, in);
            assertEquals(1, r.screens().size(), message);
            assertEquals(message, r.finalScreen().message());
            assertEquals(cursor, r.finalScreen().cursorField());
            assertEquals(0, usrsec.rewriteCount());
            assertInstanceOf(TaskOutcome.Return.class, r.outcome());
        }

        @Test
        void unchangedRecordAsksToModify() {
            Cousr02cResponse r = reenter(Dfhaid.DFHPF5, full("USER0001", "TEST", "USERONE", "PASSWRDU", "U"));

            assertEquals(2, r.screens().size());
            assertEquals("Press PF5 key to save your updates ...", r.screens().get(0).message());
            ScreenOutput s = r.finalScreen();
            assertEquals("Please modify to update ...", s.message());
            assertEquals(Dfhbmsca.DFHRED, s.errmsgColor());
            assertEquals("FNAME", s.cursorField());
            assertEquals(0, usrsec.rewriteCount());
        }

        @Test
        void changedFirstNameIsRewritten() {
            usrsec.put(SecUserData.fromRecord("USER0001TEST                USERONE             PASSWRDUU"
                    + "KEEP-THIS-FILLER-BYTES!"));

            Cousr02cResponse r = reenter(Dfhaid.DFHPF5, full("USER0001", "RENAMED", "USERONE", "PASSWRDU", "U"));

            ScreenOutput s = r.finalScreen();
            assertEquals("User USER0001 has been updated ...", s.message());
            assertEquals(Dfhbmsca.DFHGREEN, s.errmsgColor());
            assertEquals(1, usrsec.rewriteCount());
            SecUserData stored = usrsec.find("USER0001").orElseThrow();
            assertEquals(Pic.x("RENAMED", 20), stored.getSecUsrFname());
            assertEquals("KEEP-THIS-FILLER-BYTES!", stored.getSecUsrFiller());
            assertInstanceOf(TaskOutcome.Return.class, r.outcome());
        }

        @Test
        void eachFieldCountsAsAModification() {
            reenter(Dfhaid.DFHPF5, full("USER0001", "TEST", "CHANGED", "PASSWRDU", "U"));
            reenter(Dfhaid.DFHPF5, full("USER0001", "TEST", "CHANGED", "NEWPASS", "U"));
            reenter(Dfhaid.DFHPF5, full("USER0001", "TEST", "CHANGED", "NEWPASS", "A"));

            assertEquals(3, usrsec.rewriteCount());
            SecUserData stored = usrsec.find("USER0001").orElseThrow();
            assertEquals(Pic.x("CHANGED", 20), stored.getSecUsrLname());
            assertEquals(Pic.x("NEWPASS", 8), stored.getSecUsrPwd());
            assertEquals("A", stored.getSecUsrType());
        }

        @Test
        void inputIsTruncatedToFieldSize() {
            reenter(Dfhaid.DFHPF5, full("USER0001", "ABCDEFGHIJKLMNOPQRSTUVWXYZ", "USERONE", "PASSWORD123", "UX"));

            SecUserData stored = usrsec.find("USER0001").orElseThrow();
            assertEquals("ABCDEFGHIJKLMNOPQRST", stored.getSecUsrFname());
            assertEquals("PASSWORD", stored.getSecUsrPwd());
            assertEquals("U", stored.getSecUsrType());
        }

        @Test
        void unknownUserFallsThroughToFailedRewrite() {
            Cousr02cResponse r = reenter(Dfhaid.DFHPF5, full("NOBODY", "A", "B", "C", "U"));

            // COBOL does not stop after READ NOTFND: the comparison sees differences and issues a REWRITE with no
            // prior READ UPDATE, which CICS rejects with INVREQ.
            assertEquals(2, r.screens().size());
            assertEquals("User ID NOT found...", r.screens().get(0).message());
            ScreenOutput s = r.finalScreen();
            assertEquals("Unable to Update User...", s.message());
            assertEquals("USRIDIN", s.cursorField());
            assertEquals(List.of("RESP:+0000000016REAS:+0000000030"), r.displayLines());
            assertTrue(usrsec.find("NOBODY").isEmpty());
        }

        @Test
        void rewriteNotFound() {
            usrsec.injectNextRewrite(CicsResp.NOTFND, 80);

            Cousr02cResponse r = reenter(Dfhaid.DFHPF5, full("USER0001", "NEW", "USERONE", "PASSWRDU", "U"));

            assertEquals("User ID NOT found...", r.finalScreen().message());
            assertEquals("USRIDIN", r.finalScreen().cursorField());
            assertTrue(r.displayLines().isEmpty());
        }

        @Test
        void rewriteFailure() {
            usrsec.injectNextRewrite(CicsResp.IOERR, 0);

            Cousr02cResponse r = reenter(Dfhaid.DFHPF5, full("USER0001", "NEW", "USERONE", "PASSWRDU", "U"));

            assertEquals("Unable to Update User...", r.finalScreen().message());
            assertEquals("FNAME", r.finalScreen().cursorField());
            assertEquals(Dfhbmsca.DFHNEUTR, r.finalScreen().errmsgColor());
            assertEquals(List.of("RESP:+0000000017REAS:+0000000000"), r.displayLines());
        }

        @Test
        void successMessageUsesUserIdUpToFirstSpace() {
            usrsec.put(SecUserData.of("AB CD", "FIRST", "LAST", "PW", "U"));

            Cousr02cResponse r = reenter(Dfhaid.DFHPF5, full("AB CD", "FIRST", "LAST", "PW2", "U"));

            assertEquals("User AB has been updated ...", r.finalScreen().message());
        }

        @Test
        void recordLockIsReleasedAtTaskEnd() {
            reenter(Dfhaid.DFHENTER, MapInput.userId("USER0001"));
            // A REWRITE in a later task without its own READ UPDATE must fail.
            assertEquals(CicsResp.INVREQ,
                    usrsec.rewrite("USRSEC  ", SecUserData.of("USER0001", "X", "Y", "Z", "U")).resp());
        }
    }

    @Nested
    class Pf3SaveAndExit {

        @Test
        void savesAndReturnsToCallingProgram() {
            Cousr02cResponse r = reenter(Dfhaid.DFHPF3, full("USER0001", "NEW", "USERONE", "PASSWRDU", "U"));

            TaskOutcome.Xctl xctl = assertInstanceOf(TaskOutcome.Xctl.class, r.outcome());
            assertEquals("COUSR00C", xctl.program());
            assertEquals("User USER0001 has been updated ...", r.finalScreen().message());
            assertEquals(1, usrsec.rewriteCount());
            assertEquals("COUSR02C", xctl.commarea().getCdemoFromProgram());
            assertEquals("COUSR00C", xctl.commarea().getCdemoToProgram());
        }

        @Test
        void blankFromProgramReturnsToAdminMenu() {
            CardDemoCommarea c = reenterCommarea();
            c.setCdemoFromProgram("");

            Cousr02cResponse r = service.execute(Cousr02cRequest.reenter(c, Dfhaid.DFHPF3,
                    full("USER0001", "TEST", "USERONE", "PASSWRDU", "U")));

            assertEquals("COADM01C", ((TaskOutcome.Xctl) r.outcome()).program());
            assertEquals("Please modify to update ...", r.finalScreen().message());
        }

        @Test
        void validationErrorStillTransfersControl() {
            Cousr02cResponse r = reenter(Dfhaid.DFHPF3, full("USER0001", "", "", "", ""));

            assertEquals("First Name can NOT be empty...", r.finalScreen().message());
            assertEquals("COUSR00C", ((TaskOutcome.Xctl) r.outcome()).program());
            assertEquals(0, usrsec.rewriteCount());
        }
    }

    @Test
    void displaySignedMatchesGnuCobol() {
        assertEquals("+0000000013", Cousr02cService.displaySigned(13));
        assertEquals("-0000000001", Cousr02cService.displaySigned(-1));
        assertEquals("+0000000000", Cousr02cService.displaySigned(0));
    }

    @Test
    void noScreenAfterEibcalenZero() {
        assertNull(service.execute(Cousr02cRequest.noCommarea()).finalScreen());
    }
}
