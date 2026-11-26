package com.carddemo.programs.cousr01c;

import com.carddemo.programs.cousr01c.copybook.CardDemoCommarea;
import com.carddemo.programs.cousr01c.copybook.Cousr1aMap.Field;
import com.carddemo.programs.cousr01c.copybook.Dfhaid;
import com.carddemo.programs.cousr01c.copybook.Dfhbmsca;
import com.carddemo.programs.cousr01c.copybook.Pic;
import com.carddemo.programs.cousr01c.copybook.SecUserData;
import com.carddemo.programs.cousr01c.dto.CicsRequest;
import com.carddemo.programs.cousr01c.dto.CicsResponse;
import com.carddemo.programs.cousr01c.dto.UserAddScreenInput;
import com.carddemo.programs.cousr01c.dto.UserAddScreenOutput;
import com.carddemo.programs.cousr01c.gateway.CicsResp;
import com.carddemo.programs.cousr01c.gateway.InMemoryUserSecRepository;
import com.carddemo.programs.cousr01c.gateway.WriteResult;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;

import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class Cousr01cServiceTest {

    private static final Clock CLOCK = Clock.fixed(Instant.parse("2026-03-07T09:05:04.56Z"), ZoneOffset.UTC);
    private static final UserAddScreenInput VALID =
            new UserAddScreenInput("JOHN", "SMITH", "JSMITH01", "SECRET1", "U");

    private InMemoryUserSecRepository repo;
    private Cousr01cService service;

    @BeforeEach
    void setUp() {
        repo = new InMemoryUserSecRepository();
        service = new Cousr01cService(repo, CLOCK);
    }

    private static CardDemoCommarea reenterCommarea() {
        CardDemoCommarea c = new CardDemoCommarea();
        c.fromTranid("CA00");
        c.fromProgram("COADM01C");
        c.userId("ADMIN001");
        c.setUserType(CardDemoCommarea.UserType.ADMIN);
        c.setPgmContext(CardDemoCommarea.PgmContext.REENTER);
        return c;
    }

    private CicsResponse.SendAndReturn send(Dfhaid aid, UserAddScreenInput input) {
        CicsResponse r = service.handle(new CicsRequest(reenterCommarea(), aid, input));
        return assertInstanceOf(CicsResponse.SendAndReturn.class, r);
    }

    @Nested
    class MainPara {

        @Test
        void noCommareaTransfersToSignon() {
            CicsResponse r = service.handle(CicsRequest.noCommarea());
            CicsResponse.Xctl x = assertInstanceOf(CicsResponse.Xctl.class, r);
            assertEquals("COSGN00C", x.program());
            assertEquals("COSGN00C", x.commarea().toProgram());
            assertEquals("CU01", x.commarea().fromTranid());
            assertEquals("COUSR01C", x.commarea().fromProgram());
            assertEquals("0", x.commarea().pgmContext());
            assertEquals(0, repo.size());
        }

        @Test
        void firstEntrySendsEmptyScreenAndSetsReenter() {
            CardDemoCommarea c = reenterCommarea();
            c.setPgmContext(CardDemoCommarea.PgmContext.ENTER);
            CicsResponse r = service.handle(new CicsRequest(c, Dfhaid.DFHENTER, VALID));
            CicsResponse.SendAndReturn s = assertInstanceOf(CicsResponse.SendAndReturn.class, r);

            assertEquals("CU01", s.transId());
            assertTrue(s.commarea().isPgmReenter());
            assertEquals("ADMIN001", s.commarea().userId());
            UserAddScreenOutput o = s.screen();
            assertEquals("CU01", o.trnname());
            assertEquals("COUSR01C", o.pgmname());
            assertEquals("      AWS Mainframe Modernization       ", o.title01());
            assertEquals("              CardDemo                  ", o.title02());
            assertEquals("03/07/26", o.curdate());
            assertEquals("09:05:04", o.curtime());
            assertEquals(Pic.lowValues(20), o.fname(), "map was LOW-VALUES; input ignored on first entry");
            assertEquals(Pic.lowValues(8), o.userid());
            assertEquals(Pic.spaces(78), o.errmsg());
            assertEquals(Dfhbmsca.DFHDFCOL, o.errmsgColor());
            assertEquals(Field.FNAME, o.cursor());
            assertEquals(0, repo.size());
        }

        @Test
        void nonZeroNonOneContextIsTreatedAsFirstEntry() {
            CardDemoCommarea c = reenterCommarea();
            c.pgmContext(7);
            CicsResponse.SendAndReturn s = assertInstanceOf(CicsResponse.SendAndReturn.class,
                    service.handle(new CicsRequest(c, Dfhaid.DFHENTER, VALID)));
            assertTrue(s.commarea().isPgmReenter());
            assertEquals(0, repo.size());
        }

        @Test
        void pf3ReturnsToAdminMenu() {
            CicsResponse r = service.handle(new CicsRequest(reenterCommarea(), Dfhaid.DFHPF3, VALID));
            CicsResponse.Xctl x = assertInstanceOf(CicsResponse.Xctl.class, r);
            assertEquals("COADM01C", x.program());
            assertEquals("CU01", x.commarea().fromTranid());
            assertEquals("COUSR01C", x.commarea().fromProgram());
            assertTrue(x.commarea().isPgmEnter());
            assertEquals("ADMIN001", x.commarea().userId());
            assertEquals(0, repo.size(), "PF3 does not add the user");
        }

        @ParameterizedTest
        @EnumSource(value = Dfhaid.class, names = {"DFHPF1", "DFHPF2", "DFHPF5", "DFHPF12", "DFHCLEAR", "DFHPA1"})
        void otherKeysShowInvalidKeyMessage(Dfhaid aid) {
            UserAddScreenOutput o = send(aid, VALID).screen();
            assertEquals("Invalid key pressed. Please see below...", o.errmsgText());
            assertEquals(Pic.x("Invalid key pressed. Please see below...", 78), o.errmsg());
            assertEquals(Field.FNAME, o.cursor());
            assertEquals(Pic.x("JOHN", 20), o.fname(), "received input is echoed back");
            assertEquals(0, repo.size());
        }

        @ParameterizedTest
        @EnumSource(value = Dfhaid.class, names = {"DFHCLEAR", "DFHPA1", "DFHPA2", "DFHPA3"})
        void shortReadKeysWithNoFieldDataShowInvalidKeyMessage(Dfhaid aid) {
            UserAddScreenOutput o = send(aid, UserAddScreenInput.empty()).screen();
            assertEquals("Invalid key pressed. Please see below...", o.errmsgText());
            assertEquals(Field.FNAME, o.cursor());
            assertEquals(Pic.lowValues(20), o.fname());
            assertEquals(Pic.lowValues(8), o.userid());
            assertEquals(0, repo.size());
        }
    }

    @Nested
    class ProcessEnterKey {

        @Test
        void firstNameEmpty() {
            UserAddScreenOutput o = send(Dfhaid.DFHENTER,
                    new UserAddScreenInput("   ", "SMITH", "JSMITH01", "SECRET1", "U")).screen();
            assertEquals("First Name can NOT be empty...", o.errmsgText());
            assertEquals(Field.FNAME, o.cursor());
            assertEquals(0, repo.size());
        }

        @Test
        void lastNameEmpty() {
            UserAddScreenOutput o = send(Dfhaid.DFHENTER,
                    new UserAddScreenInput("JOHN", null, "JSMITH01", "SECRET1", "U")).screen();
            assertEquals("Last Name can NOT be empty...", o.errmsgText());
            assertEquals(Field.LNAME, o.cursor());
            assertEquals(Pic.x("JOHN", 20), o.fname(), "entered values are redisplayed");
            assertEquals(Pic.lowValues(20), o.lname());
        }

        @Test
        void userIdEmpty() {
            UserAddScreenOutput o = send(Dfhaid.DFHENTER,
                    new UserAddScreenInput("JOHN", "SMITH", "", "SECRET1", "U")).screen();
            assertEquals("User ID can NOT be empty...", o.errmsgText());
            assertEquals(Field.USERID, o.cursor());
        }

        @Test
        void passwordEmpty() {
            UserAddScreenOutput o = send(Dfhaid.DFHENTER,
                    new UserAddScreenInput("JOHN", "SMITH", "JSMITH01", " ", "U")).screen();
            assertEquals("Password can NOT be empty...", o.errmsgText());
            assertEquals(Field.PASSWD, o.cursor());
        }

        @Test
        void userTypeEmpty() {
            UserAddScreenOutput o = send(Dfhaid.DFHENTER,
                    new UserAddScreenInput("JOHN", "SMITH", "JSMITH01", "SECRET1", null)).screen();
            assertEquals("User Type can NOT be empty...", o.errmsgText());
            assertEquals(Field.USRTYPE, o.cursor());
            assertEquals(0, repo.size());
        }

        @Test
        void validationOrderReportsFirstEmptyFieldOnly() {
            UserAddScreenOutput o = send(Dfhaid.DFHENTER, UserAddScreenInput.empty()).screen();
            assertEquals("First Name can NOT be empty...", o.errmsgText());
            assertEquals(Field.FNAME, o.cursor());
        }

        @Test
        void validationErrorKeepsDefaultMessageColour() {
            UserAddScreenOutput o = send(Dfhaid.DFHENTER, UserAddScreenInput.empty()).screen();
            assertEquals(Dfhbmsca.DFHDFCOL, o.errmsgColor());
        }

        @Test
        void userTypeIsNotRestrictedToAOrU() {
            send(Dfhaid.DFHENTER, new UserAddScreenInput("JOHN", "SMITH", "JSMITH01", "SECRET1", "X"));
            assertEquals("X", repo.find("JSMITH01").orElseThrow().secUsrType());
        }
    }

    @Nested
    class WriteUserSecFile {

        @Test
        void normalWriteAddsRecordAndClearsScreen() {
            UserAddScreenOutput o = send(Dfhaid.DFHENTER, VALID).screen();

            SecUserData rec = repo.find("JSMITH01").orElseThrow();
            assertEquals("JSMITH01", rec.secUsrId());
            assertEquals(Pic.x("JOHN", 20), rec.secUsrFname());
            assertEquals(Pic.x("SMITH", 20), rec.secUsrLname());
            assertEquals("SECRET1 ", rec.secUsrPwd());
            assertEquals("U", rec.secUsrType());
            assertEquals(80, rec.toFixedWidth().length());

            assertEquals("User JSMITH01 has been added ...", o.errmsgText());
            assertEquals(Dfhbmsca.DFHGREEN, o.errmsgColor());
            assertEquals(Field.FNAME, o.cursor());
            assertEquals(Pic.spaces(20), o.fname());
            assertEquals(Pic.spaces(20), o.lname());
            assertEquals(Pic.spaces(8), o.userid());
            assertEquals(Pic.spaces(8), o.passwd());
            assertEquals(Pic.spaces(1), o.usrtype());
        }

        @Test
        void successMessageUserIdDelimitedBySpace() {
            UserAddScreenOutput o = send(Dfhaid.DFHENTER,
                    new UserAddScreenInput("JOHN", "SMITH", "AB CD", "SECRET1", "A")).screen();
            assertEquals("User AB has been added ...", o.errmsgText());
            assertEquals("AB CD   ", repo.find("AB CD").orElseThrow().secUsrId());
        }

        @Test
        void successMessageWithLeadingSpaceInUserId() {
            UserAddScreenOutput o = send(Dfhaid.DFHENTER,
                    new UserAddScreenInput("JOHN", "SMITH", " XYZ", "SECRET1", "A")).screen();
            assertEquals("User  has been added ...", o.errmsgText());
        }

        @Test
        void inputLongerThanFieldIsTruncatedToPicSize() {
            send(Dfhaid.DFHENTER, new UserAddScreenInput("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "SMITH",
                    "LONGUSERID", "PASSWORD99", "UA"));
            SecUserData rec = repo.find("LONGUSER").orElseThrow();
            assertEquals("ABCDEFGHIJKLMNOPQRST", rec.secUsrFname());
            assertEquals("PASSWORD", rec.secUsrPwd());
            assertEquals("U", rec.secUsrType());
        }

        @Test
        void duplicateUserIdShowsErrorAndCursorFollowsFirstMinusOneField() {
            send(Dfhaid.DFHENTER, VALID);
            UserAddScreenOutput o = send(Dfhaid.DFHENTER,
                    new UserAddScreenInput("JANE", "DOE", "JSMITH01", "OTHER", "A")).screen();
            assertEquals("User ID already exist...", o.errmsgText());
            // FNAMEL was already -1 (WHEN OTHER) when USERIDL is set -1; BMS picks the first in map order.
            assertEquals(Field.FNAME, o.cursor());
            assertEquals(Pic.x("JANE", 20), o.fname(), "input retained on error");
            assertEquals(Dfhbmsca.DFHDFCOL, o.errmsgColor());
            assertEquals(Pic.x("JOHN", 20), repo.find("JSMITH01").orElseThrow().secUsrFname());
            assertEquals(1, repo.size());
        }

        @Test
        void dupkeyRespTreatedAsDuplicate() {
            repo.forceResult(new WriteResult(CicsResp.DUPKEY, 0));
            assertEquals("User ID already exist...", send(Dfhaid.DFHENTER, VALID).screen().errmsgText());
        }

        @ParameterizedTest
        @EnumSource(value = CicsResp.class, names = {"NOTOPEN", "IOERR", "NOSPACE", "DISABLED", "INVREQ"})
        void otherRespShowsUnableToAdd(CicsResp resp) {
            repo.forceResult(new WriteResult(resp, 0));
            UserAddScreenOutput o = send(Dfhaid.DFHENTER, VALID).screen();
            assertEquals("Unable to Add User...", o.errmsgText());
            assertEquals(Field.FNAME, o.cursor());
            assertEquals(Pic.x("JOHN", 20), o.fname());
            assertEquals(0, repo.size());
        }

        @Test
        void taskEndsWithReturnTransidAndReenterCommarea() {
            CicsResponse.SendAndReturn s = send(Dfhaid.DFHENTER, VALID);
            assertEquals("CU01", s.transId());
            assertTrue(s.commarea().isPgmReenter());
        }
    }

    @Nested
    class ClearCurrentScreen {

        @Test
        void pf4ClearsFieldsWithoutWriting() {
            UserAddScreenOutput o = send(Dfhaid.DFHPF4, VALID).screen();
            assertEquals(Pic.spaces(20), o.fname());
            assertEquals(Pic.spaces(20), o.lname());
            assertEquals(Pic.spaces(8), o.userid());
            assertEquals(Pic.spaces(8), o.passwd());
            assertEquals(Pic.spaces(1), o.usrtype());
            assertEquals(Pic.spaces(78), o.errmsg());
            assertEquals(Field.FNAME, o.cursor());
            assertEquals(0, repo.size());
        }
    }

    @Nested
    class Paragraphs {

        @Test
        void returnToPrevScreenDefaultsBlankTargetToSignon() {
            Cousr01cService.Task t = service.newTask(CicsRequest.noCommarea());
            CardDemoCommarea c = new CardDemoCommarea();
            c.toProgram(Pic.lowValues(8));
            c.pgmContext(1);
            t.commarea(c);
            CicsResponse.Xctl x = (CicsResponse.Xctl) t.returnToPrevScreen();
            assertEquals("COSGN00C", x.program());
            assertTrue(x.commarea().isPgmEnter());
        }

        @Test
        void errFlagSetOnlyOnErrorPaths() {
            Cousr01cService.Task ok = service.newTask(new CicsRequest(reenterCommarea(), Dfhaid.DFHENTER, VALID));
            ok.mainPara();
            assertFalse(ok.errFlgOn());
            assertEquals(CicsResp.NORMAL, ok.writeResult().resp());

            Cousr01cService.Task dup = service.newTask(new CicsRequest(reenterCommarea(), Dfhaid.DFHENTER, VALID));
            dup.mainPara();
            assertTrue(dup.errFlgOn());
            assertEquals(CicsResp.DUPREC, dup.writeResult().resp());
        }

        @Test
        void delimitedBySpace() {
            assertEquals("ABC", Cousr01cService.delimitedBySpace("ABC     "));
            assertEquals("ABCDEFGH", Cousr01cService.delimitedBySpace("ABCDEFGH"));
            assertEquals("", Cousr01cService.delimitedBySpace(" ABC"));
        }

        @Test
        void noCursorWhenNoLengthIsMinusOne() {
            com.carddemo.programs.cousr01c.copybook.Cousr1aMap m =
                    new com.carddemo.programs.cousr01c.copybook.Cousr1aMap();
            assertTrue(m.symbolicCursor().isEmpty());
            m.cursor(Field.USRTYPE);
            m.cursor(Field.LNAME);
            assertEquals(Field.LNAME, m.symbolicCursor().orElseThrow());
            m.lowValues();
            assertNull(m.symbolicCursor().orElse(null));
        }
    }
}
