package com.carddemo.programs.cousr03c;

import com.carddemo.programs.cousr03c.copybook.Cousr03Map.Field;
import com.carddemo.programs.cousr03c.copybook.Dfhaid;
import com.carddemo.programs.cousr03c.copybook.SecUserData;
import com.carddemo.programs.cousr03c.dto.Cousr03cRequest;
import com.carddemo.programs.cousr03c.dto.Cousr03cResult;
import com.carddemo.programs.cousr03c.dto.Cousr03cResult.Action;
import com.carddemo.programs.cousr03c.dto.ScreenInput;
import com.carddemo.programs.cousr03c.dto.UserDeleteScreen;
import com.carddemo.programs.cousr03c.gateway.CicsResp;
import com.carddemo.programs.cousr03c.gateway.InMemoryUserSecurityFile;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;

import static com.carddemo.programs.cousr03c.dto.UserDeleteScreen.text;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class Cousr03cServiceTest {

    private static final Clock CLOCK = Clock.fixed(Instant.parse("2024-03-07T13:05:09.25Z"), ZoneOffset.UTC);

    private InMemoryUserSecurityFile usrsec;
    private List<String> console;
    private Cousr03cService service;

    @BeforeEach
    void setUp() {
        usrsec = new InMemoryUserSecurityFile();
        usrsec.put(SecUserData.of("USER0001", "LAWRENCE", "THOMAS", "PASSWORD", "U"));
        usrsec.put(SecUserData.of("ADMIN001", "MARGARET", "GOLD", "PASSWORD", "A"));
        usrsec.put(SecUserData.of("AB CD", "SPACED", "KEY", "PASSWORD", "U"));
        console = new ArrayList<>();
        service = new Cousr03cService(usrsec, CLOCK, console::add);
    }

    private static Cousr03cCommarea firstEntryCommarea(String selected) {
        Cousr03cCommarea c = new Cousr03cCommarea();
        c.fill((byte) ' ');
        c.setFromTranid("CU00");
        c.setFromProgram("COUSR00C");
        c.setUserId("ADMIN001");
        c.setUserType(Cousr03cCommarea.UserType.ADMIN);
        c.setPgmContext(0);
        c.setNextPageNo();
        if (selected != null) {
            c.setCu03UsrSelected(selected);
        }
        return c;
    }

    private static byte[] reenterCommarea() {
        Cousr03cCommarea c = firstEntryCommarea(null);
        c.setPgmContext(1);
        return c.toBytes();
    }

    private Cousr03cResult reenter(Dfhaid aid, String userId) {
        return service.execute(new Cousr03cRequest(reenterCommarea(), aid, ScreenInput.userId(userId)));
    }

    @Nested
    class MainPara {

        @Test
        void noCommareaTransfersToSignon() {
            Cousr03cResult r = service.execute(new Cousr03cRequest(null, Dfhaid.DFHENTER, null));
            assertEquals(Action.XCTL, r.action());
            assertEquals("COSGN00C", r.xctlProgram());
            assertEquals("CU03", r.commarea().fromTranid());
            assertEquals("COUSR03C", r.commarea().fromProgram());
            assertEquals("COSGN00C", r.commarea().toProgram());
            assertTrue(r.commarea().isPgmEnter());
            assertTrue(r.screens().isEmpty());
            assertEquals(194, r.commarea().toBytes().length);
        }

        @Test
        void emptyCommareaIsTreatedAsEibcalenZero() {
            Cousr03cResult r = service.execute(new Cousr03cRequest(new byte[0], Dfhaid.DFHENTER, null));
            assertEquals(Action.XCTL, r.action());
            assertEquals("COSGN00C", r.xctlProgram());
        }

        @Test
        void firstEntryWithoutSelectionSendsBlankScreen() {
            Cousr03cResult r = service.execute(new Cousr03cRequest(firstEntryCommarea(null).toBytes(), Dfhaid.DFHENTER, null));
            assertEquals(Action.RETURN_TRANSID, r.action());
            assertEquals("CU03", r.returnTransid());
            assertTrue(r.commarea().isPgmReenter());
            assertEquals(1, r.screens().size());
            UserDeleteScreen s = r.lastScreen();
            assertEquals("CU03", s.trnName());
            assertEquals("COUSR03C", s.pgmName());
            assertEquals("      AWS Mainframe Modernization       ", s.title01());
            assertEquals("              CardDemo                  ", s.title02());
            assertEquals("03/07/24", s.curDate());
            assertEquals("13:05:09", s.curTime());
            assertEquals("\0".repeat(8), s.usrIdIn());
            assertEquals(" ".repeat(78), s.errMsg());
            assertEquals(Field.USRIDIN, s.cursorField());
            assertEquals("DFHDFCOL", s.errMsgColor());
        }

        @Test
        void firstEntryWithSelectedUserLooksItUp() {
            Cousr03cResult r = service.execute(new Cousr03cRequest(firstEntryCommarea("USER0001").toBytes(), Dfhaid.DFHENTER, null));
            assertEquals(Action.RETURN_TRANSID, r.action());
            // READ NORMAL send, PROCESS-ENTER-KEY send, MAIN-PARA send
            assertEquals(3, r.screens().size());
            assertEquals("", text(r.screens().get(0).fname()));
            UserDeleteScreen s = r.lastScreen();
            assertEquals("USER0001", s.usrIdIn());
            assertEquals("LAWRENCE            ", s.fname());
            assertEquals("THOMAS              ", s.lname());
            assertEquals("U", s.usrType());
            assertEquals("Press PF5 key to delete this user ...", text(s.errMsg()));
            assertEquals("DFHNEUTR", s.errMsgColor());
            assertTrue(usrsec.find("USER0001").isPresent());
        }

        @Test
        void firstEntryWithSelectedUserNotFound() {
            Cousr03cResult r = service.execute(new Cousr03cRequest(firstEntryCommarea("NOSUCH").toBytes(), Dfhaid.DFHENTER, null));
            assertEquals(2, r.screens().size());
            UserDeleteScreen s = r.lastScreen();
            assertEquals("User ID NOT found...", text(s.errMsg()));
            assertEquals(" ".repeat(20), s.fname());
            assertEquals(Field.USRIDIN, s.cursorField());
        }

        @Test
        void firstEntrySelectionOfSpacesOrLowValuesIsIgnored() {
            Cousr03cCommarea spaces = firstEntryCommarea("        ");
            assertEquals(1, service.execute(new Cousr03cRequest(spaces.toBytes(), Dfhaid.DFHENTER, null)).screens().size());
            Cousr03cCommarea lows = firstEntryCommarea("\0\0\0\0\0\0\0\0");
            assertEquals(1, service.execute(new Cousr03cRequest(lows.toBytes(), Dfhaid.DFHENTER, null)).screens().size());
        }

        @Test
        void shortCommareaIsPaddedWithSpaces() {
            byte[] shortCa = new byte[34];
            System.arraycopy(reenterCommarea(), 0, shortCa, 0, 34);
            Cousr03cResult r = service.execute(new Cousr03cRequest(shortCa, Dfhaid.DFHENTER, ScreenInput.userId("ADMIN001")));
            assertTrue(r.commarea().isPgmReenter());
            assertEquals("        ", r.commarea().cu03UsrSelected());
            assertEquals(" ", r.commarea().cu03NextPageFlg());
            assertEquals("MARGARET", text(r.lastScreen().fname()));
        }

        @Test
        void returnedCommareaPreservesCallerFields() {
            Cousr03cResult r = reenter(Dfhaid.DFHENTER, "USER0001");
            assertEquals("ADMIN001", r.commarea().userId());
            assertTrue(r.commarea().isUsrtypAdmin());
            assertEquals("COUSR00C", r.commarea().fromProgram());
            assertTrue(usrsec.heldKey() == null, "READ UPDATE hold is released at end of task");
        }
    }

    @Nested
    class ProcessEnterKey {

        @Test
        void blankUserIdIsRejected() {
            Cousr03cResult r = reenter(Dfhaid.DFHENTER, "        ");
            assertEquals(1, r.screens().size());
            UserDeleteScreen s = r.lastScreen();
            assertEquals("User ID can NOT be empty...", text(s.errMsg()));
            assertEquals(Field.USRIDIN, s.cursorField());
            assertEquals("DFHDFCOL", s.errMsgColor());
        }

        @Test
        void mapFailLeavesLowValuesAndIsRejectedAsEmpty() {
            Cousr03cResult r = service.execute(new Cousr03cRequest(reenterCommarea(), Dfhaid.DFHENTER, null));
            assertEquals("User ID can NOT be empty...", text(r.lastScreen().errMsg()));
        }

        @Test
        void mixedSpacesAndLowValuesIsNotEmpty() {
            Cousr03cResult r = reenter(Dfhaid.DFHENTER, " \0");
            assertEquals("User ID NOT found...", text(r.lastScreen().errMsg()));
        }

        @Test
        void foundUserIsDisplayed() {
            Cousr03cResult r = reenter(Dfhaid.DFHENTER, "ADMIN001");
            assertEquals(2, r.screens().size());
            UserDeleteScreen s = r.lastScreen();
            assertEquals("MARGARET", text(s.fname()));
            assertEquals("GOLD", text(s.lname()));
            assertEquals("A", s.usrType());
            assertEquals("Press PF5 key to delete this user ...", text(s.errMsg()));
            assertEquals("DFHNEUTR", s.errMsgColor());
            assertEquals(Field.USRIDIN, s.cursorField());
        }

        @Test
        void shortUserIdIsSpacePadded() {
            usrsec.put(SecUserData.of("BOB", "ROBERT", "SMITH", "X", "U"));
            assertEquals("ROBERT", text(reenter(Dfhaid.DFHENTER, "BOB").lastScreen().fname()));
        }

        @Test
        void notFoundClearsNamesAndFlagsError() {
            Cousr03cResult r = service.execute(new Cousr03cRequest(reenterCommarea(), Dfhaid.DFHENTER,
                    new ScreenInput("ZZZ", "OLD FIRST", "OLD LAST", "A")));
            assertEquals(1, r.screens().size());
            UserDeleteScreen s = r.lastScreen();
            assertEquals("User ID NOT found...", text(s.errMsg()));
            assertEquals(" ".repeat(20), s.fname());
            assertEquals(" ".repeat(20), s.lname());
            assertEquals(" ", s.usrType());
            assertEquals(Field.USRIDIN, s.cursorField());
        }

        @Test
        void otherReadResponseIsLookupError() {
            usrsec.forceNextReadResponse(new CicsResp(CicsResp.NOTOPEN, 0));
            Cousr03cResult r = reenter(Dfhaid.DFHENTER, "USER0001");
            UserDeleteScreen s = r.lastScreen();
            assertEquals(1, r.screens().size());
            assertEquals("Unable to lookup User...", text(s.errMsg()));
            // FNAMEL = -1 but USRIDINL is already -1 and precedes it in the map
            assertEquals(Field.USRIDIN, s.cursorField());
            assertEquals(List.of("RESP:+000000019REAS:+000000000"), r.displayLines());
            assertEquals(r.displayLines(), console);
        }
    }

    @Nested
    class DeleteUserInfo {

        @Test
        void deletesUserAndClearsScreen() {
            Cousr03cResult r = reenter(Dfhaid.DFHPF5, "USER0001");
            assertEquals(2, r.screens().size());
            assertEquals("Press PF5 key to delete this user ...", text(r.screens().get(0).errMsg()));
            UserDeleteScreen s = r.lastScreen();
            assertEquals("User USER0001 has been deleted ...", text(s.errMsg()));
            assertEquals("DFHGREEN", s.errMsgColor());
            assertEquals(" ".repeat(8), s.usrIdIn());
            assertEquals(" ".repeat(20), s.fname());
            assertEquals(" ".repeat(20), s.lname());
            assertEquals(" ", s.usrType());
            assertEquals(Field.USRIDIN, s.cursorField());
            assertFalse(usrsec.find("USER0001").isPresent());
            assertEquals(Action.RETURN_TRANSID, r.action());
        }

        @Test
        void deleteMessageStopsAtFirstSpaceOfUserId() {
            Cousr03cResult r = reenter(Dfhaid.DFHPF5, "AB CD");
            assertEquals("User AB has been deleted ...", text(r.lastScreen().errMsg()));
            assertFalse(usrsec.find("AB CD").isPresent());
        }

        @Test
        void blankUserIdIsRejectedWithoutDeleting() {
            Cousr03cResult r = reenter(Dfhaid.DFHPF5, "");
            assertEquals(1, r.screens().size());
            assertEquals("User ID can NOT be empty...", text(r.lastScreen().errMsg()));
            assertEquals(3, usrsec.size());
        }

        @Test
        void unknownUserStillIssuesDeleteWhichFailsWithInvreq() {
            Cousr03cResult r = reenter(Dfhaid.DFHPF5, "NOSUCH");
            assertEquals(2, r.screens().size());
            assertEquals("User ID NOT found...", text(r.screens().get(0).errMsg()));
            assertEquals("Unable to Update User...", text(r.lastScreen().errMsg()));
            assertEquals(List.of("RESP:+000000016REAS:+000000000"), r.displayLines());
            assertEquals(3, usrsec.size());
        }

        @Test
        void deleteNotFoundResponse() {
            usrsec.forceNextDeleteResponse(CicsResp.of(CicsResp.NOTFND));
            Cousr03cResult r = reenter(Dfhaid.DFHPF5, "USER0001");
            UserDeleteScreen s = r.lastScreen();
            assertEquals("User ID NOT found...", text(s.errMsg()));
                        assertEquals(Field.USRIDIN, s.cursorField());
            assertTrue(r.displayLines().isEmpty());
        }

        @Test
        void deleteOtherResponseIsUpdateError() {
            usrsec.forceNextDeleteResponse(new CicsResp(CicsResp.IOERR, 7));
            Cousr03cResult r = reenter(Dfhaid.DFHPF5, "USER0001");
            UserDeleteScreen s = r.lastScreen();
            assertEquals("Unable to Update User...", text(s.errMsg()));
            assertEquals("USER0001", s.usrIdIn());
            assertEquals(List.of("RESP:+000000017REAS:+000000007"), r.displayLines());
            assertTrue(usrsec.find("USER0001").isPresent());
        }
    }

    @Nested
    class FunctionKeys {

        @Test
        void pf3ReturnsToAdminMenuWhenNoCaller() {
            Cousr03cCommarea c = Cousr03cCommarea.fromDfhcommarea(reenterCommarea());
            c.setFromProgram("");
            Cousr03cResult r = service.execute(new Cousr03cRequest(c.toBytes(), Dfhaid.DFHPF3, ScreenInput.userId("")));
            assertEquals(Action.XCTL, r.action());
            assertEquals("COADM01C", r.xctlProgram());
            assertEquals("COUSR03C", r.commarea().fromProgram());
            assertEquals("CU03", r.commarea().fromTranid());
            assertTrue(r.commarea().isPgmEnter());
        }

        @Test
        void pf3ReturnsToLowValueCallerAsAdminMenu() {
            Cousr03cCommarea c = Cousr03cCommarea.fromDfhcommarea(reenterCommarea());
            byte[] b = c.toBytes();
            java.util.Arrays.fill(b, 4, 12, (byte) 0);
            Cousr03cResult r = service.execute(new Cousr03cRequest(b, Dfhaid.DFHPF3, null));
            assertEquals("COADM01C", r.xctlProgram());
        }

        @Test
        void pf3ReturnsToCallingProgram() {
            Cousr03cResult r = reenter(Dfhaid.DFHPF3, "");
            assertEquals(Action.XCTL, r.action());
            assertEquals("COUSR00C", r.xctlProgram());
            assertEquals("COUSR00C", r.commarea().toProgram());
            assertTrue(r.screens().isEmpty());
        }

        @Test
        void pf12ReturnsToAdminMenu() {
            Cousr03cResult r = reenter(Dfhaid.DFHPF12, "USER0001");
            assertEquals(Action.XCTL, r.action());
            assertEquals("COADM01C", r.xctlProgram());
            assertNull(r.returnTransid());
        }

        @Test
        void pf4ClearsScreen() {
            Cousr03cResult r = service.execute(new Cousr03cRequest(reenterCommarea(), Dfhaid.DFHPF4,
                    new ScreenInput("USER0001", "LAWRENCE", "THOMAS", "U")));
            assertEquals(1, r.screens().size());
            UserDeleteScreen s = r.lastScreen();
            assertEquals(" ".repeat(8), s.usrIdIn());
            assertEquals(" ".repeat(20), s.fname());
            assertEquals(" ".repeat(78), s.errMsg());
            assertEquals(Field.USRIDIN, s.cursorField());
            assertTrue(usrsec.find("USER0001").isPresent());
        }

        @Test
        void otherKeyShowsInvalidKeyMessage() {
            for (Dfhaid aid : new Dfhaid[] {Dfhaid.DFHPF7, Dfhaid.DFHCLEAR, Dfhaid.DFHPA1, Dfhaid.DFHPF1}) {
                Cousr03cResult r = reenter(aid, "USER0001");
                assertEquals(Action.RETURN_TRANSID, r.action(), aid.name());
                UserDeleteScreen s = r.lastScreen();
                assertEquals("Invalid key pressed. Please see below..." + " ".repeat(38), s.errMsg());
                assertEquals("USER0001", s.usrIdIn());
            }
        }

        @Test
        void commareaRoundTripsThroughReturn() {
            Cousr03cResult r = reenter(Dfhaid.DFHENTER, "USER0001");
            assertArrayEquals(reenterCommarea(), r.commarea().toBytes());
        }
    }
}
