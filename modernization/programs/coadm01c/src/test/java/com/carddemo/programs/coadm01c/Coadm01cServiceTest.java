package com.carddemo.programs.coadm01c;

import com.carddemo.programs.coadm01c.Coadm01cOutcome.Returned;
import com.carddemo.programs.coadm01c.Coadm01cOutcome.Transferred;
import com.carddemo.programs.coadm01c.copybook.AdminMenuOptions;
import com.carddemo.programs.coadm01c.copybook.AdminMenuOptions.AdminMenuOption;
import com.carddemo.programs.coadm01c.copybook.CardDemoCommarea;
import com.carddemo.programs.coadm01c.copybook.Coadm1aInput;
import com.carddemo.programs.coadm01c.copybook.Coadm1aOutput;
import com.carddemo.programs.coadm01c.copybook.CobolFields;
import com.carddemo.programs.coadm01c.copybook.Dfhaid;
import com.carddemo.programs.coadm01c.copybook.Dfhbmsca;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.EnumSource;

import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class Coadm01cServiceTest {

    static final Clock CLOCK = Clock.fixed(Instant.parse("2024-03-07T09:05:04.12Z"), ZoneOffset.UTC);

    static final String INVALID_OPTION_MSG = CobolFields.alnum("Please enter a valid option number...", 78);
    static final String NOT_INSTALLED_MSG = CobolFields.alnum("This option is not installed ...", 78);
    static final String INVALID_KEY_MSG = CobolFields.alnum("Invalid key pressed. Please see below...", 78);

    InMemoryProgramControlGateway gateway = InMemoryProgramControlGateway.allCardDemoPrograms();
    Coadm01cService service = new Coadm01cService(gateway, CLOCK);

    static CardDemoCommarea adminCommarea(CardDemoCommarea.PgmContext context) {
        CardDemoCommarea c = CardDemoCommarea.fromRecord("");
        c.setFromTranid("CC00");
        c.setFromProgram("COSGN00C");
        c.setUserId("ADMIN001");
        c.setUserType(CardDemoCommarea.UserType.ADMIN);
        c.setPgmContext(context);
        return c;
    }

    Coadm01cOutcome enter(String optionI) {
        return service.execute(Coadm01cRequest.of(adminCommarea(CardDemoCommarea.PgmContext.REENTER),
                Dfhaid.DFHENTER, Coadm1aInput.ofOption(optionI)));
    }

    static void assertHeader(Coadm1aOutput s) {
        assertEquals("CA00", s.trnname());
        assertEquals("COADM01C", s.pgmname());
        assertEquals("      AWS Mainframe Modernization       ", s.title01());
        assertEquals("              CardDemo                  ", s.title02());
        assertEquals("03/07/24", s.curdate());
        assertEquals("09:05:04", s.curtime());
    }

    static void assertStandardMenu(Coadm1aOutput s) {
        assertEquals(CobolFields.alnum("01. User List (Security)", 40), s.optn(1));
        assertEquals(CobolFields.alnum("02. User Add (Security)", 40), s.optn(2));
        assertEquals(CobolFields.alnum("03. User Update (Security)", 40), s.optn(3));
        assertEquals(CobolFields.alnum("04. User Delete (Security)", 40), s.optn(4));
        assertEquals(CobolFields.alnum("05. Transaction Type List/Update (Db2)", 40), s.optn(5));
        assertEquals(CobolFields.alnum("06. Transaction Type Maintenance (Db2)", 40), s.optn(6));
    }

    @Nested
    class MainPara {

        @Test
        void noCommareaTransfersToSignonWithoutCommarea() {
            Transferred t = assertInstanceOf(Transferred.class, service.execute(Coadm01cRequest.withoutCommarea()));
            assertEquals("COSGN00C", t.program());
            assertNull(t.commarea());
            assertEquals(List.of(new InMemoryProgramControlGateway.Transfer("COSGN00C", null)), gateway.transfers());
        }

        @Test
        void firstEntrySendsMenuAndReturnsWithReenterContext() {
            Returned r = assertInstanceOf(Returned.class, service.execute(Coadm01cRequest.of(
                    adminCommarea(CardDemoCommarea.PgmContext.ENTER), Dfhaid.DFHENTER, null)));

            assertEquals("CA00", r.transId());
            assertTrue(r.commarea().isPgmReenter());
            assertEquals("CC00", r.commarea().fromTranid());
            assertEquals("ADMIN001", r.commarea().userId());
            Coadm1aOutput s = r.screen();
            assertHeader(s);
            assertStandardMenu(s);
            for (int i = 7; i <= 12; i++) {
                assertTrue(CobolFields.isLowValues(s.optn(i)), "OPTN0" + i + "O stays LOW-VALUES");
            }
            assertTrue(CobolFields.isLowValues(s.option()));
            assertEquals(CobolFields.spaces(78), s.errmsg());
            assertEquals(Dfhbmsca.DFHDFCOL.code, s.errmsgc());
            assertTrue(gateway.transfers().isEmpty());
        }

        @Test
        void shortCommareaIsSpacePaddedAndTreatedAsFirstEntry() {
            Returned r = assertInstanceOf(Returned.class,
                    service.execute(new Coadm01cRequest("CC00COSGN00C", Dfhaid.DFHENTER, null)));
            assertEquals("CC00COSGN00C" + " ".repeat(21) + "1" + " ".repeat(126), r.commarea().toRecord());
        }

        @Test
        void pf3TransfersToSignonWithoutCommarea() {
            Transferred t = assertInstanceOf(Transferred.class, service.execute(Coadm01cRequest.of(
                    adminCommarea(CardDemoCommarea.PgmContext.REENTER), Dfhaid.DFHPF3, Coadm1aInput.ofOption("01"))));
            assertEquals("COSGN00C", t.program());
            assertNull(t.commarea());
        }

        @ParameterizedTest
        @EnumSource(value = Dfhaid.class, names = {"DFHENTER", "DFHPF3"}, mode = EnumSource.Mode.EXCLUDE)
        void anyOtherKeyShowsInvalidKeyMessage(Dfhaid aid) {
            Returned r = assertInstanceOf(Returned.class, service.execute(Coadm01cRequest.of(
                    adminCommarea(CardDemoCommarea.PgmContext.REENTER), aid, Coadm1aInput.ofOption("02"))));
            assertEquals(INVALID_KEY_MSG, r.screen().errmsg());
            assertEquals("02", r.screen().option(), "received OPTIONI is echoed back via the redefined OPTIONO");
            assertEquals(Dfhbmsca.DFHDFCOL.code, r.screen().errmsgc());
            assertHeader(r.screen());
            assertStandardMenu(r.screen());
            assertTrue(r.commarea().isPgmReenter());
            assertTrue(gateway.transfers().isEmpty());
        }

        @Test
        void mapfailOnEnterLeavesOptionLowValuesAndIsInvalid() {
            Returned r = assertInstanceOf(Returned.class, service.execute(Coadm01cRequest.of(
                    adminCommarea(CardDemoCommarea.PgmContext.REENTER), Dfhaid.DFHENTER, null)));
            assertEquals(INVALID_OPTION_MSG, r.screen().errmsg());
        }
    }

    @Nested
    class ProcessEnterKey {

        @ParameterizedTest
        @CsvSource({"01,COUSR00C", "02,COUSR01C", "03,COUSR02C", "04,COUSR03C", "05,COTRTLIC", "06,COTRTUPC"})
        void validOptionTransfersWithCommarea(String option, String program) {
            Transferred t = assertInstanceOf(Transferred.class, enter(option));
            assertEquals(program, t.program());
            CardDemoCommarea c = t.commarea();
            assertEquals("CA00", c.fromTranid());
            assertEquals("COADM01C", c.fromProgram());
            assertTrue(c.isPgmEnter());
            assertEquals("ADMIN001", c.userId());
            assertTrue(c.isUsrTypAdmin());
            assertEquals(1, gateway.transfers().size());
            assertEquals(program, gateway.transfers().get(0).program());
        }

        @ParameterizedTest(name = "[{0}] -> option {1}")
        @CsvSource(value = {"'3 ',03,COUSR02C", "' 3',03,COUSR02C", "'1 ',01,COUSR00C", "'06',06,COTRTUPC"})
        void trailingAndLeadingSpacesAreZeroFilled(String optionI, String expected, String program) {
            Transferred t = assertInstanceOf(Transferred.class, enter(optionI));
            assertEquals(program, t.program());
        }

        @ParameterizedTest(name = "[{0}] -> OPTIONO [{1}]")
        @CsvSource(value = {"'  ',00", "'00',00", "' 0',00", "'07',07", "'99',99", "'7 ',07",
                "'ab',ab", "'0a',0a", "'-1',-1", "'1-',1-"})
        void invalidOptionShowsMessageAndEchoesOption(String optionI, String optionO) {
            Returned r = assertInstanceOf(Returned.class, enter(optionI));
            assertEquals(INVALID_OPTION_MSG, r.screen().errmsg());
            assertEquals(optionO, r.screen().option());
            assertEquals(Dfhbmsca.DFHDFCOL.code, r.screen().errmsgc());
            assertEquals("CA00", r.transId());
            assertTrue(r.commarea().isPgmReenter());
            assertHeader(r.screen());
            assertStandardMenu(r.screen());
            assertTrue(gateway.transfers().isEmpty());
        }

        @Test
        void lowValuesOptionIsInvalid() {
            Returned r = assertInstanceOf(Returned.class, enter(null));
            assertEquals(INVALID_OPTION_MSG, r.screen().errmsg());
            assertTrue(CobolFields.isLowValues(r.screen().option()));
        }

        @Test
        void bmsJustifyRightZeroMapsKeyedSingleDigit() {
            assertEquals("03", Coadm1aInput.keyed("3").option());
            Transferred t = assertInstanceOf(Transferred.class, service.execute(Coadm01cRequest.of(
                    adminCommarea(CardDemoCommarea.PgmContext.REENTER), Dfhaid.DFHENTER, Coadm1aInput.keyed("3"))));
            assertEquals("COUSR02C", t.program());
        }

        @Test
        void optionAboveCountIsInvalidWithPreDb2MenuOfFour() {
            AdminMenuOptions four = new AdminMenuOptions(4, AdminMenuOptions.standard().options());
            service = new Coadm01cService(gateway, CLOCK, four);
            Returned r = assertInstanceOf(Returned.class, enter("05"));
            assertEquals(INVALID_OPTION_MSG, r.screen().errmsg());
            assertTrue(CobolFields.isLowValues(r.screen().optn(5)), "only CDEMO-ADMIN-OPT-COUNT options are built");
        }

        @Test
        void dummyProgramShowsNotInstalledInGreenWithoutTransfer() {
            AdminMenuOptions withDummy = new AdminMenuOptions(2, List.of(
                    new AdminMenuOption(1, "User List (Security)", "COUSR00C"),
                    new AdminMenuOption(2, "Coming soon", "DUMMY01C")));
            service = new Coadm01cService(gateway, CLOCK, withDummy);
            Returned r = assertInstanceOf(Returned.class, enter("02"));
            assertEquals(NOT_INSTALLED_MSG, r.screen().errmsg());
            assertEquals(Dfhbmsca.DFHGREEN.code, r.screen().errmsgc());
            assertEquals("02", r.screen().option());
            assertTrue(r.commarea().isPgmReenter(), "commarea routing fields untouched on the DUMMY path");
            assertEquals("CC00", r.commarea().fromTranid());
            assertTrue(gateway.transfers().isEmpty());
        }
    }

    @Nested
    class PgmidErrErrPara {

        @Test
        void uninstalledOptionProgramShowsNotInstalledAndReturnsWithEnterContext() {
            gateway = new InMemoryProgramControlGateway(Set.of("COSGN00C", "COUSR00C"));
            service = new Coadm01cService(gateway, CLOCK);
            Returned r = assertInstanceOf(Returned.class, enter("05"));

            assertEquals(NOT_INSTALLED_MSG, r.screen().errmsg());
            assertEquals(Dfhbmsca.DFHGREEN.code, r.screen().errmsgc());
            assertEquals("05", r.screen().option());
            assertHeader(r.screen());
            assertStandardMenu(r.screen());
            assertEquals("CA00", r.transId());
            // routing fields were set before the failed XCTL, so the next task re-enters as a first entry
            assertEquals("CA00", r.commarea().fromTranid());
            assertEquals("COADM01C", r.commarea().fromProgram());
            assertTrue(r.commarea().isPgmEnter());
        }

        @Test
        void uninstalledSignonProgramOnNoCommareaShowsNotInstalled() {
            gateway = new InMemoryProgramControlGateway(Set.of());
            service = new Coadm01cService(gateway, CLOCK);
            Returned r = assertInstanceOf(Returned.class, service.execute(Coadm01cRequest.withoutCommarea()));
            assertEquals(NOT_INSTALLED_MSG, r.screen().errmsg());
            assertEquals("COSGN00C", r.commarea().fromProgram());
            assertEquals("COSGN00C", r.commarea().toProgram());
        }

        @Test
        void uninstalledSignonProgramOnPf3ShowsNotInstalled() {
            gateway = new InMemoryProgramControlGateway(Set.of());
            service = new Coadm01cService(gateway, CLOCK);
            Returned r = assertInstanceOf(Returned.class, service.execute(Coadm01cRequest.of(
                    adminCommarea(CardDemoCommarea.PgmContext.REENTER), Dfhaid.DFHPF3, Coadm1aInput.ofOption("01"))));
            assertEquals(NOT_INSTALLED_MSG, r.screen().errmsg());
            assertEquals("01", r.screen().option());
            assertTrue(r.commarea().isPgmReenter());
        }
    }

    @Nested
    class PopulateHeaderInfo {

        @Test
        void yearIsLastTwoDigitsAndTimeDropsHundredths() {
            Clock y2k = Clock.fixed(Instant.parse("2000-12-31T23:59:58.99Z"), ZoneOffset.UTC);
            service = new Coadm01cService(gateway, y2k);
            Returned r = assertInstanceOf(Returned.class, service.execute(Coadm01cRequest.of(
                    adminCommarea(CardDemoCommarea.PgmContext.ENTER), Dfhaid.DFHENTER, null)));
            assertEquals("12/31/00", r.screen().curdate());
            assertEquals("23:59:58", r.screen().curtime());
        }
    }

    @Nested
    class BuildMenuOptions {

        @Test
        void optionTextIsTruncatedToFortyCharacters() {
            AdminMenuOptions longName = new AdminMenuOptions(1, List.of(
                    new AdminMenuOption(9, "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789", "COUSR00C")));
            service = new Coadm01cService(gateway, CLOCK, longName);
            Returned r = assertInstanceOf(Returned.class, service.execute(Coadm01cRequest.of(
                    adminCommarea(CardDemoCommarea.PgmContext.ENTER), Dfhaid.DFHENTER, null)));
            // 2 + 2 + 35 = 39 bytes STRINGed into PIC X(40); the 36th name char is cut by PIC X(35)
            assertEquals("09. ABCDEFGHIJKLMNOPQRSTUVWXYZ012345678 ", r.screen().optn(1));
        }
    }
}
