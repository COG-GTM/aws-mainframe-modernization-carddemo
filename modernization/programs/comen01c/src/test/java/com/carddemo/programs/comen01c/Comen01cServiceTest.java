package com.carddemo.programs.comen01c;

import com.carddemo.programs.comen01c.copybook.CardDemoCommarea;
import com.carddemo.programs.comen01c.copybook.CobolText;
import com.carddemo.programs.comen01c.copybook.Dfhaid;
import com.carddemo.programs.comen01c.copybook.Dfhbmsca;
import com.carddemo.programs.comen01c.copybook.MainMenuOptions;
import com.carddemo.programs.comen01c.copybook.MainMenuOptions.MenuOption;
import com.carddemo.programs.comen01c.dto.MenuScreenRequest;
import com.carddemo.programs.comen01c.dto.MenuScreenResponse;
import com.carddemo.programs.comen01c.gateway.InMemoryProgramInquiryGateway;
import com.carddemo.programs.comen01c.gateway.InMemoryScreenGateway;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.ValueSource;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.time.Clock;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class Comen01cServiceTest {

    private static final Clock CLOCK = Clock.fixed(
            LocalDateTime.of(2026, 11, 26, 7, 5, 9, 870_000_000).toInstant(ZoneOffset.UTC), ZoneOffset.UTC);

    private InMemoryScreenGateway screen;

    private ProgramOutcome run(CicsRequest request, MenuScreenRequest input, MainMenuOptions menu, String... installed) {
        screen = new InMemoryScreenGateway(input);
        return new Comen01cService(screen, new InMemoryProgramInquiryGateway(installed), menu, CLOCK).execute(request);
    }

    private ProgramOutcome run(CicsRequest request, MenuScreenRequest input, String... installed) {
        return run(request, input, MainMenuOptions.standard(), installed);
    }

    private static CardDemoCommarea commarea(char userType, int context) {
        CardDemoCommarea c = CardDemoCommarea.fromBytes(CobolText.spaces(CardDemoCommarea.LENGTH));
        c.fromTranid("CC00");
        c.fromProgram("COSGN00C");
        c.userId("USER0001");
        c.userType(String.valueOf(userType));
        c.pgmContext(context);
        c.custId(0);
        c.acctId(0);
        c.cardNum(0);
        return c;
    }

    private static CicsRequest reenter(Dfhaid aid) {
        return new CicsRequest(commarea('U', 1).toBytes(), aid);
    }

    private ProgramOutcome enter(String typedOption, String... installed) {
        return run(reenter(Dfhaid.DFHENTER), MenuScreenRequest.raw(typedOption), installed);
    }

    private static CardDemoCommarea returned(ProgramOutcome outcome) {
        ProgramOutcome.Return r = assertInstanceOf(ProgramOutcome.Return.class, outcome);
        assertEquals("CM00", r.transId());
        return CardDemoCommarea.fromBytes(r.commarea());
    }

    @Nested
    class MainPara {

        @Test
        void eibcalenZeroTransfersToSignonWithoutCommarea() {
            ProgramOutcome out = run(new CicsRequest(null, Dfhaid.DFHENTER), MenuScreenRequest.notTransmitted());
            assertEquals(new ProgramOutcome.Xctl("COSGN00C", null), out);
            assertTrue(screen.sent().isEmpty());
        }

        @Test
        void emptyCommareaIsTreatedAsEibcalenZero() {
            ProgramOutcome out = run(new CicsRequest("", Dfhaid.DFHENTER), MenuScreenRequest.notTransmitted());
            assertEquals(new ProgramOutcome.Xctl("COSGN00C", null), out);
        }

        @Test
        void firstEntrySetsReenterSendsMenuAndReturnsCm00() {
            ProgramOutcome out = run(new CicsRequest(commarea('U', 0).toBytes(), Dfhaid.DFHENTER),
                    MenuScreenRequest.raw("05"));
            CardDemoCommarea c = returned(out);
            assertTrue(c.isPgmReenter());
            assertEquals("CC00", c.fromTranid());
            assertEquals("COSGN00C", c.fromProgram());

            assertEquals(1, screen.sent().size());
            MenuScreenResponse s = screen.lastSent();
            assertTrue(s.erase());
            assertEquals("CM00", s.trnnameO());
            assertEquals("COMEN01C", s.pgmnameO());
            assertEquals("      AWS Mainframe Modernization       ", s.title01O());
            assertEquals("              CardDemo                  ", s.title02O());
            assertEquals(CobolText.spaces(78), s.errmsgO());
            assertEquals(CobolText.lowValues(2), s.optionO(), "MOVE LOW-VALUES TO COMEN1AO; RECEIVE not issued");
            assertEquals(Dfhbmsca.DFHDFCOL, s.errmsgC());
        }

        @Test
        void firstEntryWhenContextIsNotNumericStillTreatedAsEnter() {
            String raw = commarea('U', 0).toBytes();
            raw = raw.substring(0, 33) + " " + raw.substring(34);
            CardDemoCommarea c = returned(run(new CicsRequest(raw, Dfhaid.DFHENTER), MenuScreenRequest.notTransmitted()));
            assertTrue(c.isPgmReenter());
            assertEquals(1, screen.sent().size());
        }

        @Test
        void pf3TransfersToSignonWithoutCommarea() {
            ProgramOutcome out = run(reenter(Dfhaid.DFHPF3), MenuScreenRequest.raw("01"));
            assertEquals(new ProgramOutcome.Xctl("COSGN00C", null), out);
            assertTrue(screen.sent().isEmpty());
        }

        @ParameterizedTest
        @EnumSource(value = Dfhaid.class, names = {"DFHPF1", "DFHPF4", "DFHPF12", "DFHCLEAR", "DFHPA1", "DFHPF24"})
        void otherKeysShowInvalidKeyMessage(Dfhaid aid) {
            CardDemoCommarea c = returned(run(reenter(aid), MenuScreenRequest.raw("01")));
            assertTrue(c.isPgmReenter());
            assertEquals(1, screen.sent().size());
            MenuScreenResponse s = screen.lastSent();
            assertEquals(CobolText.move("Invalid key pressed. Please see below...", 78), s.errmsgO());
            assertEquals("01", s.optionO(), "received OPTIONI is echoed back via the REDEFINES");
        }
    }

    @Nested
    class ProcessEnterKey {

        @Test
        void validOptionTransfersControlWithUpdatedCommarea() {
            ProgramOutcome out = enter("01");
            ProgramOutcome.Xctl x = assertInstanceOf(ProgramOutcome.Xctl.class, out);
            assertEquals("COACTVWC", x.program());
            assertNotNull(x.commarea());
            CardDemoCommarea c = CardDemoCommarea.fromBytes(x.commarea());
            assertEquals("CM00", c.fromTranid());
            assertEquals("COMEN01C", c.fromProgram());
            assertTrue(c.isPgmEnter());
            assertEquals("USER0001", c.userId());
            assertEquals("U", c.userType());
            assertTrue(screen.sent().isEmpty());
        }

        @ParameterizedTest
        @ValueSource(strings = {"1 ", " 1", "01"})
        void leftOrRightAlignedDigitsSelectSameOption(String optionI) {
            assertEquals("COACTVWC", ((ProgramOutcome.Xctl) enter(optionI)).program());
        }

        @Test
        void eachStandardOptionRoutesToItsProgram() {
            List<String> expected = List.of("COACTVWC", "COACTUPC", "COCRDLIC", "COCRDSLC", "COCRDUPC",
                    "COTRN00C", "COTRN01C", "COTRN02C", "CORPT00C", "COBIL00C", "COPAUS0C");
            for (int i = 1; i <= 11; i++) {
                ProgramOutcome out = enter(CobolText.pic9(i, 2), "COPAUS0C");
                assertEquals(expected.get(i - 1), ((ProgramOutcome.Xctl) out).program(), "option " + i);
            }
        }

        @ParameterizedTest
        @ValueSource(strings = {"  ", "00", "0 ", "12", "99", "A1", "1A"})
        void invalidOptionShowsValidOptionMessage(String optionI) {
            CardDemoCommarea c = returned(enter(optionI));
            assertTrue(c.isPgmReenter());
            assertEquals(1, screen.sent().size());
            assertEquals("Please enter a valid option number...", screen.lastSent().message());
            assertEquals(CobolText.move("Please enter a valid option number...", 78), screen.lastSent().errmsgO());
        }

        @Test
        void invalidOptionEchoesNormalisedOption() {
            enter("  ");
            assertEquals("00", screen.lastSent().optionO());
            enter("2 ");
            assertNull(screen.lastSent(), "valid option transfers without sending");
        }

        @Test
        void optionNotTransmittedIsInvalid() {
            returned(run(reenter(Dfhaid.DFHENTER), MenuScreenRequest.notTransmitted()));
            assertEquals("Please enter a valid option number...", screen.lastSent().message());
        }

        @Test
        void keyedDigitIsRightJustifiedZeroFilledByBms() {
            assertEquals("07", MenuScreenRequest.keyed("7").optionI());
            ProgramOutcome out = run(reenter(Dfhaid.DFHENTER), MenuScreenRequest.keyed("7"));
            assertEquals("COTRN01C", ((ProgramOutcome.Xctl) out).program());
        }

        @Test
        void pendingAuthorizationNotInstalledShowsRedMessage() {
            CardDemoCommarea c = returned(enter("11"));
            assertTrue(c.isPgmReenter());
            assertEquals("CC00", c.fromTranid(), "commarea untouched when INQUIRE fails");
            MenuScreenResponse s = screen.lastSent();
            assertEquals(1, screen.sent().size());
            assertEquals("This option Pending Authorization View is not installed...", s.message());
            assertEquals(Dfhbmsca.DFHRED, s.errmsgC());
            assertEquals("11", s.optionO());
        }

        @Test
        void pendingAuthorizationInstalledTransfers() {
            ProgramOutcome.Xctl x = assertInstanceOf(ProgramOutcome.Xctl.class, enter("11", "COPAUS0C"));
            assertEquals("COPAUS0C", x.program());
            CardDemoCommarea c = CardDemoCommarea.fromBytes(x.commarea());
            assertEquals("CM00", c.fromTranid());
            assertEquals("COMEN01C", c.fromProgram());
            assertTrue(c.isPgmEnter());
        }

        @Test
        void dummyProgramShowsComingSoonInGreen() {
            MainMenuOptions menu = new MainMenuOptions(2, List.of(
                    new MenuOption(1, "Account View", "COACTVWC", "U"),
                    new MenuOption(2, "Statement Print", "DUMMY001", "U")));
            returned(run(reenter(Dfhaid.DFHENTER), MenuScreenRequest.raw("02"), menu));
            MenuScreenResponse s = screen.lastSent();
            // NAME is DELIMITED BY SPACE and no space precedes the literal, exactly as in COBOL
            assertEquals("This option Statementis coming soon ...", s.message());
            assertEquals(Dfhbmsca.DFHGREEN, s.errmsgC());
        }

        @Test
        void adminOnlyOptionDeniedForRegularUser() {
            MainMenuOptions menu = new MainMenuOptions(2, List.of(
                    new MenuOption(1, "Account View", "COACTVWC", "U"),
                    new MenuOption(2, "User List", "COUSR00C", "A")));
            returned(run(reenter(Dfhaid.DFHENTER), MenuScreenRequest.raw("02"), menu));
            assertEquals(1, screen.sent().size());
            assertEquals(CobolText.move("No access - Admin Only option... ", 78), screen.lastSent().errmsgO());
        }

        @Test
        void adminOnlyOptionAllowedForAdminUser() {
            MainMenuOptions menu = new MainMenuOptions(2, List.of(
                    new MenuOption(1, "Account View", "COACTVWC", "U"),
                    new MenuOption(2, "User List", "COUSR00C", "A")));
            CicsRequest req = new CicsRequest(commarea('A', 1).toBytes(), Dfhaid.DFHENTER);
            ProgramOutcome out = run(req, MenuScreenRequest.raw("02"), menu);
            assertEquals("COUSR00C", ((ProgramOutcome.Xctl) out).program());
        }

        @Test
        void optionNumberMatchesGnuCobolGoldenTable() throws Exception {
            try (var in = getClass().getResourceAsStream("/option-normalization-gnucobol.csv");
                 var reader = new BufferedReader(new InputStreamReader(in, StandardCharsets.UTF_8))) {
                int rows = 0;
                for (String line : reader.lines().toList()) {
                    if (line.startsWith("#")) {
                        continue;
                    }
                    String[] cols = line.split("\\|");
                    String optionI = cols[0];
                    String optionO = cols[2];
                    int n = Integer.parseInt(optionO);
                    ProgramOutcome out = enter(optionI, "COPAUS0C");
                    if (n >= 1 && n <= 11) {
                        assertInstanceOf(ProgramOutcome.Xctl.class, out, optionI);
                        assertEquals(MainMenuOptions.standard().option(n).pgmName().strip(),
                                ((ProgramOutcome.Xctl) out).program());
                    } else {
                        assertEquals(optionO, screen.lastSent().optionO(), "OPTIONO for [" + optionI + "]");
                        assertEquals("Please enter a valid option number...", screen.lastSent().message());
                    }
                    rows++;
                }
                assertEquals(13, rows);
            }
        }
    }

    @Nested
    class SendMenuScreen {

        @Test
        void headerCarriesDateAndTimeFromCurrentDate() {
            run(new CicsRequest(commarea('U', 0).toBytes(), Dfhaid.DFHENTER), MenuScreenRequest.notTransmitted());
            assertEquals("11/26/26", screen.lastSent().curdateO());
            assertEquals("07:05:09", screen.lastSent().curtimeO());
        }

        @Test
        void buildMenuOptionsFormatsElevenLinesAndLeavesTwelfthUntouched() {
            run(new CicsRequest(commarea('U', 0).toBytes(), Dfhaid.DFHENTER), MenuScreenRequest.notTransmitted());
            MenuScreenResponse s = screen.lastSent();
            assertEquals(CobolText.move("01. Account View", 40), s.optnO(1));
            assertEquals(CobolText.move("10. Bill Payment", 40), s.optnO(10));
            assertEquals(CobolText.move("11. Pending Authorization View", 40), s.optnO(11));
            assertEquals(CobolText.lowValues(40), s.optnO(12));
        }

        @Test
        void menuLineTruncatesAtFortyBytes() {
            MainMenuOptions menu = new MainMenuOptions(1, List.of(
                    new MenuOption(1, "X".repeat(35), "COACTVWC", "U")));
            run(new CicsRequest(commarea('U', 0).toBytes(), Dfhaid.DFHENTER), MenuScreenRequest.notTransmitted(), menu);
            assertEquals("01. " + "X".repeat(35) + " ", screen.lastSent().optnO(1));
            assertEquals(CobolText.lowValues(40), screen.lastSent().optnO(2));
        }

        @Test
        void errorMessageIsTruncatedTo78Bytes() {
            String longName = "N".repeat(35);
            MainMenuOptions menu = new MainMenuOptions(1, List.of(
                    new MenuOption(1, longName, "COPAUS0C", "U")));
            returned(run(reenter(Dfhaid.DFHENTER), MenuScreenRequest.raw("01"), menu));
            String expected = "This option " + longName + " is not installed...";
            assertEquals(CobolText.move(expected, 78), screen.lastSent().errmsgO());
            assertFalse(expected.length() > 78);
        }
    }

    @Test
    void shortCommareaIsPaddedWithSpaces() {
        CardDemoCommarea c = returned(run(new CicsRequest("CC00COSGN00C", Dfhaid.DFHENTER), MenuScreenRequest.notTransmitted()));
        assertEquals("CC00", c.fromTranid());
        assertEquals("COSGN00C", c.fromProgram());
        assertEquals("    ", c.toTranid());
        assertTrue(c.isPgmReenter());
    }
}
