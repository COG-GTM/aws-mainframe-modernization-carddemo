package com.carddemo.programs.cosgn00c;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.programs.cosgn00c.copybook.CardDemoCommarea;
import com.carddemo.programs.cosgn00c.copybook.CicsAid;
import com.carddemo.programs.cosgn00c.copybook.CobolText;
import com.carddemo.programs.cosgn00c.copybook.Cosgn0aField;
import com.carddemo.programs.cosgn00c.copybook.Cosgn0aInput;
import com.carddemo.programs.cosgn00c.copybook.SecUserData;

import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.EnumSource;
import org.junit.jupiter.params.provider.ValueSource;

class SignonServiceTest {

    private static final Clock CLOCK = Clock.fixed(Instant.parse("2026-11-26T09:05:07.25Z"), ZoneOffset.UTC);
    private static final CicsSystemInfo REGION = CicsSystemInfo.of("CARDDEMO", "CICS");

    private RecordingRepository repo;
    private SignonService service;

    @BeforeEach
    void setUp() {
        InMemoryUserSecurityRepository store = new InMemoryUserSecurityRepository()
                .put(SecUserData.of("ADMIN001", "MARGARET", "GOLD", "PASSWORD", "A"))
                .put(SecUserData.of("USER0001", "LAWRENCE", "THOMAS", "PASSWORD", "U"))
                .put(SecUserData.of("ODDTYPE1", "ODD", "TYPE", "SECRET12", "X"))
                .put(SecUserData.of("LOWERPW1", "LOWER", "CASE", "lowerpw1", "U"));
        repo = new RecordingRepository(store);
        service = new SignonService(repo, REGION, CLOCK);
    }

    private static String msg(String text) {
        return CobolText.picX(text, 78);
    }

    private SignonOutcome.ScreenSent screen(SignonRequest request) {
        return assertInstanceOf(SignonOutcome.ScreenSent.class, service.run(request));
    }

    @Nested
    class MainPara {

        @Test
        void firstEntrySendsBlankSignonScreenWithCursorOnUserId() {
            SignonOutcome.ScreenSent s = screen(SignonRequest.firstEntry());

            assertEquals(msg(""), s.map().errmsgO());
            assertEquals(Optional.of(Cosgn0aField.USERID), s.map().cursorField());
            assertFalse(s.errorFlag());
            assertEquals("CC00", s.returnTransId());
            assertEquals(CobolText.lowValues(8), s.map().useridO());
            assertEquals(CobolText.lowValues(8), s.map().passwdO());
            assertTrue(repo.keys.isEmpty());
        }

        @Test
        void pf3SendsThankYouPlainTextOf80BytesAndEndsConversation() {
            SignonOutcome out = service.run(SignonRequest.key(CicsAid.DFHPF3));

            SignonOutcome.TextSent text = assertInstanceOf(SignonOutcome.TextSent.class, out);
            assertEquals(80, text.text().length());
            assertEquals("Thank you for using CardDemo application...", text.text().stripTrailing());
        }

        @ParameterizedTest
        @EnumSource(value = CicsAid.class, names = {"DFHENTER", "DFHPF3"}, mode = EnumSource.Mode.EXCLUDE)
        void anyOtherKeyShowsInvalidKeyMessage(CicsAid aid) {
            SignonOutcome.ScreenSent s = screen(SignonRequest.key(aid));

            assertTrue(s.errorFlag());
            assertEquals(msg("Invalid key pressed. Please see below..."), s.map().errmsgO());
            assertEquals(Optional.empty(), s.map().cursorField());
            assertEquals("CC00", s.returnTransId());
        }

        @Test
        void eibcalenZeroIgnoresAidKey() {
            SignonOutcome.ScreenSent s = screen(new SignonRequest(0, CicsAid.DFHPF3, Cosgn0aInput.of(null, null)));
            assertEquals(msg(""), s.map().errmsgO());
        }
    }

    @Nested
    class PopulateHeaderInfo {

        @Test
        void headerCarriesTitlesTranProgramDateTimeAndRegion() {
            SignonOutcome.ScreenSent s = screen(SignonRequest.firstEntry());

            assertEquals("      AWS Mainframe Modernization       ", s.map().title01O());
            assertEquals("              CardDemo                  ", s.map().title02O());
            assertEquals("CC00", s.map().trnnameO());
            assertEquals("COSGN00C", s.map().pgmnameO());
            assertEquals("11/26/26", s.map().curdateO());
            assertEquals("09:05:07 ", s.map().curtimeO());
            assertEquals("CARDDEMO", s.map().applidO());
            assertEquals("CICS    ", s.map().sysidO());
        }

        @Test
        void yearUsesLastTwoDigitsOfFourDigitYear() {
            Clock y2k = Clock.fixed(Instant.parse("2000-01-02T23:59:58Z"), ZoneOffset.UTC);
            SignonOutcome.ScreenSent s = assertInstanceOf(SignonOutcome.ScreenSent.class,
                    new SignonService(repo, REGION, y2k).run(SignonRequest.firstEntry()));
            assertEquals("01/02/00", s.map().curdateO());
            assertEquals("23:59:58 ", s.map().curtimeO());
        }
    }

    @Nested
    class ProcessEnterKey {

        @ParameterizedTest
        @ValueSource(strings = {"", " ", "        "})
        void blankUserIdIsRejected(String userId) {
            SignonOutcome.ScreenSent s = screen(SignonRequest.enter(userId, "PASSWORD"));

            assertTrue(s.errorFlag());
            assertEquals(msg("Please enter User ID ..."), s.map().errmsgO());
            assertEquals(Optional.of(Cosgn0aField.USERID), s.map().cursorField());
            assertTrue(repo.keys.isEmpty());
        }

        @Test
        void userIdNotTransmittedIsLowValuesAndRejected() {
            SignonOutcome.ScreenSent s = screen(SignonRequest.enter(null, null));
            assertEquals(msg("Please enter User ID ..."), s.map().errmsgO());
        }

        @Test
        void userIdCheckedBeforePassword() {
            SignonOutcome.ScreenSent s = screen(SignonRequest.enter("  ", "  "));
            assertEquals(msg("Please enter User ID ..."), s.map().errmsgO());
        }

        @ParameterizedTest
        @ValueSource(strings = {"", "        "})
        void blankPasswordIsRejected(String password) {
            SignonOutcome.ScreenSent s = screen(SignonRequest.enter("admin001", password));

            assertTrue(s.errorFlag());
            assertEquals(msg("Please enter Password ..."), s.map().errmsgO());
            assertEquals(Optional.of(Cosgn0aField.PASSWD), s.map().cursorField());
            assertTrue(repo.keys.isEmpty());
        }

        @Test
        void passwordNotTransmittedIsRejected() {
            SignonOutcome.ScreenSent s = screen(SignonRequest.enter("ADMIN001", null));
            assertEquals(msg("Please enter Password ..."), s.map().errmsgO());
        }

        @Test
        void userIdIsUpperCasedIntoCommareaEvenWhenValidationFails() {
            SignonOutcome.ScreenSent s = screen(SignonRequest.enter("admin001", ""));
            assertEquals("ADMIN001", s.commarea().userId());
            assertEquals("    ", s.commarea().fromTranid());
        }

        @Test
        void blankPasswordRedisplayKeepsTypedUserIdAndUntransmittedPasswordAsLowValues() {
            SignonOutcome.ScreenSent s = screen(SignonRequest.enter("admin001", null));
            assertEquals("admin001", s.map().useridO());
            assertEquals(CobolText.lowValues(8), s.map().passwdO());
        }

        @Test
        void mixedSpacesAndLowValuesIsNotTreatedAsBlank() {
            Cosgn0aInput in = new Cosgn0aInput(null, null, null, null, null, null, null, null,
                    "    \u0000\u0000\u0000\u0000", "PASSWORD", null);
            SignonOutcome.ScreenSent s = screen(new SignonRequest(1, CicsAid.DFHENTER, in));
            assertEquals(msg("User not found. Try again ..."), s.map().errmsgO());
        }

        @Test
        void userIdAndPasswordAreUpperCasedAndPaddedToEightBytesForTheRead() {
            SignonOutcome out = service.run(SignonRequest.enter("user0001", "password"));

            assertEquals(List.of("USER0001"), repo.keys);
            assertInstanceOf(SignonOutcome.ControlTransferred.class, out);
        }

        @Test
        void inputLongerThanFieldIsTruncatedToEightBytes() {
            SignonOutcome out = service.run(SignonRequest.enter("ADMIN001XYZ", "PASSWORDXYZ"));
            assertEquals(List.of("ADMIN001"), repo.keys);
            assertInstanceOf(SignonOutcome.ControlTransferred.class, out);
        }

        @Test
        void shortUserIdIsSpacePadded() {
            service.run(SignonRequest.enter("ab", "x"));
            assertEquals(List.of("AB      "), repo.keys);
        }
    }

    @Nested
    class ReadUserSecFile {

        @Test
        void adminSignonTransfersToAdminMenuWithCommarea() {
            SignonOutcome out = service.run(SignonRequest.enter("ADMIN001", "PASSWORD"));

            SignonOutcome.ControlTransferred x = assertInstanceOf(SignonOutcome.ControlTransferred.class, out);
            assertEquals("COADM01C", x.program());
            CardDemoCommarea c = x.commarea();
            assertEquals("CC00", c.fromTranid());
            assertEquals("COSGN00C", c.fromProgram());
            assertEquals("ADMIN001", c.userId());
            assertEquals("A", c.userType());
            assertTrue(c.isUserTypeAdmin());
            assertEquals(0, c.pgmContext());
            assertTrue(c.isPgmEnter());
        }

        @Test
        void regularUserSignonTransfersToMainMenu() {
            SignonOutcome out = service.run(SignonRequest.enter("USER0001", "PASSWORD"));

            SignonOutcome.ControlTransferred x = assertInstanceOf(SignonOutcome.ControlTransferred.class, out);
            assertEquals("COMEN01C", x.program());
            assertTrue(x.commarea().isUserTypeUser());
        }

        @Test
        void anyNonAdminUserTypeGoesToMainMenu() {
            SignonOutcome out = service.run(SignonRequest.enter("ODDTYPE1", "SECRET12"));

            SignonOutcome.ControlTransferred x = assertInstanceOf(SignonOutcome.ControlTransferred.class, out);
            assertEquals("COMEN01C", x.program());
            assertEquals("X", x.commarea().userType());
        }

        @Test
        void passwordInputIsCaseInsensitive() {
            SignonOutcome out = service.run(SignonRequest.enter("ADMIN001", "PaSsWoRd"));
            assertInstanceOf(SignonOutcome.ControlTransferred.class, out);
        }

        @Test
        void storedLowerCasePasswordCanNeverMatchUpperCasedInput() {
            SignonOutcome.ScreenSent s = screen(SignonRequest.enter("LOWERPW1", "lowerpw1"));
            assertEquals(msg("Wrong Password. Try again ..."), s.map().errmsgO());
        }

        @Test
        void wrongPasswordRedisplaysWithCursorOnPasswordWithoutSettingErrorFlag() {
            SignonOutcome.ScreenSent s = screen(SignonRequest.enter("ADMIN001", "WRONGPWD"));

            assertEquals(msg("Wrong Password. Try again ..."), s.map().errmsgO());
            assertEquals(Optional.of(Cosgn0aField.PASSWD), s.map().cursorField());
            assertFalse(s.errorFlag());
            assertEquals("ADMIN001", s.commarea().userId());
            assertEquals("    ", s.commarea().fromTranid());
            assertEquals("CC00", s.returnTransId());
        }

        @Test
        void wrongPasswordEchoesReceivedFieldsAsTypedBecauseOutputMapRedefinesInput() {
            SignonOutcome.ScreenSent s = screen(SignonRequest.enter("admin001", "wrongpwd"));

            assertEquals("admin001", s.map().useridO());
            assertEquals("wrongpwd", s.map().passwdO());
            assertEquals("COSGN00C", s.map().pgmnameO());
        }

        @Test
        void unknownUserGivesNotFoundMessage() {
            SignonOutcome.ScreenSent s = screen(SignonRequest.enter("NOBODY01", "PASSWORD"));

            assertTrue(s.errorFlag());
            assertEquals(msg("User not found. Try again ..."), s.map().errmsgO());
            assertEquals(Optional.of(Cosgn0aField.USERID), s.map().cursorField());
        }

        @ParameterizedTest
        @ValueSource(ints = {CicsResp.FILENOTFOUND, CicsResp.IOERR, CicsResp.NOTOPEN, CicsResp.DISABLED})
        void anyOtherRespGivesUnableToVerify(int resp) {
            SignonService broken = new SignonService(
                    userId -> new UserSecurityRepository.ReadResult(resp, 0, null), REGION, CLOCK);

            SignonOutcome.ScreenSent s = assertInstanceOf(SignonOutcome.ScreenSent.class,
                    broken.run(SignonRequest.enter("ADMIN001", "PASSWORD")));
            assertTrue(s.errorFlag());
            assertEquals(msg("Unable to verify the User ..."), s.map().errmsgO());
            assertEquals(Optional.of(Cosgn0aField.USERID), s.map().cursorField());
        }
    }

    /** Records every RIDFLD passed to the USRSEC read. */
    private static final class RecordingRepository implements UserSecurityRepository {
        private final UserSecurityRepository delegate;
        private final List<String> keys = new ArrayList<>();

        RecordingRepository(UserSecurityRepository delegate) {
            this.delegate = delegate;
        }

        @Override
        public ReadResult read(String userId) {
            keys.add(userId);
            return delegate.read(userId);
        }
    }
}
