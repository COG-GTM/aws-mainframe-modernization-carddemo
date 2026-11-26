package com.carddemo.programs.corpt00c;

import com.carddemo.programs.corpt00c.Corpt00cResponse.Action;
import com.carddemo.programs.corpt00c.copybook.CarddemoCommarea;
import com.carddemo.programs.corpt00c.copybook.CommonMessages;
import com.carddemo.programs.corpt00c.copybook.Corpt0aInput;
import com.carddemo.programs.corpt00c.copybook.Corpt0aOutput;
import com.carddemo.programs.corpt00c.copybook.Dfhaid;
import com.carddemo.programs.corpt00c.copybook.Dfhbmsca;
import com.carddemo.programs.corpt00c.copybook.ScreenTitle;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.time.Clock;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class Corpt00cTest {

    private InMemoryTransientDataQueue tdq = new InMemoryTransientDataQueue();
    private final List<String> displayed = new ArrayList<>();

    private Corpt00c program(LocalDateTime now) {
        Clock clock = Clock.fixed(now.toInstant(ZoneOffset.UTC), ZoneOffset.UTC);
        return new Corpt00c(clock, tdq, new Csutldtc(), displayed::add);
    }

    private Corpt00c program() {
        return program(LocalDateTime.of(2024, 2, 15, 9, 5, 7));
    }

    private static CarddemoCommarea reenterCommarea() {
        CarddemoCommarea c = new CarddemoCommarea();
        c.setFromProgram("COMEN01C");
        c.setUserId("ADMIN001");
        c.setUserType("A");
        c.setPgmReenter();
        return c;
    }

    private Corpt00cResponse enter(Corpt0aInput in) {
        return program().execute(new Corpt00cRequest(reenterCommarea(), Dfhaid.DFHENTER, in));
    }

    private static String msg(String text) {
        return CobolText.move(text, 78);
    }

    private static void assertError(Corpt00cResponse r, String message, String cursor) {
        assertEquals(Action.SEND_MAP, r.action());
        assertEquals(msg(message), r.screen().errmsg());
        assertEquals(cursor, r.screen().cursorField());
        assertNotEquals(Dfhbmsca.DFHGREEN, r.screen().errmsgColor());
    }

    // MAIN-PARA: EIBCALEN = 0 -> RETURN-TO-PREV-SCREEN to COSGN00C
    @Test
    void noCommareaTransfersToSignon() {
        Corpt00cResponse r = program().execute(Corpt00cRequest.noCommarea());
        assertEquals(Action.XCTL, r.action());
        assertEquals("COSGN00C", r.program());
        assertEquals("CR00", r.commarea().fromTranid());
        assertEquals("CORPT00C", r.commarea().fromProgram());
        assertTrue(r.commarea().isPgmEnter());
        assertTrue(tdq.records("JOBS").isEmpty());
    }

    // MAIN-PARA: first entry -> LOW-VALUES map, cursor MONTHLY, SEND ERASE, RETURN TRANSID(CR00)
    @Test
    void firstEntrySendsBlankScreenWithHeader() {
        CarddemoCommarea c = new CarddemoCommarea();
        c.setFromProgram("COMEN01C");
        Corpt00cResponse r = program().execute(new Corpt00cRequest(c, Dfhaid.DFHENTER, Corpt0aInput.monthly("Y")));
        assertEquals(Action.SEND_MAP, r.action());
        assertEquals("CR00", r.transId());
        assertTrue(r.erase());
        assertTrue(r.commarea().isPgmReenter());
        Corpt0aOutput s = r.screen();
        assertEquals("MONTHLY", s.cursorField());
        assertEquals(CobolText.spaces(78), s.errmsg());
        assertEquals(CobolText.lowValues(1), s.monthly());
        assertEquals(CobolText.lowValues(4), s.sdtyyyy());
        assertEquals(ScreenTitle.CCDA_TITLE01, s.title01());
        assertEquals(ScreenTitle.CCDA_TITLE02, s.title02());
        assertEquals("CR00", s.trnname());
        assertEquals("CORPT00C", s.pgmname());
        assertEquals("02/15/24", s.curdate());
        assertEquals("09:05:07", s.curtime());
        assertTrue(tdq.records("JOBS").isEmpty(), "first entry must not process input");
    }

    // MAIN-PARA: PF3 -> COMEN01C
    @Test
    void pf3ReturnsToMainMenu() {
        Corpt00cResponse r = program().execute(new Corpt00cRequest(reenterCommarea(), Dfhaid.DFHPF3,
                Corpt0aInput.empty()));
        assertEquals(Action.XCTL, r.action());
        assertEquals("COMEN01C", r.program());
        assertEquals("CORPT00C", r.commarea().fromProgram());
        assertEquals("CR00", r.commarea().fromTranid());
        assertTrue(r.commarea().isPgmEnter());
        assertEquals("ADMIN001", r.commarea().userId());
    }

    // MAIN-PARA: WHEN OTHER -> invalid key
    @Test
    void otherKeyShowsInvalidKeyMessageAndEchoesInput() {
        Corpt00cResponse r = program().execute(new Corpt00cRequest(reenterCommarea(), Dfhaid.DFHPF5,
                Corpt0aInput.yearly("Y")));
        assertError(r, CommonMessages.CCDA_MSG_INVALID_KEY, "MONTHLY");
        assertEquals("Invalid key pressed. Please see below...", r.screen().errmsg().strip());
        assertEquals("X", r.screen().yearly());
        assertEquals("Y", r.screen().confirm());
        assertTrue(tdq.records("JOBS").isEmpty());
    }

    // PROCESS-ENTER-KEY: WHEN OTHER
    @Test
    void enterWithNoSelectionAsksForReportType() {
        Corpt00cResponse r = enter(Corpt0aInput.empty());
        assertError(r, "Select a report type to print report...", "MONTHLY");
        assertEquals(List.of("PROCESS ENTER KEY"), displayed);
    }

    @Test
    void spacesAreNotASelection() {
        Corpt00cResponse r = enter(new Corpt0aInput(" ", " ", " ", null, null, null, null, null, null, "Y"));
        assertError(r, "Select a report type to print report...", "MONTHLY");
    }

    // SUBMIT-JOB-TO-INTRDR: CONFIRMI = SPACES OR LOW-VALUES
    @Test
    void monthlyWithoutConfirmAsksForConfirmation() {
        Corpt00cResponse r = enter(Corpt0aInput.monthly(null));
        assertError(r, "Please confirm to print the Monthly report...", "CONFIRM");
        assertEquals("X", r.screen().monthly());
        assertTrue(tdq.records("JOBS").isEmpty());
    }

    @Test
    void yearlyWithBlankConfirmAsksForConfirmation() {
        Corpt00cResponse r = enter(Corpt0aInput.yearly(" "));
        assertError(r, "Please confirm to print the Yearly report...", "CONFIRM");
    }

    // PROCESS-ENTER-KEY monthly branch + SUBMIT-JOB-TO-INTRDR + WIRTE-JOBSUB-TDQ
    @Test
    void monthlyConfirmedSubmitsJobForCurrentMonth() {
        Corpt00cResponse r = enter(Corpt0aInput.monthly("Y"));
        List<String> jcl = tdq.records("JOBS");
        assertEquals(17, jcl.size());
        jcl.forEach(l -> assertEquals(80, l.length()));
        assertEquals(CobolText.move("//TRNRPT00 JOB 'TRAN REPORT',CLASS=A,MSGCLASS=0,", 80), jcl.get(0));
        assertEquals(CobolText.move("//STEP10 EXEC PROC=TRANREPT", 80), jcl.get(5));
        assertEquals(CobolText.move("PARM-START-DATE,C'2024-02-01'", 80), jcl.get(10));
        assertEquals(CobolText.move("PARM-END-DATE,C'2024-02-29'", 80), jcl.get(11));
        assertEquals(CobolText.move("2024-02-01 2024-02-29", 80), jcl.get(14));
        assertEquals(CobolText.move("/*EOF", 80), jcl.get(16));

        assertEquals(Action.SEND_MAP, r.action());
        assertEquals(msg("Monthly report submitted for printing ..."), r.screen().errmsg());
        assertEquals(Dfhbmsca.DFHGREEN, r.screen().errmsgColor());
        assertEquals("MONTHLY", r.screen().cursorField());
        assertEquals(" ", r.screen().monthly());
        assertEquals(" ", r.screen().confirm());
        assertEquals("    ", r.screen().sdtyyyy());
    }

    @Test
    void monthlyInDecemberRollsTheYearForEndOfMonth() {
        program(LocalDateTime.of(2023, 12, 31, 23, 59, 59))
                .execute(new Corpt00cRequest(reenterCommarea(), Dfhaid.DFHENTER, Corpt0aInput.monthly("y")));
        List<String> jcl = tdq.records("JOBS");
        assertEquals(CobolText.move("2023-12-01 2023-12-31", 80), jcl.get(14));
    }

    @Test
    void monthlyInNonLeapFebruary() {
        program(LocalDateTime.of(2023, 2, 1, 0, 0))
                .execute(new Corpt00cRequest(reenterCommarea(), Dfhaid.DFHENTER, Corpt0aInput.monthly("Y")));
        assertEquals(CobolText.move("2023-02-01 2023-02-28", 80), tdq.records("JOBS").get(14));
    }

    // PROCESS-ENTER-KEY yearly branch
    @Test
    void yearlyConfirmedSubmitsJobForCurrentYear() {
        Corpt00cResponse r = enter(Corpt0aInput.yearly("y"));
        List<String> jcl = tdq.records("JOBS");
        assertEquals(CobolText.move("PARM-START-DATE,C'2024-01-01'", 80), jcl.get(10));
        assertEquals(CobolText.move("PARM-END-DATE,C'2024-12-31'", 80), jcl.get(11));
        assertEquals(CobolText.move("2024-01-01 2024-12-31", 80), jcl.get(14));
        assertEquals(msg("Yearly report submitted for printing ..."), r.screen().errmsg());
        assertEquals(Dfhbmsca.DFHGREEN, r.screen().errmsgColor());
    }

    // EVALUATE TRUE order: MONTHLY wins over YEARLY / CUSTOM
    @Test
    void monthlyTakesPrecedenceOverOtherSelections() {
        Corpt00cResponse r = enter(new Corpt0aInput("X", "X", "X", null, null, null, null, null, null, "Y"));
        assertEquals(msg("Monthly report submitted for printing ..."), r.screen().errmsg());
    }

    // SUBMIT-JOB-TO-INTRDR: CONFIRMI = 'N'
    @Test
    void confirmNoClearsScreenWithoutSubmitting() {
        Corpt00cResponse r = enter(Corpt0aInput.yearly("n"));
        assertTrue(tdq.records("JOBS").isEmpty());
        assertEquals(Action.SEND_MAP, r.action());
        assertEquals(CobolText.spaces(78), r.screen().errmsg());
        assertEquals("MONTHLY", r.screen().cursorField());
        assertEquals(" ", r.screen().yearly());
        assertEquals(" ", r.screen().confirm());
        assertNotEquals(Dfhbmsca.DFHGREEN, r.screen().errmsgColor());
    }

    // SUBMIT-JOB-TO-INTRDR: WHEN OTHER
    @Test
    void invalidConfirmValueIsRejected() {
        Corpt00cResponse r = enter(Corpt0aInput.monthly("X"));
        assertError(r, "\"X\" is not a valid value to confirm...", "CONFIRM");
        assertTrue(tdq.records("JOBS").isEmpty());
    }

    // WIRTE-JOBSUB-TDQ: RESP NOT NORMAL
    @Test
    void tdqWriteFailureStopsSubmissionAndReports() {
        tdq = new InMemoryTransientDataQueue().failFromWrite(3, new CicsResponse(CicsResponse.QIDERR, 0));
        Corpt00cResponse r = enter(Corpt0aInput.monthly("Y"));
        assertEquals(2, tdq.records("JOBS").size());
        assertError(r, "Unable to Write TDQ (JOBS)...", "MONTHLY");
        assertEquals(List.of("PROCESS ENTER KEY", "RESP:00000004DREAS:00000000{"), displayed);
    }

    // PROCESS-ENTER-KEY custom branch: empty field checks, in EVALUATE order
    @ParameterizedTest
    @CsvSource(delimiter = '|', nullValues = "NULL", value = {
            "NULL|15|2024|02|20|2024|Start Date - Month can NOT be empty...|SDTMM",
            "'  '|15|2024|02|20|2024|Start Date - Month can NOT be empty...|SDTMM",
            "01|NULL|2024|02|20|2024|Start Date - Day can NOT be empty...|SDTDD",
            "01|15|NULL|02|20|2024|Start Date - Year can NOT be empty...|SDTYYYY",
            "01|15|2024|NULL|20|2024|End Date - Month can NOT be empty...|EDTMM",
            "01|15|2024|02|NULL|2024|End Date - Day can NOT be empty...|EDTDD",
            "01|15|2024|02|20|NULL|End Date - Year can NOT be empty...|EDTYYYY",
            "NULL|NULL|NULL|NULL|NULL|NULL|Start Date - Month can NOT be empty...|SDTMM",
    })
    void customEmptyFieldValidation(String sm, String sd, String sy, String em, String ed, String ey,
                                    String message, String cursor) {
        Corpt00cResponse r = enter(Corpt0aInput.custom(sm, sd, sy, em, ed, ey, "Y"));
        assertError(r, message, cursor);
        assertTrue(tdq.records("JOBS").isEmpty());
    }

    // PROCESS-ENTER-KEY custom branch: range checks after NUMVAL-C normalisation
    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {
            "13|15|2024|02|20|2024|Start Date - Not a valid Month...|SDTMM",
            "01|32|2024|02|20|2024|Start Date - Not a valid Day...|SDTDD",
            "01|15|2024|13|20|2024|End Date - Not a valid Month...|EDTMM",
            "01|15|2024|02|32|2024|End Date - Not a valid Day...|EDTDD",
            "99|99|2024|99|99|2024|Start Date - Not a valid Month...|SDTMM",
    })
    void customRangeValidation(String sm, String sd, String sy, String em, String ed, String ey,
                               String message, String cursor) {
        Corpt00cResponse r = enter(Corpt0aInput.custom(sm, sd, sy, em, ed, ey, "Y"));
        assertError(r, message, cursor);
    }

    // CALL 'CSUTLDTC' result handling
    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {
            "02|30|2024|03|01|2024|Start Date - Not a valid date...|SDTMM",
            "00|10|2024|03|01|2024|Start Date - Not a valid date...|SDTMM",
            "01|00|2024|03|01|2024|Start Date - Not a valid date...|SDTMM",
            "01|01|2023|02|29|2023|End Date - Not a valid date...|EDTMM",
            "01|01|2024|04|31|2024|End Date - Not a valid date...|EDTMM",
    })
    void customCalendarValidation(String sm, String sd, String sy, String em, String ed, String ey,
                                  String message, String cursor) {
        Corpt00cResponse r = enter(Corpt0aInput.custom(sm, sd, sy, em, ed, ey, "Y"));
        assertError(r, message, cursor);
        assertTrue(tdq.records("JOBS").isEmpty());
    }

    @Test
    void customValidRangeSubmitsJob() {
        Corpt00cResponse r = enter(Corpt0aInput.custom("01", "15", "2022", "07", "06", "2022", "Y"));
        List<String> jcl = tdq.records("JOBS");
        assertEquals(17, jcl.size());
        assertEquals(CobolText.move("PARM-START-DATE,C'2022-01-15'", 80), jcl.get(10));
        assertEquals(CobolText.move("PARM-END-DATE,C'2022-07-06'", 80), jcl.get(11));
        assertEquals(CobolText.move("2022-01-15 2022-07-06", 80), jcl.get(14));
        assertEquals(msg("Custom report submitted for printing ..."), r.screen().errmsg());
        assertEquals(Dfhbmsca.DFHGREEN, r.screen().errmsgColor());
        assertEquals("MONTHLY", r.screen().cursorField());
    }

    // NUMVAL-C normalisation: short / signed / non-numeric input becomes zero-padded digits
    @Test
    void customFieldsAreNormalisedAndEchoed() {
        Corpt00cResponse r = enter(Corpt0aInput.custom("1", "5 ", "2022", "-7", " 6", "2022", null));
        assertError(r, "Please confirm to print the Custom report...", "CONFIRM");
        assertEquals("01", r.screen().sdtmm());
        assertEquals("05", r.screen().sdtdd());
        assertEquals("07", r.screen().edtmm());
        assertEquals("06", r.screen().edtdd());
        assertEquals("2022", r.screen().sdtyyyy());
    }

    @Test
    void nonNumericMonthBecomesZeroAndFailsDateValidation() {
        Corpt00cResponse r = enter(Corpt0aInput.custom("ab", "15", "2022", "07", "06", "2022", "Y"));
        assertError(r, "Start Date - Not a valid date...", "SDTMM");
        assertEquals("00", r.screen().sdtmm());
    }

    // CSUTLDTC message 2513 (unsupported range) is deliberately ignored by CORPT00C
    @Test
    void unsupportedRangeYearsAreAccepted() {
        Corpt00cResponse r = enter(Corpt0aInput.custom("01", "01", "24", "12", "31", "1500", "Y"));
        assertEquals(msg("Custom report submitted for printing ..."), r.screen().errmsg());
        assertEquals(CobolText.move("0024-01-01 1500-12-31", 80), tdq.records("JOBS").get(14));
    }

    @Test
    void customInvalidConfirmEchoesValue() {
        Corpt00cResponse r = enter(Corpt0aInput.custom("01", "01", "2024", "01", "31", "2024", "q"));
        assertError(r, "\"q\" is not a valid value to confirm...", "CONFIRM");
    }

    @Test
    void commareaUserFieldsArePreservedOnSend() {
        Corpt00cResponse r = enter(Corpt0aInput.empty());
        assertEquals("ADMIN001", r.commarea().userId());
        assertTrue(r.commarea().isUsrtypAdmin());
        assertTrue(r.commarea().isPgmReenter());
        assertNull(r.program());
        assertFalse(r.screen() == null);
    }
}
