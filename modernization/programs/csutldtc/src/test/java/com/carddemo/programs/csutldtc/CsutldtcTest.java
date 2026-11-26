package com.carddemo.programs.csutldtc;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import com.carddemo.programs.csutldtc.copybook.CsutldtcParm;
import com.carddemo.programs.csutldtc.copybook.FeedbackCode;
import com.carddemo.programs.csutldtc.copybook.VString;
import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class CsutldtcTest {
    private static final String FMT = "YYYY-MM-DD";
    private final Csutldtc program = new Csutldtc();

    private static Csutldtc stubbed(FeedbackCode fc) {
        return new Csutldtc((d, p) -> new CeedaysResult(0, fc));
    }

    @Test
    void validDateProducesExactResultRecordAndRcZero() {
        CsutldtcResult r = program.call("2022-06-10", FMT);
        assertEquals("0000Mesg Code: 0000 Date is valid   TstDate: \u0000\n2022-06- Mask used:YYYY-MM-DD    ", r.lsResult());
        assertEquals(80, r.lsResult().length());
        assertEquals(0, r.returnCode());
        assertEquals(160_581, r.outputLillian());
    }

    @ParameterizedTest(name = "{0} {1} -> {2} {3}")
    @CsvSource(delimiter = '|', ignoreLeadingAndTrailingWhitespace = false, value = {
        "2024-02-29|YYYY-MM-DD|0000|Date is valid  ",
        "2023-02-29|YYYY-MM-DD|2508|Datevalue error",
        "2023-04-31|YYYY-MM-DD|2508|Datevalue error",
        "2023-01-00|YYYY-MM-DD|2508|Datevalue error",
        "2023-13-01|YYYY-MM-DD|2517|Invalid month  ",
        "2023-00-10|YYYY-MM-DD|2517|Invalid month  ",
        "1582-10-14|YYYY-MM-DD|2513|Unsupp. Range  ",
        "0000-01-01|YYYY-MM-DD|2513|Unsupp. Range  ",
        "ABCD-EF-GH|YYYY-MM-DD|2520|Nonnumeric data",
        "2023/01/01|YYYY-MM-DD|2520|Nonnumeric data",
        "2023-1-1  |YYYY-MM-DD|2507|Insufficient   ",
        "2023-01-01|XXXX-XX-XX|2518|Bad Pic String ",
        "2023-01-01|YYYY-MM   |2518|Bad Pic String ",
    })
    void a000MainEvaluateBranchesWithLeEmulation(String date, String fmt, String msgNum, String text) {
        CsutldtcResult r = program.call(date, fmt);
        assertEquals(msgNum, r.msgNum());
        assertEquals(text, r.resultText());
        assertEquals("0000".equals(msgNum) ? 0 : 3, r.returnCode());
        assertEquals("0000".equals(msgNum) ? "0000" : "0003", r.severityCode());
    }

    @Test
    void blankDateIsInsufficientData() {
        CsutldtcResult r = program.call("          ", FMT);
        assertEquals("Insufficient   ", r.resultText());
        assertEquals("2507", r.msgNum());
        assertEquals(3, r.returnCode());
    }

    @Test
    void eraBranchesOnlyReachableFromGateway() {
        assertEquals("Invalid Era    ", stubbed(FeedbackCode.ofCondition(3, 2509)).call("2023-01-01", FMT).resultText());
        assertEquals("YearInEra is 0 ", stubbed(FeedbackCode.ofCondition(3, 2521)).call("2023-01-01", FMT).resultText());
    }

    @Test
    void whenOtherIsDateIsInvalid() {
        CsutldtcResult unknown = stubbed(FeedbackCode.ofCondition(3, 2510)).call("2023-01-01", FMT);
        assertEquals("Date is invalid", unknown.resultText());
        assertEquals("2510", unknown.msgNum());
        assertEquals(3, unknown.returnCode());
        // severity differs from the 88-level token -> OTHER, even with a known MSG-NO
        CsutldtcResult sev2 = stubbed(FeedbackCode.ofCondition(2, 2508)).call("2023-01-01", FMT);
        assertEquals("Date is invalid", sev2.resultText());
        assertEquals(2, sev2.returnCode());
    }

    @Test
    void exactValidationMessageTexts() {
        assertEquals(List.of("Date is valid  ", "Insufficient   ", "Datevalue error", "Invalid Era    ",
                "Unsupp. Range  ", "Invalid month  ", "Bad Pic String ", "Nonnumeric data", "YearInEra is 0 ",
                "Date is invalid"), resultTexts(0, 2507, 2508, 2509, 2513, 2517, 2518, 2520, 2521, 9999));
    }

    private static List<String> resultTexts(int... msgNos) {
        List<String> out = new ArrayList<>();
        for (int m : msgNos) {
            FeedbackCode fc = m == 0 ? FeedbackCode.success() : FeedbackCode.ofCondition(3, m);
            out.add(stubbed(fc).call("2023-01-01", FMT).resultText());
        }
        return out;
    }

    @Test
    void negativeSeverityLosesSignInWsSeverityN() {
        CsutldtcResult r = stubbed(FeedbackCode.ofCondition(-2, -2508)).call("2023-01-01", FMT);
        assertEquals("0002", r.severityCode());
        assertEquals("2508", r.msgNum());
        assertEquals(2, r.returnCode());
    }

    @Test
    void severityAbove9999IsTruncatedToFourDigits() {
        CsutldtcResult r = stubbed(FeedbackCode.ofCondition(12345, 2508)).call("2023-01-01", FMT);
        assertEquals("2345", r.severityCode());
        assertEquals(2345, r.returnCode());
    }

    @Test
    void wsDateHoldsGroupMoveOfVstringNotTheDate() {
        String r = program.call("2023-12-25", FMT).lsResult();
        assertEquals("TstDate: \u0000\n2023-12-", r.substring(36, 55));
        assertEquals("Mask used:YYYY-MM-DD", r.substring(56, 76));
    }

    @Test
    void ceedaysReceivesTenByteVstringsWithTrailingSpaces() {
        VString[] seen = new VString[2];
        new Csutldtc((d, p) -> {
            seen[0] = d;
            seen[1] = p;
            return new CeedaysResult(0, FeedbackCode.success());
        }).call("20230101", "YYYYMMDD");
        assertEquals(new VString((short) 10, "20230101  "), seen[0]);
        assertEquals(new VString((short) 10, "YYYYMMDD  "), seen[1]);
        assertArrayEquals(new byte[] {0, 10, '2', '0', '2', '3', '0', '1', '0', '1', ' ', ' '}, seen[0].groupBytes());
    }

    @Test
    void linkageFieldsAreTruncatedAndPaddedToPicX10() {
        CsutldtcResult r = program.call("2023-01-01T10:00", "YYYY-MM-DDXX");
        assertEquals("Date is valid  ", r.resultText());
        assertEquals("YYYY-MM-DD", r.lsResult().substring(66, 76));
        CsutldtcResult shortFmt = program.call("230101", "YYMMDD");
        assertEquals("YYMMDD    ", shortFmt.lsResult().substring(66, 76));
    }

    @Test
    void callersAcceptValidAndUnsupportedRangeButRejectOthers() {
        assertTrue(program.call("2023-01-01", FMT).toParm("2023-01-01", FMT).acceptedByCallers());
        CsutldtcParm range = program.call("1500-01-01", FMT).toParm("1500-01-01", FMT);
        assertEquals("2513", range.resultMsgNum());
        assertTrue(range.acceptedByCallers());
        assertFalse(program.call("2023-02-30", FMT).toParm("2023-02-30", FMT).acceptedByCallers());
        assertFalse(program.call("2023-13-01", FMT).toParm("2023-13-01", FMT).acceptedByCallers());
    }
}
