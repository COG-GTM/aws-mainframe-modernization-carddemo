package com.carddemo.common.date;

import com.carddemo.common.codec.TestData;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.MethodSource;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;

/** Replays every case of the GnuCOBOL baseline run of CSUTLDTC (docs/validation/baseline/CSUTLDTC/sysout.txt). */
class CsutldtcBaselineTest {

    private static final Pattern CASE = Pattern.compile(
            "CASE (\\d\\d) IN=\\[(.{10})] MASK=\\[(.{10})] RC=(\\d{4})\\n {8}RESULT=\\[(.*?)]\\n", Pattern.DOTALL);

    static Stream<Arguments> baselineCases() throws IOException {
        String sysout = Files.readString(TestData.resolve("docs/validation/baseline/CSUTLDTC/sysout.txt"),
                StandardCharsets.ISO_8859_1);
        List<Arguments> cases = new ArrayList<>();
        Matcher m = CASE.matcher(sysout);
        while (m.find()) {
            cases.add(Arguments.of(m.group(1), m.group(2), m.group(3), Integer.parseInt(m.group(4)),
                    m.group(5).replace("\\x00", "\u0000")));
        }
        return cases.stream();
    }

    @Test
    void baselineHasTwelveCases() throws IOException {
        assertThat(baselineCases()).hasSize(12);
    }

    @ParameterizedTest(name = "case {0}: {1} / {2}")
    @MethodSource("baselineCases")
    void reproducesBaselineResultAndReturnCode(String id, String date, String mask, int rc, String result) {
        Csutldtc.Result r = Csutldtc.validate(date, mask);
        assertThat(r.message()).hasSize(Csutldtc.RESULT_LENGTH).isEqualTo(result);
        assertThat(r.returnCode()).isEqualTo(rc);
        assertThat(r.isValid()).isEqualTo(rc == 0);
        assertThat(r.lilian() > 0).isEqualTo(rc == 0);
    }

    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {
            "2022-07-06|YYYY-MM-DD|VALID|160607",
            "07/06/2022|MM/DD/YYYY|VALID|160607",
            "20220706|YYYYMMDD|VALID|160607",
            "2024-02-29|YYYY-MM-DD|VALID|161210",
            "2023-02-29|YYYY-MM-DD|BAD_DATE_VALUE|0",
            "2022-13-01|YYYY-MM-DD|INVALID_MONTH|0",
            "2022-04-31|YYYY-MM-DD|BAD_DATE_VALUE|0",
            "2022-04-00|YYYY-MM-DD|BAD_DATE_VALUE|0",
            "20XX-01-01|YYYY-MM-DD|NON_NUMERIC_DATA|0",
            "1600-01-01|YYYY-MM-DD|BAD_DATE_VALUE|0",
            "2022-07-06|YYYY-MM|INSUFFICIENT_DATA|0"
    })
    void ceeDaysConditions(String date, String mask, CeeDays.Feedback feedback, int lilian) {
        CeeDays.Result r = CeeDays.days(date, mask);
        assertThat(r.feedback()).isEqualTo(feedback);
        assertThat(r.lilian()).isEqualTo(lilian);
        assertThat(r.isValid()).isEqualTo(feedback == CeeDays.Feedback.VALID);
    }

    @Test
    void shortDateIsSpacePaddedAndExtraMaskLettersIgnored() {
        assertThat(CeeDays.days("2022", "YYYYYYMMDD").feedback()).isEqualTo(CeeDays.Feedback.INSUFFICIENT_DATA);
        assertThat(CeeDays.days("2022070600", "YYYYMMDDDD").lilian()).isEqualTo(160607);
    }

    @Test
    void feedbackTokens() {
        assertThat(CeeDays.Feedback.VALID.token()).containsOnly(0);
        assertThat(CeeDays.Feedback.BAD_DATE_VALUE.token())
                .containsExactly(0x00, 0x03, 0x09, 0xCC, 0x59, 0xC3, 0xC5, 0xC5);
        assertThat(CeeDays.Feedback.YEAR_IN_ERA_ZERO.messageNumber()).isEqualTo(0x09D9);
        assertThat(CeeDays.Feedback.INVALID_ERA.resultText()).isEqualTo("Invalid Era");
        assertThat(CeeDays.Feedback.UNSUPPORTED_RANGE.severity()).isEqualTo(3);
        assertThat(CeeDays.Feedback.BAD_PICTURE_STRING.messageNumber()).isEqualTo(2518);
    }

    @Test
    void resultAccessors() {
        Csutldtc.Result r = Csutldtc.validate("2022-13-01", "YYYY-MM-DD");
        assertThat(r.severityCode()).isEqualTo("0003");
        assertThat(r.messageCode()).isEqualTo("2517");
        assertThat(Csutldtc.validate("2022-07-06 extra", "YYYY-MM-DD").isValid()).isTrue();
    }
}
