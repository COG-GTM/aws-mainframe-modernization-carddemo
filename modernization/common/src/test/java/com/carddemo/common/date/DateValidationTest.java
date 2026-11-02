package com.carddemo.common.date;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.common.date.DateValidation.Feedback;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.api.Test;

class DateValidationTest {

    private static final String PICTURE = "YYYY-MM-DD";

    @ParameterizedTest
    @CsvSource({
            "2022-07-18, VALID",
            "2024-02-29, VALID",
            "1582-10-15, VALID",
            "9999-12-31, VALID",
            "2023-02-29, BAD_DATE_VALUE",
            "2022-04-31, BAD_DATE_VALUE",
            "2022-01-00, BAD_DATE_VALUE",
            "2022-13-01, INVALID_MONTH",
            "2022-00-10, INVALID_MONTH",
            "20AB-07-18, NON_NUMERIC_DATA",
            "2022-07, INSUFFICIENT_DATA",
            "1582-10-14, UNSUPPORTED_RANGE",
            "0000-01-01, UNSUPPORTED_RANGE",
    })
    void feedbackForTheCallersPicture(String date, Feedback expected) {
        assertThat(DateValidation.validate(date, PICTURE).feedback()).isEqualTo(expected);
    }

    @Test
    void validDateMessageAndReturnCode() {
        DateValidation.Result result = DateValidation.validate("2022-07-18", PICTURE);
        assertThat(result.valid()).isTrue();
        assertThat(result.returnCode()).isZero();
        assertThat(result.severityCode()).isEqualTo("0000");
        assertThat(result.messageNumber()).isEqualTo("0000");
        assertThat(result.message()).hasSize(80)
                .isEqualTo("0000Mesg Code: 0000 Date is valid   TstDate: \u0000\n2022-07- Mask used:YYYY-MM-DD    ");
    }

    @Test
    void invalidDateCarriesSeverityThreeAndTheMessageNumberCorpt00cTests() {
        DateValidation.Result range = DateValidation.validate("1500-01-01", PICTURE);
        assertThat(range.returnCode()).isEqualTo(3);
        assertThat(range.severityCode()).isEqualTo("0003");
        assertThat(range.messageNumber()).isEqualTo("2513");
        assertThat(range.message().substring(20, 35)).isEqualTo("Unsupp. Range  ");

        DateValidation.Result month = DateValidation.validate("2022-13-01", PICTURE);
        assertThat(month.messageNumber()).isEqualTo("2517");
        assertThat(month.message().substring(20, 35)).isEqualTo("Invalid month  ");
    }

    @Test
    void unknownPictureTokensAreABadPicString() {
        assertThat(DateValidation.validate("2022-07-18", "YYYY-MON-D").feedback()).isEqualTo(Feedback.BAD_PIC_STRING);
        assertThat(DateValidation.validate("2022-07-18", "          ").feedback()).isEqualTo(Feedback.BAD_PIC_STRING);
    }

    @Test
    void otherPicturesAndLengths() {
        assertThat(DateValidation.validate("20220718", "YYYYMMDD").valid()).isTrue();
        assertThat(DateValidation.validate("2022/07/18", "YYYY/MM/DD").valid()).isTrue();
        assertThat(DateValidation.validate("2022200", "YYYYDDD").valid()).isTrue();
        assertThat(DateValidation.validate("2022-07-18-extra", PICTURE).message()).contains("Mask used:YYYY-MM-DD");
    }
}
