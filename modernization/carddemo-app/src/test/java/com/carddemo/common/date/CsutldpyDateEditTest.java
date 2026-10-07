package com.carddemo.common.date;

import com.carddemo.common.date.CsutldpyDateEdit.Flag;
import com.carddemo.common.date.CsutldpyDateEdit.Result;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.time.LocalDate;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/** EDIT-DATE-CCYYMMDD paragraphs of CSUTLDPY, messages verbatim from the copybook. */
class CsutldpyDateEditTest {

    @ParameterizedTest
    @CsvSource(delimiter = '|', nullValues = "NULL", value = {
            "20220706|false|VALID|VALID|VALID|NULL",
            "20240229|false|VALID|VALID|VALID|NULL",
            "20000229|false|VALID|VALID|VALID|NULL",
            "19000229|true|NOT_OK|NOT_OK|NOT_OK|Open Date:Not a leap year.Cannot have 29 days in this month.",
            "20230229|true|NOT_OK|NOT_OK|NOT_OK|Open Date:Not a leap year.Cannot have 29 days in this month.",
            "20220230|true|VALID|NOT_OK|NOT_OK|Open Date:Cannot have 30 days in this month.",
            "20220431|true|VALID|NOT_OK|NOT_OK|Open Date:Cannot have 31 days in this month.",
            "18000101|true|NOT_OK|VALID|VALID|Open Date : Century is not valid.",
            "20X20101|true|NOT_OK|VALID|VALID|Open Date must be 4 digit number.",
            "'    0101'|true|BLANK|VALID|VALID|Open Date : Year must be supplied.",
            "2022  01|true|VALID|BLANK|VALID|Open Date : Month must be supplied.",
            "20221301|true|VALID|NOT_OK|VALID|Open Date: Month must be a number between 1 and 12.",
            "202201  |true|VALID|VALID|BLANK|Open Date : Day must be supplied.",
            "20220132|true|VALID|VALID|NOT_OK|Open Date:day must be a number between 1 and 31.",
            "202201X1|true|VALID|VALID|NOT_OK|Open Date:day must be a number between 1 and 31.",
            "20220100|true|VALID|VALID|NOT_OK|Open Date:day must be a number between 1 and 31.",
            "'2022011 '|false|VALID|VALID|VALID|NULL",
            "'        '|true|BLANK|BLANK|BLANK|Open Date : Year must be supplied."
    })
    void editsCcyymmdd(String date, boolean error, Flag year, Flag month, Flag day, String message) {
        Result r = CsutldpyDateEdit.editDateCcyymmdd("Open Date", date);
        assertThat(r).isEqualTo(new Result(error, year, month, day, message));
    }

    @Test
    void lowValuesCountAsNotSupplied() {
        Result r = CsutldpyDateEdit.editDateCcyymmdd("Date", "\u0000\u0000\u0000\u000001\u0000\u0000");
        assertThat(r.year()).isEqualTo(Flag.BLANK);
        assertThat(r.day()).isEqualTo(Flag.BLANK);
    }

    @Test
    void dateOfBirthMustBeInThePast() {
        LocalDate today = LocalDate.of(2022, 7, 6);
        assertThat(CsutldpyDateEdit.editDateOfBirth("Date of Birth", "19800101", today))
                .isEqualTo(new Result(false, Flag.VALID, Flag.VALID, Flag.VALID, null));
        assertThat(CsutldpyDateEdit.editDateOfBirth("Date of Birth", "20220706", today))
                .isEqualTo(new Result(true, Flag.NOT_OK, Flag.NOT_OK, Flag.NOT_OK,
                        "Date of Birth:cannot be in the future "));
        assertThatThrownBy(() -> CsutldpyDateEdit.editDateOfBirth("DOB", "2022-1-1", today))
                .isInstanceOf(IllegalArgumentException.class);
    }
}
