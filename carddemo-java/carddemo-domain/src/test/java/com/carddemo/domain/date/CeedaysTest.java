package com.carddemo.domain.date;

import static org.assertj.core.api.Assertions.assertThat;

import java.time.LocalDate;
import java.time.temporal.ChronoUnit;
import org.junit.jupiter.api.Test;

class CeedaysTest {

    @Test
    void lilianDayOneIs15October1582() {
        assertThat(Ceedays.ceedays("1582-10-15", "YYYY-MM-DD")).isEqualTo(new Ceedays.Result(CeedaysFeedback.VALID, 1));
    }

    @Test
    void lilianDayIsJulianDayNumberMinus2299160() {
        // 1 January 2000 is Julian day number 2451545.
        assertThat(Ceedays.ceedays("2000-01-01", "YYYY-MM-DD").lilian()).isEqualTo(2451545 - 2299160);
    }

    @Test
    void julianDayOfYearPicture() {
        int lilian = (int) ChronoUnit.DAYS.between(Ceedays.LILIAN_DAY_ONE, LocalDate.of(2024, 12, 31)) + 1;

        assertThat(Ceedays.ceedays("2024366", "YYYYDDD")).isEqualTo(new Ceedays.Result(CeedaysFeedback.VALID, lilian));
        assertThat(Ceedays.ceedays("2023366", "YYYYDDD").feedback()).isEqualTo(CeedaysFeedback.BAD_DATE_VALUE);
        assertThat(Ceedays.ceedays("2023000", "YYYYDDD").feedback()).isEqualTo(CeedaysFeedback.BAD_DATE_VALUE);
    }

    @Test
    void leadingBlanksOfTheDateAndTrailingBlanksOfThePictureAreIgnored() {
        assertThat(Ceedays.ceedays("  20240229", "YYYYMMDD  ").valid()).isTrue();
        assertThat(Ceedays.ceedays("20240229XX", "YYYYMMDD").valid()).isTrue();
    }

    @Test
    void pictureLettersAreCaseInsensitive() {
        assertThat(Ceedays.ceedays("2024-02-29", "yyyy-mm-dd").valid()).isTrue();
    }

    @Test
    void picturesCardDemoDoesNotUseAreBadPicStrings() {
        assertThat(Ceedays.ceedays("24-02-29", "YY-MM-DD").feedback()).isEqualTo(CeedaysFeedback.BAD_PIC_STRING);
        assertThat(Ceedays.ceedays("2024-02", "YYYY-MM").feedback()).isEqualTo(CeedaysFeedback.BAD_PIC_STRING);
        assertThat(Ceedays.ceedays("2024-02-29", "YYYY-MM-DD-DD").feedback()).isEqualTo(CeedaysFeedback.BAD_PIC_STRING);
        assertThat(Ceedays.ceedays("2024-02-29", "1YYY-MM-DD").feedback()).isEqualTo(CeedaysFeedback.BAD_PIC_STRING);
    }

    @Test
    void invalidResultsCarryNoLilianDay() {
        assertThat(Ceedays.ceedays("2023-02-29", "YYYY-MM-DD")).isEqualTo(new Ceedays.Result(CeedaysFeedback.BAD_DATE_VALUE, 0));
    }

    @Test
    void tokensSplitIntoSeverityAndMessageNumber() {
        assertThat(CeedaysFeedback.VALID.severity()).isZero();
        assertThat(CeedaysFeedback.VALID.messageNumber()).isZero();
        assertThat(CeedaysFeedback.INSUFFICIENT_DATA.messageNumber()).isEqualTo(2507);
        assertThat(CeedaysFeedback.YEAR_IN_ERA_ZERO.severity()).isEqualTo(3);
        assertThat(CeedaysFeedback.YEAR_IN_ERA_ZERO.messageNumber()).isEqualTo(2521);
        assertThat(CeedaysFeedback.ofToken(new byte[8])).isEqualTo(CeedaysFeedback.VALID);
        assertThat(CeedaysFeedback.ofToken(new byte[] {0, 2, 9, (byte) 0xC4, 0x59, (byte) 0xC3, (byte) 0xC5, (byte) 0xC5})).isNull();
    }
}
