package com.carddemo.common.date;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.time.LocalDate;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class CobolDatesTest {

    @ParameterizedTest
    @CsvSource({"1600, true", "1700, false", "1900, false", "2000, true", "2023, false", "2024, true"})
    void gregorianLeapYears(int year, boolean leap) {
        assertThat(CobolDates.isLeapYear(year)).isEqualTo(leap);
        assertThat(CobolDates.daysInMonth(year, 2)).isEqualTo(leap ? 29 : 28);
    }

    @Test
    void daysInMonth() {
        assertThat(CobolDates.daysInMonth(2022, 1)).isEqualTo(31);
        assertThat(CobolDates.daysInMonth(2022, 4)).isEqualTo(30);
        assertThatThrownBy(() -> CobolDates.daysInMonth(2022, 13)).isInstanceOf(IllegalArgumentException.class);
    }

    /** Lilian values produced by GnuCOBOL 3.1.2: FUNCTION INTEGER-OF-DATE(d) + 6653. */
    @ParameterizedTest
    @CsvSource({"16010101, 1, 6654", "20000101, 145732, 152385", "20220706, 153954, 160607",
            "99991231, 3067671, 3074324"})
    void matchesGnuCobolIntegerOfDate(int yyyymmdd, int integerOfDate, int lilian) {
        assertThat(CobolDates.integerOfDate(yyyymmdd)).isEqualTo(integerOfDate);
        assertThat(CobolDates.dateOfInteger(integerOfDate)).isEqualTo(yyyymmdd);
        assertThat(CobolDates.lilian(CobolDates.toDate(yyyymmdd))).isEqualTo(lilian);
        assertThat(CobolDates.fromLilian(lilian)).isEqualTo(CobolDates.toDate(yyyymmdd));
    }

    @Test
    void lilianDayOneIsGregorianReformDay() {
        assertThat(CobolDates.lilian(LocalDate.of(1582, 10, 15))).isEqualTo(1);
        assertThatThrownBy(() -> CobolDates.fromLilian(0)).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void rejectsOutOfRangeDates() {
        assertThatThrownBy(() -> CobolDates.integerOfDate(16001231)).isInstanceOf(IllegalArgumentException.class);
        assertThatThrownBy(() -> CobolDates.integerOfDate(20230229)).isInstanceOf(IllegalArgumentException.class);
        assertThatThrownBy(() -> CobolDates.dateOfInteger(0)).isInstanceOf(IllegalArgumentException.class);
        assertThatThrownBy(() -> CobolDates.dateOfInteger(3067672)).isInstanceOf(IllegalArgumentException.class);
    }
}
