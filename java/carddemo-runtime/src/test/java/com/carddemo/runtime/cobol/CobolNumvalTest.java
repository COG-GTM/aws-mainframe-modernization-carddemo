package com.carddemo.runtime.cobol;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import java.math.BigDecimal;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;

class CobolNumvalTest {

    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {
        "1000|1000", "  12.50  |12.50", "-5|-5", "+  7.1|7.1", "7-|-7", "8 CR|-8", "9db|-9", ".5|0.5", "3.|3"})
    void numvalAcceptsIbmFormats(String in, String expected) {
        assertThat(CobolNumval.testNumval(in)).isZero();
        assertThat(CobolNumval.numval(in)).isEqualByComparingTo(new BigDecimal(expected));
    }

    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {
        "+      2,020.00|2020.00", "$1,000,000.99|1000000.99", "-$5.00|-5.00", "$-5.00|-5.00", "  -1,234.5 |-1234.5"})
    void numvalCAcceptsCurrencyAndCommas(String in, String expected) {
        assertThat(CobolNumval.testNumvalC(in)).isZero();
        assertThat(CobolNumval.numvalC(in)).isEqualByComparingTo(new BigDecimal(expected));
    }

    @ParameterizedTest
    @ValueSource(strings = {"", "   ", "abc", "1.2.3", "+-1", "1 2", "--", "\u0000\u0000", "$5", "1,000"})
    void numvalRejects(String in) {
        assertThat(CobolNumval.testNumval(in)).isPositive();
        assertThatThrownBy(() -> CobolNumval.numval(in)).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void numvalCRejectsAndReportsPosition() {
        assertThat(CobolNumval.testNumvalC("12a")).isEqualTo(3);
        assertThat(CobolNumval.testNumvalC(",5")).isEqualTo(1);
        assertThat(CobolNumval.testNumvalC("5-6")).isEqualTo(3);
    }

    @Test
    void numericClassTest() {
        assertThat(CobolNumval.isNumeric("0123")).isTrue();
        assertThat(CobolNumval.isNumeric("12 ")).isFalse();
        assertThat(CobolNumval.isNumeric("-1")).isFalse();
        assertThat(CobolNumval.isNumeric("")).isFalse();
    }

    @Test
    void numvalCCommaMustBeFollowedByADigit() {
        assertThat(CobolNumval.testNumvalC("1,234")).isZero();
        assertThat(CobolNumval.testNumvalC("1,")).isNotZero();
        assertThat(CobolNumval.testNumvalC("1,.5")).isNotZero();
        assertThat(CobolNumval.testNumvalC("1,,2")).isNotZero();
    }
}
