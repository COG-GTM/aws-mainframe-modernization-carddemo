package com.carddemo.common.codec;

import static org.assertj.core.api.Assertions.assertThat;

import java.math.BigDecimal;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;

class NumvalCTest {

    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {
        "1940.00|1940.00",
        "  1940  |1940",
        "$1,940.50|1940.50",
        "-$12.5|-12.5",
        "$ -12.5|-12.5",
        "+7|7",
        "12.5-|-12.5",
        "12.5 CR|-12.5",
        "12.5db|-12.5",
        ".75|0.75",
        "5.|5"})
    void validFormats(String text, String expected) {
        assertThat(NumvalC.isValid(text)).isTrue();
        assertThat(NumvalC.parse(text)).hasValueSatisfying(v -> assertThat(v).isEqualByComparingTo(new BigDecimal(expected)));
    }

    @ParameterizedTest
    @ValueSource(strings = {"", "  ", "abc", "1.2.3", "--1", "-1-", "+-5", "$", ".", "1 2", "12a", "1e5", "CR"})
    void invalidFormats(String text) {
        assertThat(NumvalC.isValid(text)).isFalse();
        assertThat(NumvalC.parse(text)).isEmpty();
    }
}
