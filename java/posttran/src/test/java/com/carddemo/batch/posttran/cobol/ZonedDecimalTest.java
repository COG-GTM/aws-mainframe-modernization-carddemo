package com.carddemo.batch.posttran.cobol;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import java.math.BigDecimal;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class ZonedDecimalTest {

    @ParameterizedTest
    @CsvSource({
            "0000000000{, 0.00",
            "0000005047D, 504.74",
            "0000001000}, -100.00",
            "0000000012L, -1.23",
            "0000000012s, -1.23",
            "00000000000, 0.00",
            "00000012345, 123.45"})
    void decodesOverpunchedSign(String field, String expected) {
        assertThat(ZonedDecimal.decodeSigned(field, 2)).isEqualByComparingTo(expected);
        assertThat(ZonedDecimal.decodeSigned(field, 2).scale()).isEqualTo(2);
    }

    @ParameterizedTest
    @CsvSource({
            "0.00, 0000000000{",
            "504.74, 0000005047D",
            "-100.00, 0000001000}",
            "-1.23, 0000000012L"})
    void encodesWithCobolSignNibbles(String value, String expected) {
        assertThat(ZonedDecimal.encodeSigned(new BigDecimal(value), 9, 2)).isEqualTo(expected);
    }

    @Test
    void truncatesHighOrderDigitsLikeComputeWithoutSizeError() {
        assertThat(ZonedDecimal.truncate(new BigDecimal("1234567890.129"), 9, 2))
                .isEqualByComparingTo("234567890.12");
        assertThat(ZonedDecimal.truncate(new BigDecimal("-1234567890.12"), 9, 2))
                .isEqualByComparingTo("-234567890.12");
    }

    @Test
    void unsignedRoundTrip() {
        assertThat(ZonedDecimal.encodeUnsigned(42, 11)).isEqualTo("00000000042");
        assertThat(ZonedDecimal.decodeUnsigned("00000000042")).isEqualTo(42);
    }

    @Test
    void rejectsNonNumericData() {
        assertThatThrownBy(() -> ZonedDecimal.decodeSigned("00000ABC00{", 2))
                .isInstanceOf(IllegalArgumentException.class);
    }
}
