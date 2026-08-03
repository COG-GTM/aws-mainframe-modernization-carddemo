package com.carddemo.interestcalc.copybook;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import java.math.BigDecimal;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/** Unit tests for the zoned decimal codec, including the trailing EBCDIC sign overpunch. */
class ZonedDecimalTest {

    @ParameterizedTest(name = "{0} decodes to {1}")
    @CsvSource({
            "'0000001004H', 100.48",   // H = positive 8
            "'0000001004Q', -100.48",  // Q = negative 8
            "'0000000000{', 0.00",     // { = positive 0
            "'0000000000}', 0.00",     // } = negative 0 - still zero
            "'0123456789I', 12345678.99",
            "'00000000000', 0.00",
    })
    @DisplayName("PIC S9(09)V99 values round-trip through the overpunch encoding")
    void decodesSignedDisplayFields(String raw, BigDecimal expected) {
        assertThat(ZonedDecimal.decode(raw, 9, 2)).isEqualByComparingTo(expected);
    }

    @Test
    @DisplayName("Encoding is the inverse of decoding for the shipped sample values")
    void encodeIsTheInverseOfDecode() {
        for (String raw : new String[] {"0000001004H", "0000001004Q", "0000000000{", "0123456789I"}) {
            assertThat(ZonedDecimal.encode(ZonedDecimal.decode(raw, 9, 2), 9, 2)).isEqualTo(raw);
        }
    }

    @Test
    @DisplayName("The sign and the decimal point do not occupy bytes: S9(9)V99 is 11 bytes")
    void fieldWidthExcludesSignAndDecimalPoint() {
        assertThat(ZonedDecimal.encode(new BigDecimal("100.48"), 9, 2)).hasSize(11);
        assertThat(ZonedDecimal.encode(new BigDecimal("100.48"), 10, 2)).hasSize(12);
        assertThatThrownBy(() -> ZonedDecimal.decode("123", 9, 2))
                .isInstanceOf(IllegalArgumentException.class)
                .hasMessageContaining("expected 11 bytes");
    }

    @Test
    @DisplayName("Storing into a PIC clause truncates low-order digits toward zero")
    void storeTruncatesTowardZero() {
        assertThat(CobolNumeric.store(new BigDecimal("1.256"), 9, 2)).isEqualByComparingTo("1.25");
        assertThat(CobolNumeric.store(new BigDecimal("-1.256"), 9, 2)).isEqualByComparingTo("-1.25");
        assertThat(CobolNumeric.store(new BigDecimal("1.999"), 9, 2)).isEqualByComparingTo("1.99");
    }

    @Test
    @DisplayName("Storing into a PIC clause truncates high-order digits, as COBOL does without ON SIZE ERROR")
    void storeTruncatesHighOrderDigits() {
        assertThat(CobolNumeric.store(new BigDecimal("1234567890.12"), 9, 2))
                .as("PIC S9(09)V99 holds nine integer digits")
                .isEqualByComparingTo("234567890.12");
        assertThat(CobolNumeric.store(new BigDecimal("-1234567890.12"), 9, 2))
                .isEqualByComparingTo("-234567890.12");
    }
}
