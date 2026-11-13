package com.carddemo.common.cobol;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class PackedDecimalTest {

    @ParameterizedTest
    @CsvSource({
            // value, digits, scale, signed, hex
            "12345, 5, 0, true, 12345C",
            "-12345, 5, 0, true, 12345D",
            "12345, 5, 0, false, 12345F",
            "1234, 4, 0, true, 01234C",
            "194.00, 11, 2, true, 00000019400C",       // PIC S9(9)V99 COMP-3 (CIPAUSMY PA-CREDIT-LIMIT)
            "-0.01, 12, 2, true, 0000000000001D",      // PIC S9(10)V99 COMP-3 (CIPAUDTY PA-TRANSACTION-AMT)
            "41000000001, 11, 0, true, 41000000001C",  // PIC S9(11) COMP-3 (PA-ACCT-ID)
            "0, 1, 0, true, 0C"
    })
    void roundTrips(String value, int digits, int scale, boolean signed, String hex) {
        byte[] encoded = PackedDecimal.encode(new BigDecimal(value), digits, scale, signed);
        assertThat(Hex.of(encoded)).isEqualTo(hex);
        assertThat(PackedDecimal.decode(encoded, 0, encoded.length, scale))
                .isEqualByComparingTo(new BigDecimal(value));
    }

    @Test
    void byteLengthFollowsDigitCount() {
        assertThat(PackedDecimal.byteLength(11)).isEqualTo(6);
        assertThat(PackedDecimal.byteLength(12)).isEqualTo(7);
        assertThat(PackedDecimal.byteLength(5)).isEqualTo(3);
        assertThat(PackedDecimal.byteLength(9)).isEqualTo(5);
    }

    @Test
    void acceptsAlternateSignNibbles() {
        assertThat(PackedDecimal.decode(Hex.parse("123B"), 0, 2, 0)).isEqualByComparingTo("-123");
        assertThat(PackedDecimal.decode(Hex.parse("123A"), 0, 2, 0)).isEqualByComparingTo("123");
        assertThat(PackedDecimal.decode(Hex.parse("123E"), 0, 2, 1)).isEqualByComparingTo("12.3");
    }

    @Test
    void decodesMoreThanEighteenDigits() {
        byte[] b = Hex.parse("1234567890123456789012345C");
        assertThat(PackedDecimal.decode(b, 0, b.length, 0)).isEqualByComparingTo("1234567890123456789012345");
    }

    @Test
    void truncatesDecimalsLikeCobolMove() {
        assertThat(Hex.of(PackedDecimal.encode(new BigDecimal("1.239"), 3, 2, true))).isEqualTo("123C");
    }

    @Test
    void rejectsInvalidData() {
        assertThatThrownBy(() -> PackedDecimal.decode(Hex.parse("1A3C"), 0, 2, 0)).isInstanceOf(CobolDataException.class);
        assertThatThrownBy(() -> PackedDecimal.decode(Hex.parse("1234"), 0, 2, 0)).isInstanceOf(CobolDataException.class);
        assertThat(PackedDecimal.isValid(Hex.parse("4040"), 0, 2)).isFalse();
        assertThatThrownBy(() -> PackedDecimal.encode(new BigDecimal("1000"), 3, 0, true))
                .isInstanceOf(CobolDataException.class);
    }
}
