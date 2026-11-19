package com.carddemo.common.cobol;

import com.carddemo.common.error.NumericDataException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class ZonedDecimalTest {

    @ParameterizedTest
    @CsvSource({
            "0000005047G, 2, 504.77",
            "0000005047P, 2, -504.77",
            "0000000000{, 2, 0.00",
            "0000000000}, 2, 0.00",
            "0000001000}, 2, -100.00",
            "00000000001, 2, 0.01",
            "000000000I, 0, 9",
            "000000000R, 0, -9",
            "000000000A, 0, 1",
            "000000000J, 0, -1",
    })
    void decodesOverpunchedAndUnsignedFields(String field, int scale, String expected) {
        assertThat(ZonedDecimal.decode(field, scale)).isEqualTo(new BigDecimal(expected));
    }

    @ParameterizedTest
    @CsvSource({
            "504.77, 9, 2, true, 0000005047G",
            "-504.77, 9, 2, true, 0000005047P",
            "0, 9, 2, true, 0000000000{",
            "-100.00, 9, 2, true, 0000001000}",
            "42, 4, 0, false, 0042",
            "7, 11, 0, false, 00000000007",
    })
    void encodesWithCobolSignConvention(String value, int integerDigits, int scale, boolean signed, String expected) {
        assertThat(ZonedDecimal.encode(new BigDecimal(value), integerDigits, scale, signed)).isEqualTo(expected);
    }

    @Test
    void roundTripsEveryLastDigitAndSign() {
        for (int i = -19; i <= 19; i++) {
            BigDecimal value = BigDecimal.valueOf(i, 1);
            String encoded = ZonedDecimal.encode(value, 3, 1, true);
            assertThat(ZonedDecimal.decode(encoded, 1)).isEqualTo(value);
        }
    }

    @Test
    void overpunchCharactersMatchCp037Zones() {
        // EBCDIC X'C7' (zone C = +, digit 7) decodes to 'G'; X'D7' (zone D = -) to 'P'.
        assertThat(CodePage.CP037.decode(new byte[] {(byte) 0xC7, (byte) 0xD7, (byte) 0xC0, (byte) 0xD0}))
                .isEqualTo("GP{}");
    }

    @Test
    void invalidDataIsANumericDataExceptionNotZero() {
        assertThatThrownBy(() -> ZonedDecimal.decode("00000 5047G", 2)).isInstanceOf(NumericDataException.class);
        assertThatThrownBy(() -> ZonedDecimal.decode("000000000-", 2)).isInstanceOf(NumericDataException.class);
        assertThatThrownBy(() -> ZonedDecimal.decode("", 2)).isInstanceOf(NumericDataException.class);
    }

    @Test
    void refusesValuesThatDoNotFitThePicture() {
        assertThatThrownBy(() -> ZonedDecimal.encode(new BigDecimal("1000"), 3, 0, true))
                .isInstanceOf(IllegalArgumentException.class);
        assertThatThrownBy(() -> ZonedDecimal.encode(new BigDecimal("1.234"), 3, 2, true))
                .isInstanceOf(IllegalArgumentException.class);
        assertThatThrownBy(() -> ZonedDecimal.encode(new BigDecimal("-1"), 3, 0, false))
                .isInstanceOf(IllegalArgumentException.class);
    }
}
