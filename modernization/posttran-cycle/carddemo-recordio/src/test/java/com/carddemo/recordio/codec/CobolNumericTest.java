package com.carddemo.recordio.codec;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class CobolNumericTest {

    @ParameterizedTest
    @CsvSource({
            "0000005047G, 504.77",    // first DALYTRAN record in app/data: C7 overpunch = +7
            "0000000000{, 0.00",      // +0
            "0000000000}, 0.00",      // -0 decodes as zero
            "0000012345}, -1234.50",  // } = -0
            "0000012345R, -1234.59",  // R = -9
            "9999999999I, 999999999.99",
            "00000000001, 0.01",      // unsigned-looking digit in a signed field is positive
    })
    void decodesZonedSignOverpunch(String image, String expected) {
        assertThat(CobolNumeric.decodeZoned(image, 2, true)).isEqualByComparingTo(new BigDecimal(expected));
    }

    @ParameterizedTest
    @CsvSource({
            "504.77, 0000005047G",
            "0, 0000000000{",
            "-1234.50, 0000012345}",
            "-1234.59, 0000012345R",
            "-0.01, 000000000J",
    })
    void encodesZonedSignOverpunch(String value, String expected) {
        int digits = expected.length();
        assertThat(CobolNumeric.encodeZoned(new BigDecimal(value), digits, 2, true)).isEqualTo(expected);
    }

    @Test
    void unsignedPictureDropsSign() {
        assertThat(CobolNumeric.encodeZoned(new BigDecimal("-12"), 4, 0, false)).isEqualTo("0012");
        assertThat(CobolNumeric.decodeZoned("0012", 0, false)).isEqualByComparingTo("12");
    }

    @Test
    void storeTruncatesLowOrderDecimalsWithoutRounding() {
        assertThat(CobolNumeric.truncate(new BigDecimal("12.999"), 9, 2)).isEqualByComparingTo("12.99");
        assertThat(CobolNumeric.truncate(new BigDecimal("-12.999"), 9, 2)).isEqualByComparingTo("-12.99");
    }

    @Test
    void storeTruncatesHighOrderDigitsSilently() {
        // PIC S9(09)V99 receiving 1,234,567,890.12 keeps only the low 9 integer digits
        assertThat(CobolNumeric.truncate(new BigDecimal("1234567890.12"), 9, 2)).isEqualByComparingTo("234567890.12");
        assertThat(CobolNumeric.fits(new BigDecimal("1234567890.12"), 9, 2)).isFalse();
        assertThat(CobolNumeric.fits(new BigDecimal("999999999.99"), 9, 2)).isTrue();
    }

    @Test
    void rejectsNonNumericImage() {
        assertThatThrownBy(() -> CobolNumeric.decodeZoned("00000ABC000", 2, true))
                .isInstanceOf(RecordFormatException.class);
        assertThatThrownBy(() -> CobolNumeric.decodeZoned("           ", 2, true))
                .isInstanceOf(RecordFormatException.class);
        assertThatThrownBy(() -> CobolNumeric.decodeZoned("000000000{0", 2, false))
                .isInstanceOf(RecordFormatException.class);
    }
}
