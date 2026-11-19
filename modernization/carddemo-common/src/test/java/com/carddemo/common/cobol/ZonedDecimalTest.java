package com.carddemo.common.cobol;

import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class ZonedDecimalTest {

    @Test
    void decodesEbcdicStyleOverpunchAsInTheSampleFiles() {
        assertThat(ZonedDecimal.decode("00000012881{", 2, true)).isEqualByComparingTo("1288.10");
        assertThat(ZonedDecimal.decode("0000011648G", 2, true)).isEqualByComparingTo("1164.87");
        assertThat(ZonedDecimal.decode("00000007630}", 2, true)).isEqualByComparingTo("-763.00");
        assertThat(ZonedDecimal.decode("0000005047R", 2, true)).isEqualByComparingTo("-504.79");
        assertThat(ZonedDecimal.decode("00000000123", 2, true)).isEqualByComparingTo("1.23");
        assertThat(ZonedDecimal.decode("00000000007", 0, false)).isEqualTo(BigDecimal.valueOf(7));
        assertThat(ZonedDecimal.decode("0000000000{", 2, true).scale()).isEqualTo(2);
    }

    @Test
    void encodesPositiveWithBraceToIAndNegativeWithBraceToR() {
        assertThat(ZonedDecimal.encode(new BigDecimal("1288.10"), 12, 2, true)).isEqualTo("00000012881{");
        assertThat(ZonedDecimal.encode(new BigDecimal("1164.87"), 11, 2, true)).isEqualTo("0000011648G");
        assertThat(ZonedDecimal.encode(new BigDecimal("-763"), 12, 2, true)).isEqualTo("00000007630}");
        assertThat(ZonedDecimal.encode(new BigDecimal("-0.01"), 11, 2, true)).isEqualTo("0000000000J");
        assertThat(ZonedDecimal.encode(BigDecimal.ZERO, 11, 2, true)).isEqualTo("0000000000{");
        assertThat(ZonedDecimal.encode(BigDecimal.valueOf(42), 4, 0, false)).isEqualTo("0042");
    }

    @Test
    void roundTripsEveryLastDigitAndSign() {
        for (int i = -19; i <= 19; i++) {
            BigDecimal v = BigDecimal.valueOf(i, 2);
            assertThat(ZonedDecimal.decode(ZonedDecimal.encode(v, 5, 2, true), 2, true)).isEqualByComparingTo(v);
        }
    }

    @Test
    void refusesInvalidTextAndOverflowInsteadOfTruncating() {
        assertThatThrownBy(() -> ZonedDecimal.decode("12 4", 0, false)).isInstanceOf(NumberFormatException.class);
        assertThatThrownBy(() -> ZonedDecimal.decode("12{", 0, false)).isInstanceOf(NumberFormatException.class);
        assertThatThrownBy(() -> ZonedDecimal.decode("1{2", 0, true)).isInstanceOf(NumberFormatException.class);
        assertThatThrownBy(() -> ZonedDecimal.encode(new BigDecimal("1000.00"), 5, 2, true)).hasMessageContaining("does not fit");
        assertThatThrownBy(() -> ZonedDecimal.encode(new BigDecimal("1.001"), 5, 2, true)).hasMessageContaining("decimals");
        assertThatThrownBy(() -> ZonedDecimal.encode(new BigDecimal("-1"), 5, 0, false)).hasMessageContaining("unsigned");
    }

    @Test
    void negativeZeroIsDetectedAndWrittenWithTheNegativeOverpunch() {
        assertThat(ZonedDecimal.isNegativeZero("0000000000}", true)).isTrue();
        assertThat(ZonedDecimal.isNegativeZero("0000000000{", true)).isFalse();
        assertThat(ZonedDecimal.isNegativeZero("0000000001}", true)).isFalse();
        assertThat(ZonedDecimal.isNegativeZero("00000000000", false)).isFalse();
        assertThat(ZonedDecimal.decode("0000000000}", 2, true)).isEqualByComparingTo("0");
        assertThat(ZonedDecimal.encode(new BigDecimal("0.00"), 11, 2, true, true)).isEqualTo("0000000000}");
        assertThat(ZonedDecimal.encode(new BigDecimal("-0.00"), 11, 2, true)).as("BigDecimal has no -0").isEqualTo("0000000000{");
        assertThatThrownBy(() -> ZonedDecimal.encode(new BigDecimal("1.00"), 11, 2, true, true))
                .isInstanceOf(IllegalArgumentException.class);
        assertThatThrownBy(() -> ZonedDecimal.encode(BigDecimal.ZERO, 4, 0, false, true)).isInstanceOf(IllegalArgumentException.class);
    }
}
