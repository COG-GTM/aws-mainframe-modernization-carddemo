package com.carddemo.batch.io;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class OverpunchTest {

    @ParameterizedTest(name = "{0} -> {1}")
    @CsvSource({
            "0000000000{, 0.00",
            "0000000000A, 0.01",
            "0000000000B, 0.02",
            "0000000000C, 0.03",
            "0000000000D, 0.04",
            "0000000000E, 0.05",
            "0000000000F, 0.06",
            "0000000000G, 0.07",
            "0000000000H, 0.08",
            "0000000000I, 0.09",
            "0000000001J, -0.11",
            "0000000001K, -0.12",
            "0000000001L, -0.13",
            "0000000001M, -0.14",
            "0000000001N, -0.15",
            "0000000001O, -0.16",
            "0000000001P, -0.17",
            "0000000001Q, -0.18",
            "0000000001R, -0.19",
            "0000000194}, -19.40",
            "0000005047G, 504.77",
            "9999999999I, 999999999.99"
    })
    void parsesAndFormatsEverySign(String zoned, String expected) {
        BigDecimal value = Overpunch.parse(zoned, 2);
        assertThat(value).isEqualTo(new BigDecimal(expected));
        assertThat(value.scale()).isEqualTo(2);
        assertThat(Overpunch.format(value, zoned.length(), 2)).isEqualTo(zoned);
    }

    @Test
    void negativeZeroParsesAsZeroAndFormatsPositive() {
        BigDecimal value = Overpunch.parse("0000000000}", 2);
        assertThat(value.signum()).isZero();
        assertThat(Overpunch.format(value, 11, 2)).isEqualTo("0000000000{");
    }

    @Test
    void rejectsInvalidSignAndOverflow() {
        assertThatThrownBy(() -> Overpunch.parse("00000000005", 2)).isInstanceOf(IllegalArgumentException.class);
        assertThatThrownBy(() -> Overpunch.parse("0000 00000{", 2)).isInstanceOf(IllegalArgumentException.class);
        assertThatThrownBy(() -> Overpunch.format(new BigDecimal("1000.00"), 5, 2))
                .isInstanceOf(IllegalArgumentException.class);
        assertThatThrownBy(() -> Overpunch.format(new BigDecimal("1.005"), 5, 2))
                .isInstanceOf(ArithmeticException.class);
    }
}
