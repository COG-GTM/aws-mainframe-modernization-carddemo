package com.carddemo.common.cobol;

import com.carddemo.common.error.NumericDataException;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class PackedDecimalTest {

    private static byte[] bytes(int... values) {
        byte[] out = new byte[values.length];
        for (int i = 0; i < values.length; i++) {
            out[i] = (byte) values[i];
        }
        return out;
    }

    @Test
    void encodesWithCdfSignNibbles() {
        assertThat(PackedDecimal.encode(new BigDecimal("12345"), 5, 0, true)).containsExactly(bytes(0x12, 0x34, 0x5C));
        assertThat(PackedDecimal.encode(new BigDecimal("-12.34"), 3, 2, true)).containsExactly(bytes(0x01, 0x23, 0x4D));
        assertThat(PackedDecimal.encode(new BigDecimal("7"), 4, 0, false)).containsExactly(bytes(0x00, 0x00, 0x7F));
        assertThat(PackedDecimal.encode(new BigDecimal("504.77"), 9, 2, true))
                .containsExactly(bytes(0x00, 0x00, 0x00, 0x50, 0x47, 0x7C));
    }

    @Test
    void decodesAllValidSignNibbles() {
        assertThat(PackedDecimal.decode(bytes(0x12, 0x34, 0x5C), 0)).isEqualTo(new BigDecimal("12345"));
        assertThat(PackedDecimal.decode(bytes(0x12, 0x34, 0x5F), 2)).isEqualTo(new BigDecimal("123.45"));
        assertThat(PackedDecimal.decode(bytes(0x12, 0x34, 0x5A), 0)).isEqualTo(new BigDecimal("12345"));
        assertThat(PackedDecimal.decode(bytes(0x12, 0x34, 0x5E), 0)).isEqualTo(new BigDecimal("12345"));
        assertThat(PackedDecimal.decode(bytes(0x01, 0x23, 0x4D), 2)).isEqualTo(new BigDecimal("-12.34"));
        assertThat(PackedDecimal.decode(bytes(0x01, 0x23, 0x4B), 2)).isEqualTo(new BigDecimal("-12.34"));
    }

    @Test
    void byteLengthIsDigitsOverTwoPlusOne() {
        assertThat(PackedDecimal.byteLength(1)).isEqualTo(1);
        assertThat(PackedDecimal.byteLength(4)).isEqualTo(3);
        assertThat(PackedDecimal.byteLength(5)).isEqualTo(3);
        assertThat(PackedDecimal.byteLength(11)).isEqualTo(6);
    }

    @Test
    void roundTrips() {
        for (String v : new String[] {"0.00", "-0.01", "999999999.99", "-999999999.99", "1234.50"}) {
            BigDecimal value = new BigDecimal(v);
            assertThat(PackedDecimal.decode(PackedDecimal.encode(value, 9, 2, true), 2)).isEqualTo(value);
        }
    }

    @Test
    void badNibblesAbendInsteadOfReadingZero() {
        assertThatThrownBy(() -> PackedDecimal.decode(bytes(0x1A, 0x3C), 0))
                .isInstanceOf(NumericDataException.class).hasMessageContaining("X'1A3C'");
        assertThatThrownBy(() -> PackedDecimal.decode(bytes(0x12, 0x34), 0))
                .isInstanceOf(NumericDataException.class).hasMessageContaining("sign");
        assertThatThrownBy(() -> PackedDecimal.decode(bytes(0x40, 0x40, 0x40), 0))
                .isInstanceOf(NumericDataException.class);
    }
}
