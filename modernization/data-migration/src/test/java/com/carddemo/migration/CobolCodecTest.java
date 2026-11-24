package com.carddemo.migration;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import java.math.BigDecimal;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class CobolCodecTest {

    @ParameterizedTest
    @CsvSource({
            "00000001940{, 2, 194.00",
            "0000005047G, 2, 504.77",
            "0000009190}, 2, -919.00",
            "0000000123R, 2, -12.39",
            "00150{, 2, 15.00",
            "0000000001, 0, 1"
    })
    void decodesAsciiOverpunchedZonedDecimal(String text, int scale, String expected) {
        assertThat(CobolCodec.zonedFromAscii(text, scale)).isEqualByComparingTo(new BigDecimal(expected));
    }

    @Test
    void decodesEbcdicZonedDecimalWithSignZone() {
        byte[] positive = {(byte) 0xF0, (byte) 0xF1, (byte) 0xF2, (byte) 0xC5};
        byte[] negative = {(byte) 0xF0, (byte) 0xF1, (byte) 0xF2, (byte) 0xD5};
        byte[] unsigned = {(byte) 0xF0, (byte) 0xF1, (byte) 0xF2, (byte) 0xF5};
        assertThat(CobolCodec.zonedFromEbcdic(positive, 0, 4, 2)).isEqualByComparingTo("1.25");
        assertThat(CobolCodec.zonedFromEbcdic(negative, 0, 4, 2)).isEqualByComparingTo("-1.25");
        assertThat(CobolCodec.zonedFromEbcdic(unsigned, 0, 4, 0)).isEqualByComparingTo("125");
    }

    @Test
    void decodesPackedDecimal() {
        // PIC S9(9)V99 COMP-3 = 6 bytes: 12345678.90 -> 01 23 45 67 89 0C
        byte[] bytes = {0x01, 0x23, 0x45, 0x67, (byte) 0x89, 0x0C};
        assertThat(CobolCodec.packed(bytes, 0, 6, 2)).isEqualByComparingTo("12345678.90");
        byte[] negative = {0x00, 0x00, 0x00, 0x01, 0x23, 0x4D};
        assertThat(CobolCodec.packed(negative, 0, 6, 2)).isEqualByComparingTo("-12.34");
        byte[] unsignedF = {0x12, 0x3F};
        assertThat(CobolCodec.packed(unsignedF, 0, 2, 0)).isEqualByComparingTo("123");
    }

    @Test
    void packedRoundTrip() {
        for (String v : new String[] {"0.00", "1.25", "-919.00", "999999999.99", "-0.01"}) {
            byte[] packed = CobolCodec.toPacked(new BigDecimal(v), 11, 2);
            assertThat(packed).hasSize(6);
            assertThat(CobolCodec.packed(packed, 0, packed.length, 2)).isEqualByComparingTo(v);
        }
    }

    @Test
    void rejectsInvalidPackedSign() {
        byte[] bad = {0x12, 0x34};
        assertThatThrownBy(() -> CobolCodec.packed(bad, 0, 2, 0)).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void decodesEbcdicCharacters() {
        byte[] hello = {(byte) 0xC8, (byte) 0x85, (byte) 0x93, (byte) 0x93, (byte) 0x96, 0x40, (byte) 0x7B};
        assertThat(CobolCodec.ebcdicToString(hello, 0, hello.length)).isEqualTo("Hello #");
    }
}
