package com.carddemo.runtime.cobol;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import java.math.BigDecimal;
import org.junit.jupiter.api.Test;

class NumericCodecTest {

    @Test
    void storeTruncatesUnlessRounded() {
        assertThat(CobolNumeric.store(new BigDecimal("12.349"), 5, 2, true)).isEqualByComparingTo("12.34");
        assertThat(CobolNumeric.storeRounded(new BigDecimal("12.345"), 5, 2, true)).isEqualByComparingTo("12.35");
        assertThat(CobolNumeric.storeRounded(new BigDecimal("-12.345"), 5, 2, true)).isEqualByComparingTo("-12.35");
        assertThat(CobolNumeric.store(new BigDecimal("123456.78"), 5, 2, true)).isEqualByComparingTo("23456.78");
        assertThat(CobolNumeric.store(new BigDecimal("-5"), 3, 0, false)).isEqualByComparingTo("5");
        assertThat(CobolNumeric.store(new BigDecimal("1.5"), 3, 2, true).scale()).isEqualTo(2);
        assertThat(CobolNumeric.sizeError(new BigDecimal("1000"), 3, 0)).isTrue();
    }

    @Test
    void packedDecimalEncodesSignNibbles() {
        assertThat(PackedDecimal.length(7, 2)).isEqualTo(5);
        assertThat(PackedDecimal.encode(new BigDecimal("123.45"), 3, 2, true)).containsExactly(0x12, 0x34, 0x5C);
        assertThat(PackedDecimal.encode(new BigDecimal("-123.45"), 3, 2, true)).containsExactly(0x12, 0x34, 0x5D);
        assertThat(PackedDecimal.encode(new BigDecimal("12"), 4, 0, false)).containsExactly(0x00, 0x01, 0x2F);
        assertThat(PackedDecimal.decode(new byte[] {0x12, 0x34, 0x5D}, 2)).isEqualByComparingTo("-123.45");
        assertThat(PackedDecimal.decode(new byte[] {0x12, 0x34, 0x5D}, 2).scale()).isEqualTo(2);
        assertThat(PackedDecimal.isNumeric(new byte[] {0x1A, 0x3C})).isFalse();
        assertThatThrownBy(() -> PackedDecimal.decode(new byte[] {(byte) 0xAB, 0x0C}, 0))
                .isInstanceOf(NumberFormatException.class);
    }

    @Test
    void zonedDecimalUsesTrailingOverpunch() {
        assertThat(ZonedDecimal.encode(new BigDecimal("12.30"), 3, 2, false)).isEqualTo("01230");
        assertThat(ZonedDecimal.encode(new BigDecimal("12.30"), 3, 2, true)).isEqualTo("0123{");
        assertThat(ZonedDecimal.encode(new BigDecimal("-12.31"), 3, 2, true)).isEqualTo("0123J");
        assertThat(ZonedDecimal.decode("0123J", 2)).isEqualByComparingTo("-12.31");
        assertThat(ZonedDecimal.decode("0123{", 2)).isEqualByComparingTo("12.30");
        assertThat(ZonedDecimal.decode("00012", 0)).isEqualByComparingTo("12");
        assertThat(ZonedDecimal.isNumeric("12A", false)).isFalse();
        assertThat(ZonedDecimal.isNumeric("  ", false)).isFalse();
    }

    @Test
    void binaryUsesHalfwordFullwordDoubleword() {
        assertThat(BinaryInteger.length(4)).isEqualTo(2);
        assertThat(BinaryInteger.length(9)).isEqualTo(4);
        assertThat(BinaryInteger.length(18)).isEqualTo(8);
        assertThat(BinaryInteger.encode(BigDecimal.valueOf(-1), 9, 0, true)).containsExactly(0xFF, 0xFF, 0xFF, 0xFF);
        assertThat(BinaryInteger.decode(new byte[] {(byte) 0xFF, (byte) 0xFE}, 0, true)).isEqualByComparingTo("-2");
        assertThat(BinaryInteger.decode(new byte[] {(byte) 0xFF, (byte) 0xFE}, 0, false)).isEqualByComparingTo("65534");
        assertThat(BinaryInteger.encode(BigDecimal.valueOf(12345), 4, 0, true)).containsExactly(0x09, 0x29);
    }

    @Test
    void ebcdicCp037() {
        assertThat(Ebcdic.encode("A1 ")).containsExactly(0xC1, 0xF1, 0x40);
        assertThat(Ebcdic.decode(new byte[] {(byte) 0xC1, (byte) 0xF1, 0x40})).isEqualTo("A1 ");
    }
}
