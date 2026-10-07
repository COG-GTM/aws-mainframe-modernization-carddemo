package com.carddemo.common.codec;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.ValueSource;

import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class PictureAndEditingTest {

    @Test
    void parsesNumericPictures() {
        Picture p = Picture.parse("S9(10)V99");
        assertThat(p).isEqualTo(new Picture("S9999999999V99", Picture.Category.NUMERIC, 12, 12, 2, true));
        assertThat(p.isNumeric()).isTrue();
        assertThat(Picture.parse("9(03)")).isEqualTo(new Picture("999", Picture.Category.NUMERIC, 3, 3, 0, false));
        assertThat(Picture.parse("v99").scale()).isEqualTo(2);
    }

    @Test
    void parsesAlphanumericPictures() {
        assertThat(Picture.parse("X(08)")).isEqualTo(new Picture("XXXXXXXX", Picture.Category.ALPHANUMERIC, 8, 0, 0,
                false));
        assertThat(Picture.parse("A(3)").category()).isEqualTo(Picture.Category.ALPHABETIC);
        assertThat(Picture.parse("XX9").category()).isEqualTo(Picture.Category.ALPHANUMERIC);
        assertThat(Picture.parse("X(2)").isNumeric()).isFalse();
    }

    @Test
    void parsesEditedPictures() {
        Picture p = Picture.parse("-ZZZ,ZZZ,ZZZ.ZZ");
        assertThat(p.category()).isEqualTo(Picture.Category.NUMERIC_EDITED);
        assertThat(p.size()).isEqualTo(15);
        assertThat(p.digits()).isEqualTo(11);
        assertThat(p.scale()).isEqualTo(2);
        assertThat(p.signed()).isTrue();
        assertThat(Picture.parse("Z(9).99").signed()).isFalse();
        assertThat(Picture.parse("9(4).99CR").signed()).isTrue();
        assertThat(Picture.parse("ZZ9V99").size()).isEqualTo(5);
    }

    @Test
    void expandsRepetitionFactors() {
        assertThat(Picture.expand("x(3)9( 2 )")).isEqualTo("XXX99");
    }

    @ParameterizedTest
    @ValueSource(strings = {"", "X(", "X(0)", "(3)", "X(a)", "S9P", "XZ"})
    void rejectsMalformedPictures(String pic) {
        assertThatThrownBy(() -> Picture.parse(pic)).isInstanceOf(RecordFormatException.class);
    }

    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {
            "-ZZZ,ZZZ,ZZZ.ZZ|-1234567.891|'-  1,234,567.89'",
            "-ZZZ,ZZZ,ZZZ.ZZ|1234567.891|'   1,234,567.89'",
            "+ZZZ,ZZZ,ZZZ.ZZ|1234567.89|'+  1,234,567.89'",
            "+ZZZ,ZZZ,ZZZ.ZZ|0.05|'+           .05'",
            "Z(9).99-|-12.5|'       12.50-'",
            "+99999999.99|12.5|'+00000012.50'",
            "+99999999.99|-12.5|'-00000012.50'",
            "+9999999999|-7|'-0000000007'",
            "ZZZ,ZZ9.99|0|'      0.00'",
            "9999.99CR|-1|'0001.00CR'",
            "9999.99CR|1|'0001.00  '",
            "9999.99DB|-1|'0001.00DB'",
            "99/99/99|70622|'07/06/22'",
            "999B999|123456|'123 456'",
            "9990999|123456|'1230456'",
            "****9.99|12.5|'***12.50'",
            "ZZ9.99|1.999|'  1.99'",
            "99.9|123.45|'23.4'",
            "-ZZ9|-0.4|'   0'"
    })
    void formatsLikeCobolEditedMove(String picture, String value, String expected) {
        assertThat(NumericEdited.format(new BigDecimal(value), picture)).isEqualTo(expected);
    }

    @Test
    void zeroInPictureWithoutNinesIsSpaces() {
        assertThat(NumericEdited.format(BigDecimal.ZERO, "ZZ,ZZZ.ZZ")).isEqualTo(" ".repeat(9));
        assertThat(NumericEdited.format(new BigDecimal("-0.001"), "-ZZ.ZZ")).isEqualTo(" ".repeat(6));
    }

    @Test
    void rejectsNonEditedPictures() {
        assertThatThrownBy(() -> NumericEdited.format(BigDecimal.ONE, "9(3)"))
                .isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void usageKeywords() {
        assertThat(Usage.fromKeyword("comp-3")).contains(Usage.PACKED);
        assertThat(Usage.fromKeyword("PACKED-DECIMAL")).contains(Usage.PACKED);
        assertThat(Usage.fromKeyword("COMP")).contains(Usage.BINARY);
        assertThat(Usage.fromKeyword("BINARY")).contains(Usage.BINARY);
        assertThat(Usage.fromKeyword("DISPLAY")).contains(Usage.DISPLAY);
        assertThat(Usage.fromKeyword("PIC")).isEmpty();
        assertThatThrownBy(() -> Usage.fromKeyword("COMP-2")).isInstanceOf(RecordFormatException.class);
    }

    @Test
    void recordEncodings() {
        assertThat(RecordEncoding.of("cp037")).isSameAs(RecordEncoding.EBCDIC);
        assertThat(RecordEncoding.of("ASCII")).isSameAs(RecordEncoding.ASCII);
        assertThat(RecordEncoding.of("windows-1252").charset().name()).isEqualTo("windows-1252");
        assertThat(RecordEncoding.EBCDIC.space()).isEqualTo((byte) 0x40);
        assertThat(RecordEncoding.ASCII.space()).isEqualTo((byte) 0x20);
        assertThat(RecordEncoding.EBCDIC.encode("A1{")).containsExactly(0xC1, 0xF1, 0xC0);
        assertThat(RecordEncoding.EBCDIC.decode(new byte[] {(byte) 0xD0, (byte) 0xC1}, 0, 2)).isEqualTo("}A");
        assertThatThrownBy(() -> RecordEncoding.of("UTF-8")).isInstanceOf(IllegalArgumentException.class);
        assertThatThrownBy(() -> RecordEncoding.ASCII.encode("\u20ac")).isInstanceOf(RecordFormatException.class);
    }
}
