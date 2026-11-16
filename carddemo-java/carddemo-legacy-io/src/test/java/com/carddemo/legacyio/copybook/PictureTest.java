package com.carddemo.legacyio.copybook;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class PictureTest {

    @ParameterizedTest
    @CsvSource({
        "X(10),     ALPHANUMERIC, 10, 0, false",
        "X,         ALPHANUMERIC, 1,  0, false",
        "A(3)X(2),  ALPHANUMERIC, 5,  0, false",
        "9(04),     NUMERIC,      4,  0, false",
        "9(11),     NUMERIC,      11, 0, false",
        "S9(10)V99, NUMERIC,      12, 2, true",
        "S9(04)V99, NUMERIC,      6,  2, true",
        "9V9(4),    NUMERIC,      5,  4, false",
        "ZZ9.99-,   EDITED,       7,  0, false",
        "'-ZZZ,ZZZ,ZZ9.99', EDITED, 15, 0, false"
    })
    void parsesPictures(String source, Picture.Category category, int size, int scale, boolean signed) {
        Picture picture = Picture.parse(source);

        assertThat(picture.category()).isEqualTo(category);
        assertThat(picture.size()).isEqualTo(size);
        assertThat(picture.scale()).isEqualTo(scale);
        assertThat(picture.signed()).isEqualTo(signed);
    }

    @ParameterizedTest
    @CsvSource({
        "S9(10)V99, DISPLAY,        12",
        "S9(10)V99, PACKED_DECIMAL, 7",
        "S9(09)V99, PACKED_DECIMAL, 6",
        "9(01),     PACKED_DECIMAL, 1",
        "S9(04),    BINARY,         2",
        "9(09),     BINARY,         4",
        "S9(10),    BINARY,         8",
        "9(18),     BINARY,         8"
    })
    void storageLengths(String source, Usage usage, int bytes) {
        assertThat(Picture.parse(source).byteLength(usage)).isEqualTo(bytes);
    }

    @Test
    void rejectsComputationalAlphanumerics() {
        assertThatThrownBy(() -> Picture.parse("X(4)").byteLength(Usage.PACKED_DECIMAL))
                .isInstanceOf(CopybookParseException.class);
    }

    @Test
    void rejectsMalformedPictures() {
        assertThatThrownBy(() -> Picture.parse("9(4")).isInstanceOf(CopybookParseException.class);
        assertThatThrownBy(() -> Picture.parse("Q")).isInstanceOf(CopybookParseException.class);
    }
}
