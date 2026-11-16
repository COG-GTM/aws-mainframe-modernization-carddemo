package com.carddemo.legacyio;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import org.junit.jupiter.api.Test;

class PictureTest {

    @Test
    void parsesAlphanumeric() {
        Picture picture = Picture.parse("X(26)");
        assertThat(picture.category()).isEqualTo(Picture.Category.ALPHANUMERIC);
        assertThat(picture.byteLength(Usage.DISPLAY)).isEqualTo(26);
    }

    @Test
    void parsesSignedDecimalWithImpliedPoint() {
        Picture picture = Picture.parse("S9(10)V99");
        assertThat(picture.isNumeric()).isTrue();
        assertThat(picture.signed()).isTrue();
        assertThat(picture.digits()).isEqualTo(12);
        assertThat(picture.scale()).isEqualTo(2);
        assertThat(picture.byteLength(Usage.DISPLAY)).isEqualTo(12);
        assertThat(picture.byteLength(Usage.PACKED_DECIMAL)).isEqualTo(7);
        assertThat(picture.byteLength(Usage.BINARY)).isEqualTo(8);
    }

    @Test
    void sizesBinaryByDigitCount() {
        assertThat(Picture.parse("9(4)").byteLength(Usage.BINARY)).isEqualTo(2);
        assertThat(Picture.parse("9(9)").byteLength(Usage.BINARY)).isEqualTo(4);
        assertThat(Picture.parse("9(11)").byteLength(Usage.BINARY)).isEqualTo(8);
    }

    @Test
    void sizesPackedDecimal() {
        assertThat(Picture.parse("9(03)").byteLength(Usage.PACKED_DECIMAL)).isEqualTo(2);
        assertThat(Picture.parse("S9(09)V99").byteLength(Usage.PACKED_DECIMAL)).isEqualTo(6);
    }

    @Test
    void parsesEditedPictureAsText() {
        Picture picture = Picture.parse("-ZZZ,ZZZ,ZZ9.99");
        assertThat(picture.category()).isEqualTo(Picture.Category.EDITED);
        assertThat(picture.byteLength(Usage.DISPLAY)).isEqualTo(15);
    }

    @Test
    void rejectsScalingPositions() {
        assertThatThrownBy(() -> Picture.parse("9(3)PP")).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void rejectsMoreThanEighteenDigits() {
        assertThatThrownBy(() -> Picture.parse("9(19)")).isInstanceOf(IllegalArgumentException.class);
    }
}
