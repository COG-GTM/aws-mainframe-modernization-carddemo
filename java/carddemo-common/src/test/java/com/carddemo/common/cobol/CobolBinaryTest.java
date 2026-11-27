package com.carddemo.common.cobol;

import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class CobolBinaryTest {

    @Test
    void sizesFollowIbmRules() {
        assertThat(CobolBinary.byteLength(4)).isEqualTo(2);
        assertThat(CobolBinary.byteLength(5)).isEqualTo(4);
        assertThat(CobolBinary.byteLength(9)).isEqualTo(4);
        assertThat(CobolBinary.byteLength(10)).isEqualTo(8);
        assertThat(CobolBinary.byteLength(18)).isEqualTo(8);
    }

    @Test
    void signedHalfwordRoundTrip() {
        byte[] b = CobolBinary.encode(BigDecimal.valueOf(-2), 4, 0, true, false); // PIC S9(4) COMP
        assertThat(Hex.of(b)).isEqualTo("FFFE");
        assertThat(CobolBinary.decodeLong(b, 0, 2, true)).isEqualTo(-2);
        assertThat(CobolBinary.decodeLong(b, 0, 2, false)).isEqualTo(65534);
    }

    @Test
    void fullwordWithScale() {
        byte[] b = CobolBinary.encode(new BigDecimal("1234.56"), 9, 2, true, false);
        assertThat(Hex.of(b)).isEqualTo("0001E240");
        assertThat(CobolBinary.decode(b, 0, 4, 2, true)).isEqualByComparingTo("1234.56");
    }

    @Test
    void compTruncatesToPictureButComp5UsesStorage() {
        assertThatThrownBy(() -> CobolBinary.encode(BigDecimal.valueOf(10000), 4, 0, true, false))
                .isInstanceOf(CobolDataException.class);
        assertThat(Hex.of(CobolBinary.encode(BigDecimal.valueOf(32767), 4, 0, true, true))).isEqualTo("7FFF");
        assertThatThrownBy(() -> CobolBinary.encode(BigDecimal.valueOf(32768), 4, 0, true, true))
                .isInstanceOf(CobolDataException.class);
    }
}
