package com.carddemo.common.cobol;

import com.carddemo.common.error.NumericDataException;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class BinaryIntegerTest {

    @Test
    void storageSizeFollowsDigitCount() {
        assertThat(BinaryInteger.byteLength(4)).isEqualTo(2);
        assertThat(BinaryInteger.byteLength(5)).isEqualTo(4);
        assertThat(BinaryInteger.byteLength(9)).isEqualTo(4);
        assertThat(BinaryInteger.byteLength(10)).isEqualTo(8);
        assertThat(BinaryInteger.byteLength(18)).isEqualTo(8);
        assertThatThrownBy(() -> BinaryInteger.byteLength(19)).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void bigEndianTwosComplement() {
        assertThat(BinaryInteger.encode(BigDecimal.ONE.negate(), 4, 0, true)).containsExactly((byte) 0xFF, (byte) 0xFF);
        assertThat(BinaryInteger.encode(new BigDecimal("1234567"), 7, 0, true))
                .containsExactly((byte) 0x00, (byte) 0x12, (byte) 0xD6, (byte) 0x87);
        assertThat(BinaryInteger.encode(new BigDecimal("-12.34"), 7, 2, true))
                .containsExactly((byte) 0xFF, (byte) 0xFF, (byte) 0xFB, (byte) 0x2E);
        assertThat(BinaryInteger.decode(new byte[] {(byte) 0xFF, (byte) 0xFF, (byte) 0xFB, (byte) 0x2E}, 9, 2, true))
                .isEqualTo(new BigDecimal("-12.34"));
    }

    @Test
    void unsignedUsesTheHighBitAsMagnitude() {
        assertThat(BinaryInteger.decode(new byte[] {(byte) 0x27, (byte) 0x0F}, 4, 0, false)).isEqualTo(new BigDecimal("9999"));
        assertThat(BinaryInteger.decode(new byte[] {(byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00,
                (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x07}, 11, 0, false)).isEqualTo(new BigDecimal("7"));
    }

    @Test
    void valueBeyondThePictureIsInvalidData() {
        // X'2710' = 10000 does not fit PIC 9(4) COMP under TRUNC(STD).
        assertThatThrownBy(() -> BinaryInteger.decode(new byte[] {(byte) 0x27, (byte) 0x10}, 4, 0, false))
                .isInstanceOf(NumericDataException.class);
        assertThatThrownBy(() -> BinaryInteger.decode(new byte[] {0, 1, 2}, 4, 0, false))
                .isInstanceOf(IllegalArgumentException.class);
        assertThatThrownBy(() -> BinaryInteger.decode(new byte[] {1}, 0, 2, 4, 0, true))
                .isInstanceOf(IndexOutOfBoundsException.class);
    }

    @Test
    void roundTrips() {
        for (long v : new long[] {0, 1, -1, 9999, -9999, 32767, -32768 + 1}) {
            byte[] encoded = BinaryInteger.encode(BigDecimal.valueOf(v), 4 + (Math.abs(v) > 9999 ? 1 : 0), 0, true);
            assertThat(BinaryInteger.decode(encoded, encoded.length == 2 ? 4 : 5, 0, true)).isEqualTo(BigDecimal.valueOf(v));
        }
    }
}
