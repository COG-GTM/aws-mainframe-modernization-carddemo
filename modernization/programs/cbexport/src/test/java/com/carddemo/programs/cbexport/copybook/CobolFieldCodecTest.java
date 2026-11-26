package com.carddemo.programs.cbexport.copybook;

import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.math.BigDecimal;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class CobolFieldCodecTest {

    private static byte[] bytes(int... v) {
        byte[] b = new byte[v.length];
        for (int i = 0; i < v.length; i++) {
            b[i] = (byte) v[i];
        }
        return b;
    }

    @Test
    void alphanumericMoveLeftJustifiesSpaceFillsAndTruncatesOnTheRight() {
        byte[] b = new byte[5];
        CobolFieldCodec.putAlnum(b, 0, 5, "AB");
        assertEquals("AB   ", CobolFieldCodec.getAlnum(b, 0, 5));
        CobolFieldCodec.putAlnum(b, 0, 5, "ABCDEFG");
        assertEquals("ABCDE", CobolFieldCodec.getAlnum(b, 0, 5));
        CobolFieldCodec.putAlnum(b, 0, 5, null);
        assertEquals("     ", CobolFieldCodec.getAlnum(b, 0, 5));
    }

    @Test
    void unsignedZonedMoveTruncatesHighOrderDigitsAndDropsSign() {
        byte[] b = new byte[3];
        CobolFieldCodec.putUnsignedZoned(b, 0, 3, 12345);
        assertEquals("345", CobolFieldCodec.getAlnum(b, 0, 3));
        CobolFieldCodec.putUnsignedZoned(b, 0, 3, -7);
        assertEquals("007", CobolFieldCodec.getAlnum(b, 0, 3));
    }

    @Test
    void unsignedZonedDecodeTreatsSpacesAsZeroLikeTheLowNibbleMove() {
        byte[] b = "  12".getBytes(CobolFieldCodec.CHARSET);
        assertEquals(12, CobolFieldCodec.getUnsignedZoned(b, 0, 4));
    }

    @ParameterizedTest
    @CsvSource({
        "00000001940{, 194.00",
        "00000000000{, 0.00",
        "0000009190}, -919.00",
        "0000005047G, 504.77",
        "00000000001J, -0.11",
        "000000000123, 1.23"
    })
    void signedZonedOverpunchDecode(String image, String expected) {
        byte[] b = image.getBytes(CobolFieldCodec.CHARSET);
        assertEquals(new BigDecimal(expected), CobolFieldCodec.getSignedZoned(b, 0, b.length, 2));
    }

    @ParameterizedTest
    @CsvSource({
        "194.00, 00000001940{",
        "-919.00, 00000009190}",
        "504.77, 00000005047G",
        "-0.11, 00000000001J",
        "1.239, 00000000012C"
    })
    void signedZonedOverpunchEncodeTruncatesExcessDecimals(String value, String expected) {
        byte[] b = new byte[12];
        CobolFieldCodec.putSignedZoned(b, 0, 12, 2, new BigDecimal(value));
        assertEquals(expected, CobolFieldCodec.getAlnum(b, 0, 12));
    }

    @Test
    void packedSignedS9_10V99UsesSevenBytesWithCOrDSign() {
        byte[] b = new byte[7];
        CobolFieldCodec.putPacked(b, 0, 12, 2, true, new BigDecimal("194.00"));
        assertArrayEquals(bytes(0x00, 0x00, 0x00, 0x00, 0x19, 0x40, 0x0C), b);
        CobolFieldCodec.putPacked(b, 0, 12, 2, true, new BigDecimal("-12345678901.23"));
        // 13-digit value into 12 digits: high-order digit truncated
        assertArrayEquals(bytes(0x02, 0x34, 0x56, 0x78, 0x90, 0x12, 0x3D), b);
        assertEquals(new BigDecimal("-2345678901.23"), CobolFieldCodec.getPacked(b, 0, 12, 2));
    }

    @Test
    void packedUnsigned9_03UsesFSignNibble() {
        byte[] b = new byte[2];
        CobolFieldCodec.putPacked(b, 0, 3, 0, false, BigDecimal.valueOf(274));
        assertArrayEquals(bytes(0x27, 0x4F), b);
        assertEquals(BigDecimal.valueOf(274), CobolFieldCodec.getPacked(b, 0, 3, 0));
    }

    @Test
    void packedS9_09V99UsesSixBytes() {
        assertEquals(6, CobolFieldCodec.packedLength(11));
        byte[] b = new byte[6];
        CobolFieldCodec.putPacked(b, 0, 11, 2, true, new BigDecimal("-919.00"));
        assertArrayEquals(bytes(0x00, 0x00, 0x00, 0x91, 0x90, 0x0D), b);
    }

    @Test
    void binarySizesFollowPicDigits() {
        assertEquals(2, CobolFieldCodec.binaryLength(3));
        assertEquals(4, CobolFieldCodec.binaryLength(9));
        assertEquals(8, CobolFieldCodec.binaryLength(11));
        assertEquals(8, CobolFieldCodec.binaryLength(12));
    }

    @Test
    void binaryIsBigEndianAndTruncatedToPicDigits() {
        byte[] b = new byte[4];
        CobolFieldCodec.putBinary(b, 0, 9, false, 500);
        assertArrayEquals(bytes(0x00, 0x00, 0x01, 0xF4), b);
        CobolFieldCodec.putBinary(b, 0, 9, false, 1_000_000_001L);
        assertEquals(1, CobolFieldCodec.getBinary(b, 0, 9, false));

        byte[] s = new byte[8];
        CobolFieldCodec.putBinary(s, 0, 12, true, -150);
        assertEquals(-150, CobolFieldCodec.getBinary(s, 0, 12, true));
        assertEquals((byte) 0xFF, s[0]);
    }
}
