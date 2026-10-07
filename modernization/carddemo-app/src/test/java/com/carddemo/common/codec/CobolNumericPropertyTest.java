package com.carddemo.common.codec;

import net.jqwik.api.Arbitraries;
import net.jqwik.api.Arbitrary;
import net.jqwik.api.Combinators;
import net.jqwik.api.ForAll;
import net.jqwik.api.Property;
import net.jqwik.api.Provide;
import net.jqwik.api.constraints.IntRange;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

import static org.assertj.core.api.Assertions.assertThat;

class CobolNumericPropertyTest {

    /** A PIC [S]9(digits)V9(scale) and a value that fits it. */
    record Spec(int digits, int scale, boolean signed, BigDecimal value) {
    }

    @Provide
    Arbitrary<Spec> fittingValues() {
        return Combinators.combine(Arbitraries.integers().between(1, 18), Arbitraries.integers().between(0, 18),
                        Arbitraries.of(true, false), Arbitraries.longs(), Arbitraries.of(true, false))
                .as((digits, scaleSeed, signed, seed, negative) -> {
                    int scale = scaleSeed % (digits + 1);
                    BigInteger unscaled = BigInteger.valueOf(seed).abs().mod(BigInteger.TEN.pow(digits));
                    if (signed && negative) {
                        unscaled = unscaled.negate();
                    }
                    return new Spec(digits, scale, signed, new BigDecimal(unscaled, scale));
                });
    }

    @Property
    void zonedRoundTripsFittingValues(@ForAll("fittingValues") Spec s) {
        String text = CobolNumeric.encodeZoned(s.value(), s.digits(), s.scale(), s.signed());
        assertThat(text).hasSize(s.digits());
        assertThat(CobolNumeric.decodeZoned(text, s.scale(), s.signed())).isEqualTo(s.value());
    }

    @Property
    void packedRoundTripsFittingValues(@ForAll("fittingValues") Spec s) {
        byte[] image = new byte[CobolNumeric.packedLength(s.digits())];
        CobolNumeric.encodePacked(image, 0, image.length, s.digits(), s.scale(), s.signed(), s.value());
        int sign = image[image.length - 1] & 0x0F;
        assertThat(sign).isEqualTo(!s.signed() ? 0x0F : s.value().signum() < 0 ? 0x0D : 0x0C);
        assertThat(CobolNumeric.decodePacked(image, 0, image.length, s.scale())).isEqualTo(s.value());
    }

    @Property
    void binaryRoundTripsFittingValues(@ForAll("fittingValues") Spec s) {
        byte[] image = new byte[CobolNumeric.binaryLength(s.digits())];
        CobolNumeric.encodeBinary(image, 0, image.length, s.digits(), s.scale(), s.signed(), s.value());
        assertThat(CobolNumeric.decodeBinary(image, 0, image.length, s.scale(), s.signed())).isEqualTo(s.value());
    }

    @Property
    void storingTruncatesTowardZeroAndDropsHighOrderDigits(@ForAll("fittingValues") Spec s,
                                                           @ForAll @IntRange(min = 0, max = 999) int extraFraction,
                                                           @ForAll @IntRange(min = 1, max = 9) int extraHigh) {
        BigDecimal noisy = s.value()
                .add(new BigDecimal(BigInteger.valueOf(extraFraction), s.scale() + 3).multiply(
                        BigDecimal.valueOf(s.value().signum() < 0 ? -1 : 1)))
                .add(BigDecimal.valueOf(extraHigh).movePointRight(s.digits() - s.scale())
                        .multiply(BigDecimal.valueOf(s.value().signum() < 0 ? -1 : 1)));
        BigDecimal expected = s.value().signum() == 0 && !s.signed() ? s.value() : s.value();
        String zoned = CobolNumeric.encodeZoned(noisy, s.digits(), s.scale(), s.signed());
        assertThat(CobolNumeric.decodeZoned(zoned, s.scale(), s.signed())).isEqualTo(expected);
        byte[] packed = new byte[CobolNumeric.packedLength(s.digits())];
        CobolNumeric.encodePacked(packed, 0, packed.length, s.digits(), s.scale(), s.signed(), noisy);
        assertThat(CobolNumeric.decodePacked(packed, 0, packed.length, s.scale())).isEqualTo(expected);
        assertThat(CobolNumeric.truncate(noisy, s.digits(), s.scale(), s.signed())).isEqualTo(expected);
    }

    @Property
    void negativeZeroIsStoredAsPositiveZero(@ForAll @IntRange(min = 1, max = 18) int digits,
                                            @ForAll @IntRange(min = 0, max = 18) int scaleSeed) {
        int scale = scaleSeed % (digits + 1);
        BigDecimal negativeZero = new BigDecimal("-0." + "0".repeat(Math.max(scale, 1)));
        String zoned = CobolNumeric.encodeZoned(negativeZero, digits, scale, true);
        assertThat(zoned).isEqualTo("0".repeat(digits - 1) + "{");
        byte[] packed = new byte[CobolNumeric.packedLength(digits)];
        CobolNumeric.encodePacked(packed, 0, packed.length, digits, scale, true, negativeZero);
        assertThat(packed[packed.length - 1] & 0x0F).isEqualTo(0x0C);
        packed[packed.length - 1] = (byte) ((packed[packed.length - 1] & 0xF0) | 0x0D);
        BigDecimal decoded = CobolNumeric.decodePacked(packed, 0, packed.length, scale);
        assertThat(decoded.signum()).isZero();
        assertThat(decoded.scale()).isEqualTo(scale);
        assertThat(CobolNumeric.decodeZoned("0".repeat(digits - 1) + "}", scale, true).signum()).isZero();
    }

    @Property
    void maxScaleValuesRoundTrip(@ForAll @IntRange(min = 0, max = 999_999_999) int seed,
                                 @ForAll boolean negative) {
        BigDecimal value = new BigDecimal(BigInteger.valueOf(seed).multiply(BigInteger.valueOf(999_999_999L)), 18);
        if (negative) {
            value = value.negate();
        }
        assertThat(CobolNumeric.decodeZoned(CobolNumeric.encodeZoned(value, 18, 18, true), 18, true))
                .isEqualTo(value);
        byte[] packed = new byte[10];
        CobolNumeric.encodePacked(packed, 0, 10, 18, 18, true, value);
        assertThat(CobolNumeric.decodePacked(packed, 0, 10, 18)).isEqualTo(value);
        byte[] binary = new byte[8];
        CobolNumeric.encodeBinary(binary, 0, 8, 18, 18, true, value);
        assertThat(CobolNumeric.decodeBinary(binary, 0, 8, 18, true)).isEqualTo(value);
        assertThat(value.setScale(18, RoundingMode.DOWN)).isEqualTo(value);
    }

    @Property
    void signNibblesCAndFArePositiveDIsNegative(@ForAll @IntRange(min = 0, max = 99_999) int magnitude,
                                                @ForAll @IntRange(min = 0, max = 5) int scale) {
        byte[] image = new byte[3];
        CobolNumeric.encodePacked(image, 0, 3, 5, scale, true, new BigDecimal(BigInteger.valueOf(magnitude), scale));
        BigDecimal positive = new BigDecimal(BigInteger.valueOf(magnitude), scale);
        for (int nibble : new int[] {0x0C, 0x0F, 0x0D}) {
            image[2] = (byte) ((image[2] & 0xF0) | nibble);
            BigDecimal decoded = CobolNumeric.decodePacked(image, 0, 3, scale);
            assertThat(decoded).isEqualTo(nibble == 0x0D ? positive.negate() : positive);
        }
    }
}
