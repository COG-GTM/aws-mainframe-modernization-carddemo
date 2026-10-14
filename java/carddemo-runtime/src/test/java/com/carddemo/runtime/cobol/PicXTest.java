package com.carddemo.runtime.cobol;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

class PicXTest {

    @Test
    void padsWithSpacesAndTruncatesOnTheRight() {
        assertThat(PicX.of("AB", 5)).isEqualTo("AB   ");
        assertThat(PicX.of("ABCDEFG", 3)).isEqualTo("ABC");
        assertThat(PicX.of(null, 2)).isEqualTo("  ");
        assertThat(PicX.justifiedRight("AB", 4)).isEqualTo("  AB");
        assertThat(PicX.justifiedRight("ABCDE", 3)).isEqualTo("CDE");
    }

    @Test
    void figurativeConstants() {
        assertThat(PicX.isSpaces("   ")).isTrue();
        assertThat(PicX.isLowValues(PicX.lowValues(3))).isTrue();
        assertThat(PicX.isHighValues(PicX.highValues(2))).isTrue();
        assertThat(PicX.isSpacesOrLowValues(" \u0000")).isFalse();
        assertThat(PicX.isSpacesOrLowValues("\u0000\u0000")).isTrue();
    }

    @Test
    void upperCaseOnlyConvertsAsciiLetters() {
        assertThat(PicX.upperCase("ab1é\u0000z")).isEqualTo("AB1é\u0000Z");
        assertThat(PicX.lowerCase("AB1Z")).isEqualTo("ab1z");
    }

    @Test
    void comparisonPadsShorterOperandWithSpaces() {
        assertThat(PicX.equal("AB", "AB   ")).isTrue();
        assertThat(PicX.compare("AB", "AC")).isNegative();
        assertThat(PicX.compare("A\u0000", "A ")).isNegative();
    }

    @Test
    void bytesRoundTripIso88591() {
        String s = "A\u0000\u00FF ";
        assertThat(PicX.fromBytes(PicX.toBytes(s))).isEqualTo(s);
        assertThat(PicX.rtrim("AB  ")).isEqualTo("AB");
    }
}
