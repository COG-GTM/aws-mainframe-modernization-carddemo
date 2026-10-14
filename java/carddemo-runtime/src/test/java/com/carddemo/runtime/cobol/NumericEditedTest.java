package com.carddemo.runtime.cobol;

import static org.assertj.core.api.Assertions.assertThat;

import java.math.BigDecimal;
import org.junit.jupiter.api.Test;

class NumericEditedTest {

    private static final String PIC = "+ZZZ,ZZZ,ZZZ.99";

    @Test
    void fixedPlusSignWithZeroSuppression() {
        assertThat(NumericEdited.format(new BigDecimal("194.00"), PIC)).isEqualTo("+        194.00");
        assertThat(NumericEdited.format(new BigDecimal("2020.00"), PIC)).isEqualTo("+      2,020.00");
        assertThat(NumericEdited.format(new BigDecimal("123456789.12"), PIC)).isEqualTo("+123,456,789.12");
    }

    @Test
    void zeroKeepsDecimalPointAndFraction() {
        assertThat(NumericEdited.format(BigDecimal.ZERO, PIC)).isEqualTo("+           .00");
        assertThat(NumericEdited.format(new BigDecimal("0.05"), PIC)).isEqualTo("+           .05");
    }

    @Test
    void negativeValueShowsMinus() {
        assertThat(NumericEdited.format(new BigDecimal("-1234.56"), PIC)).isEqualTo("-      1,234.56");
        assertThat(NumericEdited.format(new BigDecimal("-5"), "ZZ9-")).isEqualTo("  5-");
        assertThat(NumericEdited.format(new BigDecimal("5"), "ZZ9-")).isEqualTo("  5 ");
    }

    @Test
    void truncatesHighOrderAndLowOrderDigits() {
        // S9(10)V99 into 9 integer positions: the leading digit is lost, as in a COBOL MOVE
        assertThat(NumericEdited.format(new BigDecimal("1234567890.129"), PIC)).isEqualTo("+234,567,890.12");
    }

    @Test
    void allZeroSuppressionBlanksField() {
        assertThat(NumericEdited.format(BigDecimal.ZERO, "ZZZ")).isEqualTo("   ");
    }
}
