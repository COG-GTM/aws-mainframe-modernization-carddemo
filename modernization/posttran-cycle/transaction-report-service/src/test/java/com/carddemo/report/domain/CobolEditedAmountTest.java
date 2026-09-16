package com.carddemo.report.domain;

import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThat;

/** CVTRA07Y numeric-edited pictures -ZZZ,ZZZ,ZZZ.ZZ and +ZZZ,ZZZ,ZZZ.ZZ. */
class CobolEditedAmountTest {

    @Test
    void minusPictureShowsSpaceForPositiveAndFloatingMinusForNegative() {
        assertThat(CobolEditedAmount.minusEdited(new BigDecimal("504.77"))).isEqualTo("         504.77");
        assertThat(CobolEditedAmount.minusEdited(new BigDecimal("-504.77"))).isEqualTo("        -504.77");
        assertThat(CobolEditedAmount.minusEdited(new BigDecimal("1234567.89"))).isEqualTo("   1,234,567.89");
    }

    @Test
    void plusPictureShowsFloatingPlusOrMinus() {
        assertThat(CobolEditedAmount.plusEdited(new BigDecimal("12.00"))).isEqualTo("         +12.00");
        assertThat(CobolEditedAmount.plusEdited(new BigDecimal("-999999999.99"))).isEqualTo("-999,999,999.99");
    }

    @Test
    void everyResultIsFifteenCharacters() {
        for (String v : new String[] {"0", "0.01", "-0.01", "999999999.99", "100000.00"}) {
            assertThat(CobolEditedAmount.plusEdited(new BigDecimal(v))).hasSize(15);
            assertThat(CobolEditedAmount.minusEdited(new BigDecimal(v))).hasSize(15);
        }
    }

    @Test
    void zeroIsAllSpacesAndFractionsOnlyKeepThePoint() {
        assertThat(CobolEditedAmount.plusEdited(BigDecimal.ZERO)).isEqualTo(" ".repeat(15));
        assertThat(CobolEditedAmount.minusEdited(new BigDecimal("0.05"))).isEqualTo("            .05");
        assertThat(CobolEditedAmount.minusEdited(new BigDecimal("-0.05"))).isEqualTo("           -.05");
    }

    @Test
    void extraDecimalsAreTruncated() {
        assertThat(CobolEditedAmount.minusEdited(new BigDecimal("1.999"))).isEqualTo("           1.99");
    }
}
