package com.carddemo.batch.record;

import static org.assertj.core.api.Assertions.assertThat;

import com.carddemo.batch.intcalc.CalculateInterestJob;
import java.math.BigDecimal;
import org.junit.jupiter.api.Test;

class CobolArithmeticTest {

    @Test
    void monthlyInterestTruncatesLikeComputeWithoutRounded() {
        // 1234.56 * 15.00 / 1200 = 15.432 -> 15.43 ; 999.99 * 25.00 / 1200 = 20.833125 -> 20.83
        assertThat(CalculateInterestJob.monthlyInterest(new BigDecimal("1234.56"), new BigDecimal("15.00")))
                .isEqualByComparingTo("15.43");
        assertThat(CalculateInterestJob.monthlyInterest(new BigDecimal("999.99"), new BigDecimal("25.00")))
                .isEqualByComparingTo("20.83");
        // negative balances truncate toward zero: -10.00 * 15 / 1200 = -0.125 -> -0.12
        assertThat(CalculateInterestJob.monthlyInterest(new BigDecimal("-10.00"), new BigDecimal("15.00")))
                .isEqualByComparingTo("-0.12");
    }

    @Test
    void fitDropsHighOrderDigitsLikeMoveToSmallerPicture() {
        assertThat(Cobol.fit(new BigDecimal("12345678901.239"), 9, 2)).isEqualByComparingTo("345678901.23");
    }

    @Test
    void editedPictures() {
        assertThat(Edited.format(new BigDecimal("1940.00"), "999999999.99-")).isEqualTo("000001940.00 ");
        assertThat(Edited.format(new BigDecimal("-12.5"), "999999999.99-")).isEqualTo("000000012.50-");
        assertThat(Edited.format(new BigDecimal("50.47"), "ZZZZZZZZZ.99-")).isEqualTo("       50.47 ");
        assertThat(Edited.format(new BigDecimal("0.05"), "ZZZZZZZZZ.99-")).isEqualTo("         .05 ");
        assertThat(Edited.format(new BigDecimal("1234567.8"), "-ZZZ,ZZZ,ZZZ.ZZ")).isEqualTo("   1,234,567.80");
        assertThat(Edited.format(new BigDecimal("-91.2"), "-ZZZ,ZZZ,ZZZ.ZZ")).isEqualTo("-" + " ".repeat(9) + "91.20");
        assertThat(Edited.format(BigDecimal.ZERO, "-ZZZ,ZZZ,ZZZ.ZZ")).isEqualTo(" ".repeat(15));
        assertThat(Edited.format(new BigDecimal("12.34"), "+ZZZ,ZZZ,ZZZ.ZZ")).isEqualTo("+" + " ".repeat(9) + "12.34");
    }
}
