package com.carddemo.common.batch;

import org.junit.jupiter.api.Test;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class JclCondTest {

    @Test
    void zeroNeBypassesAfterAnyNonZeroReturnCode() {
        JclCond cond = JclCond.parse("(0,NE)");

        assertThat(cond).isEqualTo(JclCond.ALL_ZERO);
        assertThat(cond.bypass(List.of())).isFalse();
        assertThat(cond.bypass(List.of(0, 0))).isFalse();
        assertThat(cond.bypass(List.of(0, 4))).isTrue();
    }

    @Test
    void fourLtBypassesOnlyAboveFour() {
        // TRANBKP STEP10: COND=(4,LT)
        JclCond cond = JclCond.parse("4,LT");

        assertThat(cond.bypass(List.of(4))).isFalse();
        assertThat(cond.bypass(List.of(8))).isTrue();
    }

    @Test
    void rejectsNonCondText() {
        assertThatThrownBy(() -> JclCond.parse("(0,XX)")).isInstanceOf(IllegalArgumentException.class);
        assertThatThrownBy(() -> JclCond.parse("EVEN")).isInstanceOf(IllegalArgumentException.class);
    }
}
