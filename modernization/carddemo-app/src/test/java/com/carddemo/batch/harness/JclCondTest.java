package com.carddemo.batch.harness;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import java.util.LinkedHashMap;
import java.util.Map;
import org.junit.jupiter.api.Test;

class JclCondTest {

    private static Map<String, ReturnCode> previous(Object... stepAndRc) {
        Map<String, ReturnCode> map = new LinkedHashMap<>();
        for (int i = 0; i < stepAndRc.length; i += 2) {
            map.put((String) stepAndRc[i], ReturnCode.of((Integer) stepAndRc[i + 1]));
        }
        return map;
    }

    @Test
    void parsesTheJclForms() {
        assertThat(JclCond.parse("(4,LT)").tests()).containsExactly(new JclCond.Test(4, JclCond.Operator.LT, null));
        JclCond two = JclCond.parse("((0,NE,STEP05),(4,LT))");
        assertThat(two.tests()).containsExactly(new JclCond.Test(0, JclCond.Operator.NE, "STEP05"),
                new JclCond.Test(4, JclCond.Operator.LT, null));
        assertThat(two.abendRule()).isEqualTo(JclCond.AbendRule.NORMAL);
        assertThat(JclCond.parse("EVEN").abendRule()).isEqualTo(JclCond.AbendRule.EVEN);
        assertThat(JclCond.parse("((4,LT),EVEN)").abendRule()).isEqualTo(JclCond.AbendRule.EVEN);
        assertThat(JclCond.parse("only").abendRule()).isEqualTo(JclCond.AbendRule.ONLY);
        assertThat(JclCond.parse("")).isEqualTo(JclCond.NONE);
        assertThatThrownBy(() -> JclCond.parse("(4,XX)")).isInstanceOf(IllegalArgumentException.class);
        assertThatThrownBy(() -> JclCond.parse("(4,LT) junk")).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void aStepIsBypassedWhenAnyTestIsTrue() {
        JclCond cond = JclCond.parse("(4,LT)");
        assertThat(cond.bypass(previous("STEP01", 4), false)).isFalse();
        assertThat(cond.bypass(previous("STEP01", 0, "STEP02", 8), false)).isTrue();

        JclCond named = JclCond.parse("(0,NE,STEP01)");
        assertThat(named.bypass(previous("STEP01", 0, "STEP02", 8), false)).isFalse();
        assertThat(named.bypass(previous("STEP01", 4), false)).isTrue();
        assertThat(named.bypass(previous("STEP02", 4), false)).isFalse();

        assertThat(JclCond.parse("(8,LE)").bypass(previous("STEP01", 8), false)).isTrue();
        assertThat(JclCond.parse("(8,GT)").bypass(previous("STEP01", 4), false)).isTrue();
        assertThat(JclCond.parse("(4,EQ)").bypass(previous("STEP01", 4), false)).isTrue();
        assertThat(JclCond.parse("(0,GE)").bypass(previous("STEP01", 4), false)).isFalse();
    }

    @Test
    void abendRules() {
        assertThat(JclCond.NONE.bypass(previous("STEP01", 16), true)).isTrue();
        assertThat(JclCond.NONE.bypass(previous("STEP01", 12), false)).isFalse();
        assertThat(JclCond.parse("EVEN").bypass(previous("STEP01", 16), true)).isFalse();
        assertThat(JclCond.parse("ONLY").bypass(previous("STEP01", 0), false)).isTrue();
        assertThat(JclCond.parse("ONLY").bypass(previous("STEP01", 16), true)).isFalse();
        assertThat(JclCond.parse("((16,EQ),EVEN)").bypass(previous("STEP01", 16), true)).isTrue();
    }
}
