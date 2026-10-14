package com.carddemo.runtime.bms;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

class BmsColorTest {

    @Test
    void colorByteOverridesMapColor() {
        SymbolicMap m = SymbolicMap.of("COMEN01", "COMEN1A");
        m.setData("ERRMSG", "x");
        assertThat(m.render(true, false, false, false).field("ERRMSG").orElseThrow().color()).isEqualTo("RED");
        m.setColor("ERRMSG", Dfhbmsca.DFHGREEN);
        assertThat(m.render(true, false, false, false).field("ERRMSG").orElseThrow().color()).isEqualTo("GREEN");
        m.setColor("ERRMSG", Dfhbmsca.DFHDFCOL);
        assertThat(m.render(true, false, false, false).field("ERRMSG").orElseThrow().color()).isEqualTo("RED");
        m.setColor("ERRMSG", Dfhbmsca.DFHGREEN);
        m.lowValues();
        assertThat(m.render(true, false, false, false).field("ERRMSG").orElseThrow().color()).isEqualTo("RED");
        assertThat(Dfhbmsca.colorName(0xF5)).isEqualTo("TURQUOISE");
    }
}
