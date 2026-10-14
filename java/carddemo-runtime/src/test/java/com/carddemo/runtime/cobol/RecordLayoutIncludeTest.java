package com.carddemo.runtime.cobol;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import java.math.BigDecimal;
import org.junit.jupiter.api.Test;

class RecordLayoutIncludeTest {

    private static final RecordLayout INNER;
    private static final NumericField INNER_N;

    static {
        RecordLayout.Builder b = RecordLayout.builder("INNER");
        b.x("A", 2);
        INNER_N = b.packed("N", 3, 0, true);
        INNER = b.build();
    }

    @Test
    void includeShiftsFieldsAndAddsGroupView() {
        RecordLayout.Builder b = RecordLayout.builder("OUTER");
        b.x("PREFIX", 3);
        AlphanumericField group = b.include(INNER);
        b.x("SUFFIX", 1);
        RecordLayout outer = b.build();

        assertThat(outer.length()).isEqualTo(3 + 2 + 2 + 1);
        assertThat(group.offset()).isEqualTo(3);
        assertThat(group.length()).isEqualTo(4);
        assertThat(outer.field("A").offset()).isEqualTo(3);
        NumericField n = (NumericField) outer.field("N");
        assertThat(n.offset()).isEqualTo(5);
        assertThat(n.usage()).isEqualTo(NumericField.Usage.COMP_3);
        assertThat(outer.field("SUFFIX").offset()).isEqualTo(7);

        CobolRecord r = new CobolRecord(outer.length());
        n.set(r, new BigDecimal("-12"));
        assertThat(n.get(r)).isEqualByComparingTo("-12");
        assertThat(outer.describe(r)).containsKeys("PREFIX", "A", "N", "SUFFIX");
        assertThat(INNER_N.offset()).isEqualTo(2);
        // a record shorter than the layout only describes the items it covers
        assertThat(outer.describe(new CobolRecord(5))).containsOnlyKeys("PREFIX", "A");
    }

    @Test
    void duplicateNamesAreRejected() {
        RecordLayout.Builder b = RecordLayout.builder("OUTER");
        b.x("A", 1);
        assertThatThrownBy(() -> b.include(INNER)).isInstanceOf(IllegalArgumentException.class);
    }
}
