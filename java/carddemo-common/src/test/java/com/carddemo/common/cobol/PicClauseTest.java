package com.carddemo.common.cobol;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class PicClauseTest {

    @Test
    void numeric() {
        PicClause p = PicClause.parse("S9(10)V99");
        assertThat(p.category()).isEqualTo(PicClause.Category.NUMERIC);
        assertThat(p.digits()).isEqualTo(12);
        assertThat(p.scale()).isEqualTo(2);
        assertThat(p.signed()).isTrue();
        assertThat(p.displayLength()).isEqualTo(12);
        assertThat(PicClause.parse("9(02)V9(04)").scale()).isEqualTo(4);
        assertThat(PicClause.parse("99").digits()).isEqualTo(2);
    }

    @Test
    void alphanumeric() {
        assertThat(PicClause.parse("X(50)").displayLength()).isEqualTo(50);
        assertThat(PicClause.parse("XXX").displayLength()).isEqualTo(3);
        assertThat(PicClause.parse("A(2)").isAlphanumeric()).isTrue();
    }

    @Test
    void edited() {
        PicClause p = PicClause.parse("-ZZZ,ZZZ,ZZZ.ZZ");
        assertThat(p.isEdited()).isTrue();
        assertThat(p.displayLength()).isEqualTo(15);
        assertThat(p.digits()).isEqualTo(11);
        assertThat(p.scale()).isEqualTo(2);
        assertThat(PicClause.parse("----9").digits()).isEqualTo(4);
        assertThat(PicClause.parse("$$$,$$9.99").digits()).isEqualTo(7);
        assertThat(PicClause.parse("ZZ9.99CR").displayLength()).isEqualTo(8);
    }

    @Test
    void rejectsUnsupported() {
        assertThatThrownBy(() -> PicClause.parse("99PPP")).isInstanceOf(CobolDataException.class);
        assertThatThrownBy(() -> PicClause.parse("Q(3)")).isInstanceOf(CobolDataException.class);
    }
}
