package com.carddemo.common.codec;

import static org.assertj.core.api.Assertions.assertThat;

import java.math.BigDecimal;
import org.junit.jupiter.api.Test;

class CobolDisplayTest {

    private static final RecordLayout LAYOUT = Copybook.parse("T", """
                   01  T-REC.
                       05  T-ID      PIC 9(11).
                       05  T-NAME    PIC X(5).
                       05  T-BAL     PIC S9(10)V99.
                       05  T-PACKED  PIC S9(10)V99 COMP-3.
            """).single();

    @Test
    void displaysDigitsWithoutPointAndTrailingSign() {
        FixedWidthRecord rec = FixedWidthRecord.spaces(LAYOUT, RecordEncoding.ASCII);
        rec.setLong("T-ID", 1);
        rec.moveString(rec.field("T-NAME"), "AB");
        rec.setDecimal("T-BAL", new BigDecimal("194.00"));
        rec.setDecimal("T-PACKED", new BigDecimal("-2525.00"));
        assertThat(CobolDisplay.of(rec, "T-ID")).isEqualTo("00000000001");
        assertThat(CobolDisplay.of(rec, "T-NAME")).isEqualTo("AB   ");
        assertThat(CobolDisplay.of(rec, "T-BAL")).isEqualTo("000000019400+");
        assertThat(CobolDisplay.of(rec, "T-PACKED")).isEqualTo("000000252500-");
    }

    @Test
    void numericTruncatesLikeAMoveWithoutRounded() {
        assertThat(CobolDisplay.numeric(new BigDecimal("12.349"), 4, 2, false)).isEqualTo("1234");
        assertThat(CobolDisplay.numeric(new BigDecimal("123.45"), 4, 2, true)).isEqualTo("2345+");
        assertThat(CobolDisplay.numeric(new BigDecimal("-0.019"), 4, 2, true)).isEqualTo("0001-");
    }
}
