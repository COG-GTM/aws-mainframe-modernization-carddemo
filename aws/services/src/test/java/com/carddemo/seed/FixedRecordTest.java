package com.carddemo.seed;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import java.time.LocalDate;
import org.junit.jupiter.api.Test;

class FixedRecordTest {

    @Test
    void decodesOverpunchSigns() {
        FixedRecord r = new FixedRecord("00000001940{0000000123E00000000100}", 35);
        assertThat(r.signed(12, 2)).isEqualByComparingTo("194.00");
        assertThat(r.signed(11, 2)).isEqualByComparingTo("12.35");
        assertThat(r.signed(12, 2)).isEqualByComparingTo("-10.00");
    }

    @Test
    void negativeOverpunch() {
        FixedRecord r = new FixedRecord("0000005047R", 11);
        assertThat(r.signed(11, 2)).isEqualByComparingTo("-504.79");
    }

    @Test
    void padsShortLinesAndStripsCarriageReturn() {
        FixedRecord r = new FixedRecord("01Purchase\r", 60);
        assertThat(r.raw(2)).isEqualTo("01");
        assertThat(r.text(50)).isEqualTo("Purchase");
        assertThat(r.text(8)).isNull();
    }

    @Test
    void parsesDatesAndRejectsLongRecords() {
        assertThat(new FixedRecord("2014-11-20", 10).date(10)).isEqualTo(LocalDate.of(2014, 11, 20));
        assertThat(new FixedRecord("          ", 10).date(10)).isNull();
        assertThatThrownBy(() -> new FixedRecord("12345", 4)).isInstanceOf(IllegalArgumentException.class);
    }
}
