package com.carddemo.runtime.cobol;

import static org.assertj.core.api.Assertions.assertThat;

import java.math.BigDecimal;
import java.time.Clock;
import java.time.Instant;
import java.time.ZoneOffset;
import java.util.Map;
import org.junit.jupiter.api.Test;

class RecordLayoutTest {

    enum Status implements CobolCode {
        ACTIVE("Y"), INACTIVE("N");

        private final String code;

        Status(String code) {
            this.code = code;
        }

        @Override
        public String code() {
            return code;
        }
    }

    @Test
    void sequentialFieldsGroupsFillerAndRedefines() {
        RecordLayout.Builder b = RecordLayout.builder("REC");
        b.group("KEY-GRP");
        AlphanumericField id = b.x("ID", 4);
        NumericField seq = b.zoned("SEQ", 2, 0, false);
        AlphanumericField keyGroup = b.endGroup();
        b.filler(1);
        NumericField amt = b.packed("AMT", 7, 2, true);
        b.redefines(amt);
        AlphanumericField amtX = b.x("AMT-X", 5);
        b.endRedefines(amt);
        NumericField cnt = b.binary("CNT", 4, 0, true);
        RecordLayout layout = b.build();

        assertThat(layout.length()).isEqualTo(4 + 2 + 1 + 5 + 2);
        assertThat(amt.offset()).isEqualTo(7);
        assertThat(amtX.offset()).isEqualTo(7);
        assertThat(cnt.offset()).isEqualTo(12);

        CobolRecord r = new CobolRecord(layout.length());
        assertThat(id.isLowValues(r)).isTrue();
        id.set(r, "AB");
        seq.set(r, 123);
        amt.set(r, new BigDecimal("-1.239"));
        cnt.set(r, 7);
        assertThat(id.get(r)).isEqualTo("AB  ");
        assertThat(seq.get(r)).isEqualByComparingTo("23");
        assertThat(keyGroup.get(r)).isEqualTo("AB  23");
        assertThat(amt.get(r)).isEqualByComparingTo("-1.23");
        assertThat(amtX.rawBytes(r)).containsExactly(0x00, 0x00, 0x00, 0x12, 0x3D);
        assertThat(cnt.getLong(r)).isEqualTo(7);
        assertThat(layout.field("SEQ")).isSameAs(seq);
        Map<String, Object> described = layout.describe(r);
        assertThat(described).containsEntry("ID", "AB  ");
        amt.setRounded(r, new BigDecimal("1.235"));
        assertThat(amt.get(r)).isEqualByComparingTo("1.24");
    }

    @Test
    void groupMovePadsAndTruncates() {
        CobolRecord r = new CobolRecord(4);
        r.move("ABCDEF");
        assertThat(r.asString()).isEqualTo("ABCD");
        r.move("A");
        assertThat(r.asString()).isEqualTo("A   ");
        r.initializeLowValues();
        assertThat(PicX.isLowValues(r.asString())).isTrue();
    }

    @Test
    void level88Conditions() {
        RecordLayout.Builder b = RecordLayout.builder("R");
        AlphanumericField type = b.x("TYPE", 1);
        NumericField ctx = b.zoned("CTX", 1, 0, false);
        b.build();
        Condition88 admin = Condition88.of("ADMIN", type, "A");
        Condition88 enter = Condition88.of("ENTER", ctx, 0);
        Condition88 low = Condition88.range("LOW", ctx, BigDecimal.ZERO, BigDecimal.valueOf(4));
        CobolRecord r = new CobolRecord(2);
        assertThat(admin.isTrue(r)).isFalse();
        assertThat(enter.isTrue(r)).isFalse();
        admin.set(r);
        enter.set(r);
        assertThat(type.get(r)).isEqualTo("A");
        assertThat(admin.isTrue(r)).isTrue();
        assertThat(enter.isTrue(r)).isTrue();
        assertThat(low.isTrue(r)).isTrue();
        assertThat(CobolCode.fromCode(Status.class, "N ")).contains(Status.INACTIVE);
        assertThat(CobolCode.fromCode(Status.class, "X")).isEmpty();
    }

    @Test
    void currentDateShape() {
        Clock clock = Clock.fixed(Instant.parse("2024-03-05T07:08:09.123Z"), ZoneOffset.ofHours(-5));
        assertThat(CobolIntrinsics.currentDate(clock)).isEqualTo("2024030502080912-0500");
    }
}
