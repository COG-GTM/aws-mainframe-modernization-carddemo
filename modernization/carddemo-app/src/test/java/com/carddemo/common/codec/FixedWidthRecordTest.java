package com.carddemo.common.codec;

import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class FixedWidthRecordTest {

    private static final RecordLayout LAYOUT = Copybook.parse("T", String.join("\n",
            "       01  T-REC.",
            "           05  T-NAME        PIC X(05).",
            "           05  T-AMT         PIC S9(3)V99.",
            "           05  T-UAMT        PIC 9(3)V99 COMP-3.",
            "           05  T-COUNT       PIC S9(4) COMP.",
            "           05  T-EDIT        PIC -ZZ9.99.",
            "           05  T-ID          PIC 9(04).",
            "           05  FILLER        PIC X(02).")).single();

    @Test
    void textIsPaddedTrimmedAndTruncatedOnMove() {
        FixedWidthRecord r = FixedWidthRecord.spaces(LAYOUT, RecordEncoding.EBCDIC);
        r.setString("T-NAME", "AB");
        assertThat(r.getString("T-NAME")).isEqualTo("AB   ");
        assertThat(r.getTrimmed(r.field("T-NAME"))).isEqualTo("AB");
        assertThat(r.get("T-NAME")).isEqualTo("AB   ");
        assertThatThrownBy(() -> r.setString("T-NAME", "ABCDEF")).isInstanceOf(RecordFormatException.class);
        r.moveString(r.field("T-NAME"), "ABCDEFG");
        assertThat(r.getString("T-NAME")).isEqualTo("ABCDE");
        assertThat(r.bytes()[0]).isEqualTo((byte) 0xC1);
        assertThatThrownBy(() -> r.moveString(r.field("T-AMT"), "1")).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void numbersInEveryUsage() {
        FixedWidthRecord r = FixedWidthRecord.spaces(LAYOUT, RecordEncoding.ASCII);
        r.setDecimal("T-AMT", new BigDecimal("-12.349"));
        assertThat(r.getDecimal("T-AMT")).isEqualTo(new BigDecimal("-12.34"));
        assertThat(r.getString("T-AMT")).isEqualTo("0123M");
        r.setDecimal("T-UAMT", new BigDecimal("999.99"));
        assertThat(r.getDecimal("T-UAMT")).isEqualTo(new BigDecimal("999.99"));
        r.setLong("T-COUNT", -42);
        assertThat(r.getLong("T-COUNT")).isEqualTo(-42);
        r.set("T-ID", 7);
        assertThat(r.getLong("T-ID")).isEqualTo(7);
        assertThat(r.get("T-ID")).isEqualTo(new BigDecimal("7"));
        r.set(r.field("T-ID"), 8L);
        assertThat(r.getString("T-ID")).isEqualTo("0008");
        assertThatThrownBy(() -> r.getLong("T-AMT")).isInstanceOf(IllegalArgumentException.class);
        assertThatThrownBy(() -> r.getDecimal("T-NAME")).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void setRejectsLossOfHighOrderDigitsOrSignButMoveTruncates() {
        FixedWidthRecord r = FixedWidthRecord.spaces(LAYOUT, RecordEncoding.ASCII);
        assertThatThrownBy(() -> r.setDecimal("T-AMT", new BigDecimal("1000"))).isInstanceOf(
                RecordFormatException.class).hasMessageContaining("PIC S999V99");
        assertThatThrownBy(() -> r.setDecimal("T-UAMT", new BigDecimal("-1")))
                .isInstanceOf(RecordFormatException.class).hasMessageContaining("unsigned");
        r.moveDecimal(r.field("T-AMT"), new BigDecimal("12345.678"));
        assertThat(r.getDecimal("T-AMT")).isEqualTo(new BigDecimal("345.67"));
        r.moveDecimal(r.field("T-UAMT"), new BigDecimal("-1.5"));
        assertThat(r.getDecimal("T-UAMT")).isEqualTo(new BigDecimal("1.50"));
    }

    @Test
    void editedItemsReceiveFormattedNumbers() {
        FixedWidthRecord r = FixedWidthRecord.spaces(LAYOUT, RecordEncoding.ASCII);
        r.setDecimal("T-EDIT", new BigDecimal("-5.5"));
        assertThat(r.getString("T-EDIT")).isEqualTo("-  5.50");
        assertThat(r.get("T-EDIT")).isEqualTo("-  5.50");
    }

    @Test
    void lowValuesSpacesAndClassTests() {
        FixedWidthRecord r = FixedWidthRecord.spaces(LAYOUT, RecordEncoding.EBCDIC);
        Field id = r.field("T-ID");
        assertThat(r.isSpaces(id)).isTrue();
        assertThat(r.isNumeric(id)).isFalse();
        r.set(id, null);
        assertThat(r.isLowValues(id)).isTrue();
        assertThat(r.get(id)).isNull();
        r.setLong(id, 12);
        assertThat(r.isNumeric(id)).isTrue();
        r.setString("T-NAME", "12345");
        assertThat(r.isNumeric(r.field("T-NAME"))).isTrue();
        r.setString("T-NAME", "1234");
        assertThat(r.isNumeric(r.field("T-NAME"))).isFalse();
        r.fill(r.field("T-UAMT"), (byte) 0xFF);
        assertThat(r.isNumeric(r.field("T-UAMT"))).isFalse();
        assertThatThrownBy(() -> r.set("T-NAME", new Object())).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void decodeEncodeMapsAndViews() {
        FixedWidthRecord r = FixedWidthRecord.fromLine(LAYOUT, "ABC  0123M", RecordEncoding.ASCII);
        r.setDecimal("T-UAMT", BigDecimal.ONE);
        r.setLong("T-COUNT", 3);
        r.setDecimal("T-EDIT", BigDecimal.TEN);
        r.setLong("T-ID", 99);
        Map<String, Object> values = LAYOUT.decode(r);
        assertThat(values).containsEntry("T-NAME", "ABC  ").containsEntry("T-AMT", new BigDecimal("-12.34"))
                .containsEntry("T-EDIT", "  10.00").doesNotContainKey("FILLER");
        FixedWidthRecord copy = FixedWidthRecord.spaces(LAYOUT, RecordEncoding.ASCII);
        LAYOUT.encode(values, copy);
        assertThat(copy).isEqualTo(r).hasSameHashCodeAs(r);
        assertThat(copy.copy()).isEqualTo(copy).isNotSameAs(copy);
        assertThat(copy.layout()).isSameAs(LAYOUT);
        assertThat(copy.encoding()).isSameAs(RecordEncoding.ASCII);
        assertThat(copy.length()).isEqualTo(LAYOUT.length());
        assertThat(copy.text()).startsWith("ABC  0123M");
        assertThat(copy.toString()).startsWith("T-REC[ABC  0123M");
        RecordLayout flat = Copybook.parse("FLAT", "       01  FLAT  PIC X(" + LAYOUT.length() + ").").single();
        assertThat(copy.as(flat).getString("FLAT")).isEqualTo(copy.text());
        FixedWidthRecord ebcdic = new FixedWidthRecord(LAYOUT, RecordEncoding.EBCDIC.encode(copy.text()),
                RecordEncoding.EBCDIC);
        assertThat(ebcdic).isNotEqualTo(copy);
        assertThat(ebcdic.getString("T-NAME")).isEqualTo("ABC  ");
    }

    @Test
    void lengthAndBoundsChecks() {
        assertThatThrownBy(() -> FixedWidthRecord.fromLine(LAYOUT, "X".repeat(LAYOUT.length() + 1),
                RecordEncoding.ASCII)).isInstanceOf(RecordFormatException.class);
        assertThatThrownBy(() -> new FixedWidthRecord(LAYOUT, new byte[3], RecordEncoding.ASCII))
                .isInstanceOf(RecordFormatException.class);
        FixedWidthRecord bare = new FixedWidthRecord(new byte[3], RecordEncoding.ASCII);
        assertThat(bare.toString()).startsWith("record[");
        assertThatThrownBy(() -> bare.field("T-NAME")).isInstanceOf(IllegalStateException.class);
        assertThatThrownBy(() -> bare.getString(LAYOUT.field("T-ID"))).isInstanceOf(IndexOutOfBoundsException.class);
        FixedWidthRecord small = new FixedWidthRecord(Copybook.parse("S", "       01  S PIC X(3).").single(),
                new byte[3], RecordEncoding.ASCII);
        assertThatThrownBy(() -> LAYOUT.decode(small)).isInstanceOf(RecordFormatException.class);
        assertThatThrownBy(() -> LAYOUT.encode(Map.of(), small)).isInstanceOf(RecordFormatException.class);
    }
}
