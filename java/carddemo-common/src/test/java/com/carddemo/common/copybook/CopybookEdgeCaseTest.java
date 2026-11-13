package com.carddemo.common.copybook;

import com.carddemo.common.cobol.CobolDataException;
import com.carddemo.common.cobol.DataEncoding;
import com.carddemo.common.cobol.NumericEdited;
import com.carddemo.common.cobol.PicClause;
import com.carddemo.common.io.RecordFileReader;
import com.carddemo.common.io.RecordFormat;
import org.junit.jupiter.api.Test;

import java.io.ByteArrayInputStream;
import java.math.BigDecimal;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class CopybookEdgeCaseTest {

    @Test
    void parseIgnoresInsertionZeros() {
        PicClause pic = PicClause.parse("99099");
        assertThat(NumericEdited.format(new BigDecimal("1234"), pic)).isEqualTo("12034");
        assertThat(NumericEdited.parse("12034", pic)).isEqualByComparingTo("1234");
    }

    @Test
    void alphanumericIsNumericClassTest() {
        RecordLayout r = CopybookParser.parse("INLINE", """
                       01  WS-REC.
                           05  WS-CODE  PIC X(4).
                """).first();
        RecordView digits = RecordView.wrap(r, DataEncoding.ASCII, "1234".getBytes());
        RecordView mixed = RecordView.wrap(r, DataEncoding.ASCII, "12A4".getBytes());
        assertThat(digits.isNumeric(r.field("WS-CODE"))).isTrue();
        assertThat(mixed.isNumeric(r.field("WS-CODE"))).isFalse();
    }

    @Test
    void transcodingKeepsBinaryFillerBytes() {
        RecordLayout r = CopybookParser.parse("INLINE", """
                       01  WS-REC.
                           05  WS-NAME  PIC X(2).
                           05  FILLER   PIC S9(3) COMP-3.
                """).first();
        byte[] ascii = {'A', 'B', 0x12, 0x3C};
        byte[] ebcdic = RecordTranscoder.transcode(ascii, r, DataEncoding.ASCII, DataEncoding.EBCDIC);
        assertThat(ebcdic).containsExactly((byte) 0xC1, (byte) 0xC2, (byte) 0x12, (byte) 0x3C);
    }

    @Test
    void itemAfterLongerRedefinitionFollowsItsEnd() {
        RecordLayout r = CopybookParser.parse("INLINE", """
                       01  WS-REC.
                           05  WS-SHORT PIC X(2).
                           05  WS-LONG  REDEFINES WS-SHORT PIC X(4).
                           05  WS-NEXT  PIC X(1).
                """).first();
        assertThat(r.field("WS-NEXT").offset()).isEqualTo(4);
        assertThat(r.length()).isEqualTo(5);
    }

    @Test
    void readerClosesStreamWhenFirstRecordIsInvalid() {
        AtomicBoolean closed = new AtomicBoolean();
        ByteArrayInputStream in = new ByteArrayInputStream(new byte[] {1, 2, 3}) {
            @Override
            public void close() {
                closed.set(true);
            }
        };
        assertThatThrownBy(() -> new RecordFileReader(in, RecordFormat.FIXED, 10, DataEncoding.EBCDIC))
                .isInstanceOf(CobolDataException.class);
        assertThat(closed).isTrue();
    }
}
