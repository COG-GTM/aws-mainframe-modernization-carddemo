package com.carddemo.common.record;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.carddemo.common.record.layout.AccountRecordLayout;
import com.carddemo.common.record.layout.CardXrefRecordLayout;
import com.carddemo.common.record.layout.TranCatBalRecordLayout;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.util.HexFormat;
import java.util.List;
import org.junit.jupiter.api.Test;

class CobolRecordTest {

    private static final RecordLayout SIGNED = signedLayout();

    private static RecordLayout signedLayout() {
        RecordLayout.Builder b = RecordLayout.builder("TESTSIGN", "SIGN-RECORD");
        b.numeric("ZONED", "S9(3)V99");
        b.numeric("PACKED", "S9(3)V99 COMP-3");
        b.numeric("BINARY", "S9(4) COMP");
        b.numeric("UBINARY", "9(9) COMP");
        b.alpha("TEXT", "X(4)");
        return b.build(5 + 3 + 2 + 4 + 4);
    }

    private static NumericField numeric(String name) {
        return (NumericField) SIGNED.field(name);
    }

    @Test
    void negativeValuesInBothCodePages() {
        for (CodePage codePage : CodePage.values()) {
            CobolRecord record = CobolRecord.initialized(SIGNED, codePage)
                    .set(numeric("ZONED"), new BigDecimal("-123.45"))
                    .set(numeric("PACKED"), new BigDecimal("-123.45"))
                    .set(numeric("BINARY"), -1234)
                    .set(numeric("UBINARY"), 999_999_999L)
                    .set((AlphanumericField) SIGNED.field("TEXT"), "ab");
            byte[] bytes = record.toBytes();
            assertThat(HexFormat.of().formatHex(bytes, 5, 14)).isEqualTo("12345dfb2e3b9ac9ff");
            assertThat(record.get(numeric("ZONED"))).isEqualByComparingTo("-123.45");
            assertThat(record.get(numeric("PACKED"))).isEqualByComparingTo("-123.45");
            assertThat(record.getLong(numeric("BINARY"))).isEqualTo(-1234L);
            assertThat(record.getLong(numeric("UBINARY"))).isEqualTo(999_999_999L);
            assertThat(record.get((AlphanumericField) SIGNED.field("TEXT"))).isEqualTo("ab  ");
            assertThat(CobolRecord.of(SIGNED, codePage, bytes)).isEqualTo(record);
        }
        byte[] ascii = CobolRecord.initialized(SIGNED, CodePage.ASCII).set(numeric("ZONED"), new BigDecimal("-123.45")).toBytes();
        assertThat(new String(ascii, 0, 5, StandardCharsets.ISO_8859_1)).isEqualTo("1234N");
        byte[] ebcdic = CobolRecord.initialized(SIGNED, CodePage.EBCDIC).set(numeric("ZONED"), new BigDecimal("-123.45")).toBytes();
        assertThat(HexFormat.of().formatHex(ebcdic, 0, 5)).isEqualTo("f1f2f3f4d5");
    }

    @Test
    void zonedSignVariantsDecode() {
        assertThat(zoned(CodePage.ASCII, "1234}")).isEqualByComparingTo("-123.40");
        assertThat(zoned(CodePage.ASCII, "1234{")).isEqualByComparingTo("123.40");
        assertThat(zoned(CodePage.ASCII, "12345")).isEqualByComparingTo("123.45");
        assertThat(zoned(CodePage.EBCDIC, "f1f2f3f4c5")).isEqualByComparingTo("123.45");
        assertThat(zoned(CodePage.EBCDIC, "f1f2f3f4f5")).isEqualByComparingTo("123.45");
        assertThat(zoned(CodePage.EBCDIC, "f1f2f3f4b5")).isEqualByComparingTo("-123.45");
    }

    private static BigDecimal zoned(CodePage codePage, String value) {
        byte[] bytes = CobolRecord.initialized(SIGNED, codePage).toBytes();
        byte[] field = codePage == CodePage.ASCII ? value.getBytes(StandardCharsets.US_ASCII) : HexFormat.of().parseHex(value);
        System.arraycopy(field, 0, bytes, 0, 5);
        return CobolRecord.of(SIGNED, codePage, bytes).get(numeric("ZONED"));
    }

    @Test
    void invalidNumericBytesAreReportedWithTheField() {
        byte[] bytes = CobolRecord.initialized(SIGNED, CodePage.ASCII).toBytes();
        bytes[1] = ' ';
        CobolRecord record = CobolRecord.of(SIGNED, CodePage.ASCII, bytes);
        assertThat(record.isNumeric(numeric("ZONED"))).isFalse();
        assertThatThrownBy(() -> record.get(numeric("ZONED"))).hasMessageContaining("TESTSIGN ZONED");
    }

    @Test
    void moveRulesForNumericFields() {
        CobolRecord record = CobolRecord.initialized(SIGNED, CodePage.ASCII);
        record.set(numeric("ZONED"), new BigDecimal("1.239"));
        assertThat(record.get(numeric("ZONED"))).isEqualByComparingTo("1.23");
        record.set(numeric("ZONED"), new BigDecimal("-1.239"));
        assertThat(record.get(numeric("ZONED"))).isEqualByComparingTo("-1.23");
        assertThatThrownBy(() -> record.set(numeric("ZONED"), new BigDecimal("1000"))).hasMessageContaining("does not fit");
        assertThatThrownBy(() -> record.set(numeric("BINARY"), 10_000)).hasMessageContaining("does not fit");
        assertThatThrownBy(() -> record.set(numeric("UBINARY"), -1)).hasMessageContaining("unsigned");
        assertThatThrownBy(() -> record.set(AccountRecordLayout.ACCT_ID, 1)).hasMessageContaining("is not a field of");
    }

    @Test
    void lineSequentialReaderHandlesCrlfShortLinesAndMissingFinalNewline() throws IOException {
        String file = "0000000000000000000000000000000000000001\r\n"
                + "4111111111111111000000001\n"
                + "4111111111111112000000002000000000";
        RecordReader reader = new RecordReader(new ByteArrayInputStream(file.getBytes(StandardCharsets.US_ASCII)),
                CardXrefRecordLayout.LAYOUT, CodePage.ASCII, RecordFormat.LINE_SEQUENTIAL);
        CobolRecord first = reader.read();
        assertThat(reader.lineEnding()).isEqualTo("\r\n");
        assertThat(first.get(CardXrefRecordLayout.FILLER)).isEqualTo("0001          ");
        assertThat(first.getLong(CardXrefRecordLayout.XREF_ACCT_ID)).isZero();
        CobolRecord second = reader.read();
        assertThat(reader.lineEnding()).isEqualTo("\n");
        assertThat(second.getLong(CardXrefRecordLayout.XREF_CUST_ID)).isEqualTo(1L);
        assertThat(second.getTrimmed(CardXrefRecordLayout.FILLER)).isNull();
        assertThat(second.isNumeric(CardXrefRecordLayout.XREF_ACCT_ID)).isFalse();
        CobolRecord third = reader.read();
        assertThat(reader.lineEnding()).isEmpty();
        assertThat(third.get(CardXrefRecordLayout.XREF_CARD_NUM)).isEqualTo("4111111111111112");
        assertThat(reader.read()).isNull();
        assertThat(reader.recordNumber()).isEqualTo(3);

        assertThat(new String(RecordWriter.toBytes(List.of(second),
                RecordFormat.LINE_SEQUENTIAL.trimmingTrailingSpaces()), StandardCharsets.US_ASCII))
                .isEqualTo("4111111111111111000000001\n");
    }

    @Test
    void readerRejectsLongLinesAndShortFixedRecords() {
        String tooLong = "x".repeat(51) + "\n";
        RecordReader lines = new RecordReader(new ByteArrayInputStream(tooLong.getBytes(StandardCharsets.US_ASCII)),
                CardXrefRecordLayout.LAYOUT, CodePage.ASCII, RecordFormat.LINE_SEQUENTIAL);
        assertThatThrownBy(lines::read).isInstanceOf(IOException.class).hasMessageContaining("51 bytes");

        RecordReader fixed = new RecordReader(new ByteArrayInputStream(new byte[75]),
                CardXrefRecordLayout.LAYOUT, CodePage.EBCDIC, RecordFormat.FIXED);
        assertThatThrownBy(() -> {
            fixed.read();
            fixed.read();
        }).isInstanceOf(IOException.class).hasMessageContaining("short: 25 of 50");
        assertThatThrownBy(() -> new RecordReader(new ByteArrayInputStream(new byte[0]), CardXrefRecordLayout.LAYOUT,
                CodePage.EBCDIC, RecordFormat.LINE_SEQUENTIAL)).hasMessageContaining("ASCII");
    }

    @Test
    void groupKeyReadsAsOneValue() {
        CobolRecord balance = CobolRecord.initialized(TranCatBalRecordLayout.LAYOUT, CodePage.EBCDIC)
                .set(TranCatBalRecordLayout.TRANCAT_ACCT_ID, 1)
                .set(TranCatBalRecordLayout.TRANCAT_TYPE_CD, "01")
                .set(TranCatBalRecordLayout.TRANCAT_CD, 5);
        assertThat(balance.get(TranCatBalRecordLayout.TRAN_CAT_KEY)).isEqualTo("00000000001010005");
    }
}
