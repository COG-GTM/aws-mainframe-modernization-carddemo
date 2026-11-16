package com.carddemo.legacyio;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.util.HexFormat;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.EnumSource;

class RecordCodecTest {

    private static final RecordLayout NUMBERS = Copybook.parse("NUMBERS", """
                   01  NUMBERS.
                       05  ZONED-SIGNED        PIC S9(4)V99.
                       05  ZONED-UNSIGNED      PIC 9(4).
                       05  PACKED-SIGNED       PIC S9(5)V99 COMP-3.
                       05  PACKED-UNSIGNED     PIC 9(3) COMP-3.
                       05  BINARY-SIGNED       PIC S9(4) COMP.
                       05  BINARY-UNSIGNED     PIC 9(9) COMP.
                       05  TEXT                PIC X(5).
            """).layout();

    @ParameterizedTest
    @CsvSource({"12.30, 00123{", "12.34, 00123D", "-12.30, 00123}", "-12.39, 00123R", "0, 00000{"})
    void zonedDecimalUsesEbcdicStyleOverpunch(String value, String text) {
        RecordCodec codec = new RecordCodec(LegacyEncoding.ASCII);
        Record record = Record.initialized(NUMBERS).set("ZONED-SIGNED", new BigDecimal(value));
        byte[] bytes = codec.encode(record);
        assertThat(new String(bytes, 0, 6, StandardCharsets.ISO_8859_1)).isEqualTo(text);
        assertThat(codec.decode(NUMBERS, bytes).getDecimal("ZONED-SIGNED")).isEqualByComparingTo(value);
    }

    @Test
    void ebcdicOverpunchIsZoneNibble() {
        RecordCodec codec = new RecordCodec(LegacyEncoding.EBCDIC);
        byte[] bytes = codec.encode(Record.initialized(NUMBERS).set("ZONED-SIGNED", new BigDecimal("-12.34")));
        assertThat(HexFormat.of().formatHex(bytes, 0, 6)).isEqualTo("f0f0f1f2f3d4");
        bytes = codec.encode(Record.initialized(NUMBERS).set("ZONED-SIGNED", new BigDecimal("12.34")));
        assertThat(HexFormat.of().formatHex(bytes, 0, 6)).isEqualTo("f0f0f1f2f3c4");
    }

    @Test
    void unsignedZonedReadsPlainDigits() {
        RecordCodec codec = new RecordCodec(LegacyEncoding.ASCII);
        byte[] bytes = codec.encode(Record.initialized(NUMBERS).set("ZONED-UNSIGNED", 42));
        assertThat(new String(bytes, 6, 4, StandardCharsets.ISO_8859_1)).isEqualTo("0042");
    }

    @Test
    void packedDecimalCarriesSignInLastNibble() {
        RecordCodec codec = new RecordCodec(LegacyEncoding.EBCDIC);
        Record record = Record.initialized(NUMBERS)
                .set("PACKED-SIGNED", new BigDecimal("-12345.67"))
                .set("PACKED-UNSIGNED", 789);
        byte[] bytes = codec.encode(record);
        assertThat(HexFormat.of().formatHex(bytes, 10, 14)).isEqualTo("1234567d");
        assertThat(HexFormat.of().formatHex(bytes, 14, 16)).isEqualTo("789f");
        Record decoded = codec.decode(NUMBERS, bytes);
        assertThat(decoded.getDecimal("PACKED-SIGNED")).isEqualByComparingTo("-12345.67");
        assertThat(decoded.getLong("PACKED-UNSIGNED")).isEqualTo(789);
    }

    @Test
    void binaryIsBigEndianTwosComplement() {
        RecordCodec codec = new RecordCodec(LegacyEncoding.EBCDIC);
        Record record = Record.initialized(NUMBERS).set("BINARY-SIGNED", -2).set("BINARY-UNSIGNED", 123456789);
        byte[] bytes = codec.encode(record);
        assertThat(HexFormat.of().formatHex(bytes, 16, 18)).isEqualTo("fffe");
        assertThat(HexFormat.of().formatHex(bytes, 18, 22)).isEqualTo("075bcd15");
        Record decoded = codec.decode(NUMBERS, bytes);
        assertThat(decoded.getLong("BINARY-SIGNED")).isEqualTo(-2);
        assertThat(decoded.getLong("BINARY-UNSIGNED")).isEqualTo(123456789);
    }

    @ParameterizedTest
    @EnumSource(LegacyEncoding.class)
    void initializedRecordRoundTrips(LegacyEncoding encoding) {
        RecordCodec codec = new RecordCodec(encoding);
        Record record = Record.initialized(NUMBERS).set("TEXT", "ab");
        Record decoded = codec.decode(NUMBERS, codec.encode(record));
        assertThat(decoded).isEqualTo(record);
        assertThat(decoded.getString("TEXT")).isEqualTo("ab   ");
    }

    @Test
    void invalidNumericBytesArePreservedVerbatim() {
        RecordCodec codec = new RecordCodec(LegacyEncoding.EBCDIC);
        byte[] bytes = codec.encode(Record.initialized(NUMBERS));
        bytes[0] = 0x40;
        bytes[10] = (byte) 0xAB;
        Record decoded = codec.decode(NUMBERS, bytes);
        assertThat(decoded.invalidFields()).containsExactly("ZONED-SIGNED", "PACKED-SIGNED");
        assertThat(decoded.isValid("ZONED-UNSIGNED")).isTrue();
        assertThat(codec.encode(decoded)).isEqualTo(bytes);
        assertThatThrownBy(() -> decoded.getDecimal("ZONED-SIGNED")).isInstanceOf(IllegalStateException.class);
    }

    @Test
    void untouchedSignedFieldsKeepTheirOriginalSignRepresentation() {
        RecordCodec codec = new RecordCodec(LegacyEncoding.ASCII);
        byte[] bytes = codec.encode(Record.initialized(NUMBERS));
        System.arraycopy("001234".getBytes(StandardCharsets.ISO_8859_1), 0, bytes, 0, 6);
        Record decoded = codec.decode(NUMBERS, bytes);
        assertThat(decoded.getDecimal("ZONED-SIGNED")).isEqualByComparingTo("12.34");
        assertThat(codec.encode(decoded)).isEqualTo(bytes);

        decoded.set("ZONED-SIGNED", new BigDecimal("12.34"));
        assertThat(new String(codec.encode(decoded), 0, 6, StandardCharsets.ISO_8859_1)).isEqualTo("00123D");
    }

    @Test
    void setRejectsValuesThatDoNotFit() {
        Record record = Record.initialized(NUMBERS);
        assertThatThrownBy(() -> record.set("ZONED-SIGNED", new BigDecimal("12345.00")))
                .isInstanceOf(IllegalArgumentException.class);
        assertThatThrownBy(() -> record.set("ZONED-SIGNED", new BigDecimal("1.234")))
                .isInstanceOf(IllegalArgumentException.class);
        assertThatThrownBy(() -> record.set("ZONED-UNSIGNED", -1)).isInstanceOf(IllegalArgumentException.class);
        assertThatThrownBy(() -> record.set("TEXT", "too long")).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void cp037MapsEveryByteBothWays() {
        byte[] all = new byte[256];
        for (int i = 0; i < 256; i++) {
            all[i] = (byte) i;
        }
        String text = LegacyEncoding.EBCDIC.decode(all, 0, 256);
        byte[] back = new byte[256];
        LegacyEncoding.EBCDIC.encode(text, back, 0);
        assertThat(back).isEqualTo(all);
        assertThat(LegacyEncoding.EBCDIC.decode(new byte[] {(byte) 0xC1, (byte) 0xF0, 0x40, (byte) 0xC0}, 0, 4))
                .isEqualTo("A0 {");
    }
}
