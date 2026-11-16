package com.carddemo.legacyio;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import com.carddemo.legacyio.copybook.Copybook;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.util.HexFormat;
import java.util.NoSuchElementException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import org.junit.jupiter.params.provider.EnumSource;

class RecordCodecTest {

    private static final RecordLayout LAYOUT = RecordLayout.of(Copybook.parse("TEST", String.join("\n",
            "       01  TEST-REC.",
            "           05  NAME           PIC X(05).",
            "           05  AMOUNT         PIC S9(03)V99.",
            "           05  COUNT          PIC 9(03).",
            "           05  PACKED         PIC S9(05)V99 COMP-3.",
            "           05  UPACKED        PIC 9(03) COMP-3.",
            "           05  HALF           PIC S9(04) COMP.",
            "           05  FULL           PIC 9(09) COMP.")));

    @ParameterizedTest
    @CsvSource({
        "123.45,  001234E",
        "-123.45, 001234N",
        "0,       000000{",
        "-0.10,   000001}"
    })
    void zonedDecimalUsesOverpunchInAscii(BigDecimal amount, String expected) {
        RecordLayout layout = RecordLayout.of(Copybook.parse("Z",
                "       01  Z-REC.\n           05  AMT  PIC S9(05)V99."));
        RecordCodec codec = new RecordCodec(layout, Encoding.ASCII);

        byte[] bytes = codec.encode(Record.blank(layout).set("AMT", amount));

        assertThat(new String(bytes, StandardCharsets.ISO_8859_1)).isEqualTo(expected);
        assertThat(codec.decode(bytes).getDecimal("AMT")).isEqualByComparingTo(amount);
    }

    @Test
    void encodesEveryUsageInEbcdic() {
        RecordCodec codec = new RecordCodec(LAYOUT, Encoding.EBCDIC);
        Record record = Record.blank(LAYOUT)
                .set("NAME", "AB")
                .set("AMOUNT", new BigDecimal("-1.23"))
                .set("COUNT", 42L)
                .set("PACKED", new BigDecimal("12345.67"))
                .set("UPACKED", 7)
                .set("HALF", -2)
                .set("FULL", 123456789L);

        byte[] bytes = codec.encode(record);

        assertThat(HexFormat.of().formatHex(bytes)).isEqualTo(
                "c1c2404040" + "f0f0f1f2d3" + "f0f4f2" + "1234567c" + "007f" + "fffe" + "075bcd15");
        Record decoded = codec.decode(bytes);
        assertThat(decoded.getString("NAME")).isEqualTo("AB   ");
        assertThat(decoded.getDecimal("AMOUNT")).isEqualTo(new BigDecimal("-1.23"));
        assertThat(decoded.getLong("COUNT")).isEqualTo(42);
        assertThat(decoded.getDecimal("PACKED")).isEqualTo(new BigDecimal("12345.67"));
        assertThat(decoded.getLong("UPACKED")).isEqualTo(7);
        assertThat(decoded.getLong("HALF")).isEqualTo(-2);
        assertThat(decoded.getLong("FULL")).isEqualTo(123456789);
        assertThat(codec.encode(decoded)).isEqualTo(bytes);
    }

    @Test
    void negativePackedDecimalUsesSignNibbleD() {
        RecordCodec codec = new RecordCodec(LAYOUT, Encoding.EBCDIC);

        byte[] bytes = codec.encode(Record.blank(LAYOUT).set("PACKED", new BigDecimal("-0.05")));

        assertThat(HexFormat.of().formatHex(bytes, 13, 17)).isEqualTo("0000005d");
    }

    @ParameterizedTest
    @EnumSource(Encoding.class)
    void keepsInvalidNumericBytesUnchanged(Encoding encoding) {
        RecordCodec codec = new RecordCodec(LAYOUT, encoding);
        byte[] bytes = codec.encode(Record.blank(LAYOUT));
        bytes[5] = 0x00;
        bytes[13] = (byte) 0xFF;

        Record decoded = codec.decode(bytes);

        assertThat(decoded.invalidFields()).containsExactly("AMOUNT", "PACKED");
        assertThat(codec.encode(decoded)).isEqualTo(bytes);
        assertThatThrownBy(() -> decoded.getDecimal("AMOUNT")).isInstanceOf(InvalidFieldDataException.class);
    }

    @Test
    void rejectsValuesThatDoNotFitThePicture() {
        RecordCodec codec = new RecordCodec(LAYOUT, Encoding.ASCII);

        assertThatThrownBy(() -> codec.encode(Record.blank(LAYOUT).set("NAME", "TOOLONG")))
                .isInstanceOf(IllegalArgumentException.class);
        assertThatThrownBy(() -> codec.encode(Record.blank(LAYOUT).set("AMOUNT", new BigDecimal("1000"))))
                .isInstanceOf(IllegalArgumentException.class);
        assertThatThrownBy(() -> codec.encode(Record.blank(LAYOUT).set("AMOUNT", new BigDecimal("1.234"))))
                .isInstanceOf(IllegalArgumentException.class);
        assertThatThrownBy(() -> codec.encode(Record.blank(LAYOUT).set("COUNT", -1)))
                .isInstanceOf(IllegalArgumentException.class);
        assertThatThrownBy(() -> codec.encode(Record.blank(LAYOUT).set("COUNT", "12")))
                .isInstanceOf(IllegalArgumentException.class);
        assertThatThrownBy(() -> Record.blank(LAYOUT).set("NO-SUCH-FIELD", "x"))
                .isInstanceOf(NoSuchElementException.class);
    }

    @Test
    void rejectsCharactersOutsideTheCodePage() {
        assertThatThrownBy(() -> new RecordCodec(LAYOUT, Encoding.EBCDIC).encode(Record.blank(LAYOUT).set("NAME", "\u20ac")))
                .isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void rejectsRecordsOfTheWrongLength() {
        assertThatThrownBy(() -> new RecordCodec(LAYOUT, Encoding.ASCII).decode(new byte[3]))
                .isInstanceOf(IllegalArgumentException.class);
    }

    @ParameterizedTest
    @EnumSource(Encoding.class)
    void everyByteRoundTripsThroughText(Encoding encoding) {
        RecordLayout layout = RecordLayout.of(Copybook.parse("ALL", "       01  ALL-REC.\n           05  T PIC X(256)."));
        RecordCodec codec = new RecordCodec(layout, encoding);
        byte[] bytes = new byte[256];
        for (int i = 0; i < bytes.length; i++) {
            bytes[i] = (byte) i;
        }

        assertThat(codec.encode(codec.decode(bytes))).isEqualTo(bytes);
    }
    @ParameterizedTest
    @CsvSource({
        "ASCII,  313233",
        "ASCII,  30307d",
        "EBCDIC, f1f2f3",
        "EBCDIC, f0f0d0"
    })
    void keepsTheStoredSignOfUnchangedZonedFields(Encoding encoding, String hex) {
        RecordLayout layout = RecordLayout.of(Copybook.parse("S", "       01  S-REC.\n           05  N  PIC S9(3)."));
        RecordCodec codec = new RecordCodec(layout, encoding);
        byte[] bytes = HexFormat.of().parseHex(hex);

        Record record = codec.decode(bytes);

        assertThat(record.invalidFields()).isEmpty();
        assertThat(codec.encode(record)).isEqualTo(bytes);
    }

    @ParameterizedTest
    @CsvSource({"12345f, 12345.67", "12345a, 12345.67", "12345e, 12345.67", "12345b, -12345.67"})
    void keepsAlternatePackedSignNibbles(String signByte, BigDecimal ignored) {
        RecordLayout layout = RecordLayout.of(Copybook.parse("P",
                "       01  P-REC.\n           05  N  PIC S9(5) COMP-3."));
        RecordCodec codec = new RecordCodec(layout, Encoding.EBCDIC);
        byte[] bytes = HexFormat.of().parseHex(signByte);

        Record record = codec.decode(bytes);

        assertThat(record.getDecimal("N").abs()).isEqualByComparingTo("12345");
        assertThat(codec.encode(record)).isEqualTo(bytes);
    }

    @Test
    void setFieldsAndOtherEncodingsUseThePreferredSign() {
        RecordLayout layout = RecordLayout.of(Copybook.parse("S", "       01  S-REC.\n           05  N  PIC S9(3)."));
        Record record = new RecordCodec(layout, Encoding.ASCII).decode("123".getBytes(StandardCharsets.ISO_8859_1));

        assertThat(new RecordCodec(layout, Encoding.EBCDIC).encode(record)).containsExactly(0xf1, 0xf2, 0xc3);
        record.set("N", new BigDecimal("123"));
        assertThat(new String(new RecordCodec(layout, Encoding.ASCII).encode(record), StandardCharsets.ISO_8859_1))
                .isEqualTo("12C");
    }

    @Test
    void numericGettersAcceptEverySupportedInputType() {
        Record record = Record.blank(LAYOUT)
                .set("COUNT", 42)
                .set("HALF", (short) -2)
                .set("FULL", 123456789L)
                .set("PACKED", java.math.BigInteger.TEN);

        assertThat(record.getLong("COUNT")).isEqualTo(42);
        assertThat(record.getLong("HALF")).isEqualTo(-2);
        assertThat(record.getLong("FULL")).isEqualTo(123456789);
        assertThat(record.getDecimal("PACKED")).isEqualTo(new BigDecimal("10.00"));
        assertThatThrownBy(() -> Record.blank(LAYOUT).set("COUNT", "42").getDecimal("COUNT"))
                .isInstanceOf(IllegalArgumentException.class);
    }

    @ParameterizedTest
    @CsvSource({
        "PIC S9(2) COMP-3, 123c",
        "PIC 9(3) COMP,    ffff",
        "PIC S9(4) COMP,   7fff"
    })
    void keepsNumbersTooLargeForThePictureAsInvalidData(String clause, String hex) {
        RecordLayout layout = RecordLayout.of(Copybook.parse("O",
                "       01  O-REC.\n           05  N  " + clause + "."));
        RecordCodec codec = new RecordCodec(layout, Encoding.EBCDIC);
        byte[] bytes = HexFormat.of().parseHex(hex);

        Record record = codec.decode(bytes);

        assertThat(record.invalidFields()).containsExactly("N");
        assertThat(codec.encode(record)).isEqualTo(bytes);
    }
}
