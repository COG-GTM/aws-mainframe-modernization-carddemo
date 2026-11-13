package com.carddemo.common.cobol;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class ZonedDecimalTest {

    @ParameterizedTest
    @CsvSource({
            // ASCII text as found in app/data/ASCII, PIC, expected value
            "00000001940{, 2, 194.00",     // ACCT-CURR-BAL PIC S9(10)V99
            "00000020200{, 2, 2020.00",
            "0000005047G, 2, 504.77",      // DALYTRAN-AMT PIC S9(09)V99, 'G' = +7
            "0000000105}, 2, -10.50",      // '}' = -0
            "0000000123J, 2, -12.31",      // 'J' = -1
            "0000000123R, 0, -1239",       // 'R' = -9
            "0000000123p, 0, -1230",       // Micro Focus ASCII negative zero
    })
    void decodesAsciiOverpunch(String text, int scale, String expected) {
        assertThat(ZonedDecimal.decodeText(text, scale, true)).isEqualByComparingTo(expected);
    }

    @Test
    void ebcdicSignedRoundTrip() {
        byte[] plus = ZonedDecimal.encode(new BigDecimal("194.00"), 12, 2, true, SignMode.TRAILING_OVERPUNCH, DataEncoding.EBCDIC);
        assertThat(Hex.of(plus)).isEqualTo("F0F0F0F0F0F0F0F1F9F4F0C0");
        byte[] minus = ZonedDecimal.encode(new BigDecimal("-50.47"), 11, 2, true, SignMode.TRAILING_OVERPUNCH, DataEncoding.EBCDIC);
        assertThat(Hex.of(minus)).endsWith("F5F0F4D7");
        assertThat(ZonedDecimal.decode(minus, 0, minus.length, 2, true, SignMode.TRAILING_OVERPUNCH, DataEncoding.EBCDIC))
                .isEqualByComparingTo("-50.47");
    }

    @Test
    void ebcdicAndAsciiOverpunchAgreeThroughTheCodePage() {
        byte[] ebcdic = ZonedDecimal.encode(new BigDecimal("-12.31"), 11, 2, true, SignMode.TRAILING_OVERPUNCH, DataEncoding.EBCDIC);
        String viaCodePage = Ebcdic.toUnicode(ebcdic);
        byte[] ascii = ZonedDecimal.encode(new BigDecimal("-12.31"), 11, 2, true, SignMode.TRAILING_OVERPUNCH, DataEncoding.ASCII);
        assertThat(viaCodePage).isEqualTo(new String(ascii, DataEncoding.ASCII.charset())).isEqualTo("0000000123J");
    }

    @Test
    void unsignedUsesPlainDigits() {
        byte[] b = ZonedDecimal.encode(new BigDecimal("42"), 4, 0, false, SignMode.TRAILING_OVERPUNCH, DataEncoding.EBCDIC);
        assertThat(Hex.of(b)).isEqualTo("F0F0F4F2");
        assertThat(ZonedDecimal.decode(b, 0, 4, 0, false, SignMode.TRAILING_OVERPUNCH, DataEncoding.EBCDIC)).isEqualByComparingTo("42");
    }

    @Test
    void separateAndLeadingSigns() {
        byte[] ts = ZonedDecimal.encode(new BigDecimal("-7.5"), 3, 1, true, SignMode.TRAILING_SEPARATE, DataEncoding.ASCII);
        assertThat(new String(ts, DataEncoding.ASCII.charset())).isEqualTo("075-");
        byte[] ls = ZonedDecimal.encode(new BigDecimal("7.5"), 3, 1, true, SignMode.LEADING_SEPARATE, DataEncoding.EBCDIC);
        assertThat(Ebcdic.toUnicode(ls)).isEqualTo("+075");
        assertThat(ZonedDecimal.decode(ls, 0, 4, 1, true, SignMode.LEADING_SEPARATE, DataEncoding.EBCDIC)).isEqualByComparingTo("7.5");
        byte[] lo = ZonedDecimal.encode(new BigDecimal("-123"), 3, 0, true, SignMode.LEADING_OVERPUNCH, DataEncoding.EBCDIC);
        assertThat(Hex.of(lo)).isEqualTo("D1F2F3");
    }

    @Test
    void rejectsNonNumericData() {
        assertThatThrownBy(() -> ZonedDecimal.decodeText("12 4", 0, false)).isInstanceOf(CobolDataException.class);
        byte[] spaces = Ebcdic.fromUnicode("    ");
        assertThat(ZonedDecimal.isValid(spaces, 0, 4, false, SignMode.TRAILING_OVERPUNCH, DataEncoding.EBCDIC)).isFalse();
        assertThatThrownBy(() -> ZonedDecimal.encode(new BigDecimal("12345"), 4, 0, false, SignMode.TRAILING_OVERPUNCH, DataEncoding.ASCII))
                .isInstanceOf(CobolDataException.class);
    }
}
