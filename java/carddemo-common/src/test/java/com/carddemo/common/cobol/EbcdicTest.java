package com.carddemo.common.cobol;

import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;

import static org.assertj.core.api.Assertions.assertThat;

class EbcdicTest {

    @Test
    void cp037RoundTrip() {
        String text = "ADMIN001MARGARET GOLD {}ABC-xyz 0123456789 $@#.,";
        byte[] ebcdic = Ebcdic.fromUnicode(text);
        assertThat(Ebcdic.toUnicode(ebcdic)).isEqualTo(text);
        assertThat(ebcdic[0]).isEqualTo((byte) 0xC1);
        assertThat(Hex.of(Ebcdic.fromUnicode("{}0 "))).isEqualTo("C0D0F040");
    }

    @Test
    void byteLevelConversionIsReversible() {
        byte[] all = new byte[256];
        for (int i = 0; i < 256; i++) {
            all[i] = (byte) i;
        }
        assertThat(Ebcdic.toEbcdic(Ebcdic.toAscii(all))).isEqualTo(all);
        assertThat(new String(Ebcdic.toAscii(Ebcdic.fromUnicode("Hello")), StandardCharsets.ISO_8859_1)).isEqualTo("Hello");
    }

    @Test
    void figurativeConstants() {
        assertThat(Ebcdic.isSpaces(Ebcdic.fromUnicode("   "), 0, 3)).isTrue();
        assertThat(Ebcdic.isLowValues(new byte[3], 0, 3)).isTrue();
        assertThat(Ebcdic.isHighValues(new byte[] {(byte) 0xFF, (byte) 0xFF}, 0, 2)).isTrue();
    }
}
