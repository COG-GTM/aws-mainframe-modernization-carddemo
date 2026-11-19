package com.carddemo.common.cobol;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class CodePageTest {

    @Test
    void cp037KnownBytes() {
        assertThat(CodePage.CP037.encode("A1 {}")).containsExactly((byte) 0xC1, (byte) 0xF1, (byte) 0x40, (byte) 0xC0, (byte) 0xD0);
        assertThat(CodePage.CP037.space()).isEqualTo((byte) 0x40);
        assertThat(CodePage.ASCII.space()).isEqualTo((byte) 0x20);
    }

    @Test
    void convertsRecordImagesBothWays() {
        String text = "0000005047GPOS TERM 4111111111111111";
        byte[] ascii = CodePage.ASCII.encode(text);
        byte[] ebcdic = CodePage.ASCII.convert(ascii, CodePage.CP037);
        assertThat(ebcdic).isNotEqualTo(ascii);
        assertThat(CodePage.CP037.convert(ebcdic, CodePage.ASCII)).containsExactly(ascii);
        assertThat(CodePage.CP037.decode(ebcdic)).isEqualTo(text);
    }

    @Test
    void asciiCopyRoundTripsEveryByte() {
        byte[] all = new byte[256];
        for (int i = 0; i < 256; i++) {
            all[i] = (byte) i;
        }
        assertThat(CodePage.ASCII.encode(CodePage.ASCII.decode(all))).containsExactly(all);
    }

    @Test
    void unmappableCharacterFailsInsteadOfSubstituting() {
        assertThatThrownBy(() -> CodePage.CP037.encode("\u20AC")).isInstanceOf(IllegalArgumentException.class);
    }

    @Test
    void parsesNames() {
        assertThat(CodePage.parse("ascii")).isEqualTo(CodePage.ASCII);
        assertThat(CodePage.parse("EBCDIC")).isEqualTo(CodePage.CP037);
        assertThat(CodePage.parse(" cp037 ")).isEqualTo(CodePage.CP037);
        assertThatThrownBy(() -> CodePage.parse("UTF-8")).isInstanceOf(IllegalArgumentException.class);
    }
}
