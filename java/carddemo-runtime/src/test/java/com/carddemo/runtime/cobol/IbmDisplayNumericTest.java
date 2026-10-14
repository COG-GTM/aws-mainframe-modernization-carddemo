package com.carddemo.runtime.cobol;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import org.junit.jupiter.api.Test;

class IbmDisplayNumericTest {

    @Test
    void lowNibblesOfEachByte() {
        assertThat(IbmDisplayNumeric.unsignedValue("12")).isEqualTo(12);
        assertThat(IbmDisplayNumeric.unsignedValue(" 1")).isEqualTo(1);
        assertThat(IbmDisplayNumeric.unsignedValue("1 ")).isEqualTo(10);
        assertThat(IbmDisplayNumeric.unsignedValue("  ")).isZero();
        assertThat(IbmDisplayNumeric.unsignedValue("\u0000\u0000")).isZero();
        assertThat(IbmDisplayNumeric.unsignedValue("A1")).isEqualTo(11);
    }

    @Test
    void nibbleAboveNineIsADataException() {
        // '+' is X'4E' in EBCDIC
        assertThatThrownBy(() -> IbmDisplayNumeric.unsignedValue("1+")).isInstanceOf(NumberFormatException.class);
    }

    @Test
    void alphanumericToUnsignedMoveFixesLastZone() {
        assertThat(IbmDisplayNumeric.moveAlphanumericToUnsigned("00000000001")).isEqualTo("00000000001");
        assertThat(IbmDisplayNumeric.moveAlphanumericToUnsigned("1          ")).isEqualTo("1         0");
        assertThat(IbmDisplayNumeric.moveAlphanumericToUnsigned("abc")).isEqualTo("ab3");
    }
}
