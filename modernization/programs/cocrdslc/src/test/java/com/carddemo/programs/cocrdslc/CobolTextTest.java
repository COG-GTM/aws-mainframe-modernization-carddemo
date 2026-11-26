package com.carddemo.programs.cocrdslc;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class CobolTextTest {

    @Test
    void picPadsAndTruncates() {
        assertEquals("AB   ", CobolText.pic("AB", 5));
        assertEquals("ABC", CobolText.pic("ABCDEF", 3));
        assertEquals("   ", CobolText.pic(null, 3));
    }

    @Test
    void isNumericRequiresEveryByteToBeADigit() {
        assertTrue(CobolText.isNumeric("00000000050"));
        assertFalse(CobolText.isNumeric("50         "));
        assertFalse(CobolText.isNumeric("-5"));
        assertFalse(CobolText.isNumeric(""));
    }

    @Test
    void zonedValueUsesEbcdicDigitNibbles() {
        assertEquals(50, CobolText.zonedValue("00000000050"));
        assertEquals(0, CobolText.zonedValue("0          "));
        assertEquals(0, CobolText.zonedValue(CobolText.lowValues(4)));
        assertEquals(500, CobolText.zonedValue("5  "));
        assertEquals(-1, CobolText.zonedValue("*"));
        assertEquals(1, CobolText.zonedValue("A"));
    }

    @Test
    void zonedKeepsLowOrderDigits() {
        assertEquals("00042", CobolText.zoned(42, 5));
        assertEquals("345", CobolText.zoned(12345, 3));
    }

    @Test
    void binaryToAlphaDropsSignAndLeftJustifiesNineDigits() {
        assertEquals("000000013 ", CobolText.binaryS9_9ToAlpha(13, 10));
        assertEquals("000000013 ", CobolText.binaryS9_9ToAlpha(-13, 10));
    }

    @Test
    void slicePadsShortSourceWithLowValues() {
        assertEquals("AB\u0000\u0000", CobolText.slice("XAB", 1, 4));
        assertEquals(CobolText.lowValues(3), CobolText.slice(null, 0, 3));
    }

    @Test
    void thisProgCommareaPadsShortFieldsToTheirPictures() {
        ThisProgCommarea t = ThisProgCommarea.lowValues();
        t.caFromProgram = "MENU";
        t.caFromTranid = "CM00";
        assertEquals("MENU    CM00", t.toFixedWidth());
        assertEquals("CM00", ThisProgCommarea.parse(t.toFixedWidth()).caFromTranid);
    }
}
