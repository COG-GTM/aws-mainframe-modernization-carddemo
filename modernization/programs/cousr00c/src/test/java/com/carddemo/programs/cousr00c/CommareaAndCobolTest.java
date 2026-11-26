package com.carddemo.programs.cousr00c;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class CommareaAndCobolTest {

    @Test
    void cu00InfoIs34BytesAndRoundTrips() {
        Cu00Info c = new Cu00Info();
        c.setUsridFirst("USER0001");
        c.setUsridLast("USER0010");
        c.setPageNum(12);
        c.setNextPageYes();
        c.setUsrSelFlg('U');
        c.setUsrSelected("USER0003");
        String raw = c.toFixedWidth();
        assertEquals("USER0001USER001000000012YUUSER0003", raw);
        assertEquals(Cu00Info.LENGTH, raw.length());
        Cu00Info back = Cu00Info.fromFixedWidth(raw);
        assertTrue(back.isNextPageYes());
        assertFalse(back.isNextPageNo());
        assertEquals(12, back.getPageNum());
        assertEquals('U', back.getUsrSelFlg());
    }

    @Test
    void nextPageFlagDefaultsToN() {
        assertTrue(new Cu00Info().isNextPageNo());
    }

    @Test
    void pageNumIsPic9of8AndTruncatesHighOrder() {
        Cu00Info c = new Cu00Info();
        c.setPageNum(123_456_789L);
        assertEquals(23_456_789L, c.getPageNum());
    }

    @Test
    void cousr00cCommareaIs194BytesAndShortInputIsSpacePadded() {
        assertEquals(194, Cousr00cCommarea.LENGTH);
        String general = Fixtures.adminMenuCommarea();
        assertEquals(160, general.length());
        Cousr00cCommarea c = Cousr00cCommarea.fromFixedWidth(general);
        assertEquals(" ".repeat(16), c.cu00().getUsridFirst() + c.cu00().getUsridLast());
        assertEquals(" ".repeat(8), c.cu00().getUsrSelected());
        assertEquals(0, c.cu00().getPageNum(), "spaces in PIC 9(08) read as zero");
        assertEquals(' ', c.cu00().getNextPageFlg());
        assertEquals("ADMIN001", c.general().getUserId());
    }

    @Test
    void alnumMovePadsAndTruncates() {
        assertEquals("AB  ", Cobol.alnum("AB", 4));
        assertEquals("ABCD", Cobol.alnum("ABCDEF", 4));
        assertEquals("    ", Cobol.alnum(null, 4));
    }

    @Test
    void spacesOrLowValuesPredicate() {
        assertTrue(Cobol.isSpacesOrLowValues("    "));
        assertTrue(Cobol.isSpacesOrLowValues("\u0000\u0000"));
        assertFalse(Cobol.isSpacesOrLowValues(" \u0000"));
        assertFalse(Cobol.isSpacesOrLowValues(" A"));
    }

    @Test
    void numericDisplayZeroFillsAndTruncates() {
        assertEquals("00000013", Cobol.numericDisplay(13, 8));
        assertEquals("000000019", Cobol.numericDisplay(19, 9));
        assertEquals("23", Cobol.numericDisplay(123, 2));
        assertEquals(0, Cobol.parseNumericDisplay("        "));
        assertEquals(42, Cobol.parseNumericDisplay("00000042"));
    }

    @Test
    void numericDisplayIgnoresDefaultLocaleDigits() {
        java.util.Locale saved = java.util.Locale.getDefault();
        try {
            java.util.Locale.setDefault(java.util.Locale.forLanguageTag("ar-EG-u-nu-arab"));
            assertEquals("00000001", Cobol.numericDisplay(1, 8));
        } finally {
            java.util.Locale.setDefault(saved);
        }
    }

    @Test
    void ebcdicOrderPutsSpaceBeforeDigitsAndLettersBeforeDigits() {
        assertTrue(Cobol.EBCDIC_ORDER.compare("USER002 ", "USER0020") < 0);
        assertTrue(Cobol.EBCDIC_ORDER.compare("ZZZZZZZZ", "00000000") < 0, "letters < digits in EBCDIC");
        assertTrue(Cobol.EBCDIC_ORDER.compare("abc", "ABC") < 0, "lower case < upper case in EBCDIC");
        assertTrue(Cobol.EBCDIC_ORDER.compare("99999999", Cobol.highValues(8)) < 0);
        assertTrue(Cobol.EBCDIC_ORDER.compare(Cobol.lowValues(8), "        ") < 0);
    }
}
