package com.carddemo.programs.cbtrn03c.support;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.math.BigDecimal;

import static org.junit.jupiter.api.Assertions.assertEquals;

/** Expected images captured from GnuCOBOL 3.1.2 (-std=ibm) MOVEs into the CVTRA07Y edited pictures. */
class PicEditorTest {

    @ParameterizedTest(name = "{0} -> [{1}] / [{2}]")
    @CsvSource(delimiter = '|', value = {
        "0             |'               '|'               '",
        "0.05          |'            .05'|'+           .05'",
        "-0.01         |'-           .01'|'-           .01'",
        "1.5           |'           1.50'|'+          1.50'",
        "-1234.5       |'-      1,234.50'|'-      1,234.50'",
        "999999999.99  |' 999,999,999.99'|'+999,999,999.99'",
        "-999999999.99 |'-999,999,999.99'|'-999,999,999.99'",
        "1000          |'       1,000.00'|'+      1,000.00'",
        "1000000000.01 |'            .01'|'+           .01'",
        "1234.567      |'       1,234.56'|'+      1,234.56'",
    })
    void editsLikeCobol(String value, String minusImage, String plusImage) {
        BigDecimal v = new BigDecimal(value);
        assertEquals(minusImage, PicEditor.edit(v, "-ZZZ,ZZZ,ZZZ.ZZ"));
        assertEquals(plusImage, PicEditor.edit(v, "+ZZZ,ZZZ,ZZZ.ZZ"));
    }
}
