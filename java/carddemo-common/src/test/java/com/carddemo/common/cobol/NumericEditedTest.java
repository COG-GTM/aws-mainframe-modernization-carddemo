package com.carddemo.common.cobol;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.math.BigDecimal;

import static org.assertj.core.api.Assertions.assertThat;

class NumericEditedTest {

    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {
            // CVTRA07Y report pictures
            "-ZZZ,ZZZ,ZZZ.ZZ | -1234.5    | '-      1,234.50'",
            "-ZZZ,ZZZ,ZZZ.ZZ | 1234.5     | '       1,234.50'",
            "+ZZZ,ZZZ,ZZZ.ZZ | 50.47      | '+         50.47'",
            "+ZZZ,ZZZ,ZZZ.ZZ | -999999999.99 | '-999,999,999.99'",
            "+ZZZ,ZZZ,ZZZ.ZZ | 0          | '               '",   // all-Z zero blanks the item
            // CSDB2RWY WS-DISP-SQLCODE
            "----9           | -803       | ' -803'",
            "----9           | 100        | '  100'",
            "ZZZ9            | 0          | '   0'",
            "ZZZZ            | 0          | '    '",
            "$$$,$$9.99      | 5.5        | '     $5.50'",
            "$$$,$$9.99      | 12345.67   | '$12,345.67'",
            "***,**9.99      | 42         | '*****42.00'",
            "ZZ9.99CR        | -1.5       | '  1.50CR'",
            "ZZ9.99CR        | 1.5        | '  1.50  '",
            "99/99/99        | 123124     | '12/31/24'",
    })
    void formatsLikeCobol(String picture, String value, String expected) {
        assertThat(NumericEdited.format(new BigDecimal(value), picture)).isEqualTo(expected);
    }

    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {
            "-ZZZ,ZZZ,ZZZ.ZZ | '-      1,234.50' | -1234.50",
            "+ZZZ,ZZZ,ZZZ.ZZ | '+         50.47' | 50.47",
            "ZZ9.99CR        | '  1.50CR'        | -1.50",
    })
    void deEdits(String picture, String text, String expected) {
        assertThat(NumericEdited.parse(text, PicClause.parse(picture))).isEqualByComparingTo(expected);
    }
}
