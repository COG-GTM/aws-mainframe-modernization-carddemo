package com.carddemo.runtime.cobol;

import static org.assertj.core.api.Assertions.assertThat;

import java.math.BigDecimal;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class NumericEditedTest {

    @ParameterizedTest
    @CsvSource(delimiter = '|', value = {
        // fixed leading sign, Z suppression with comma insertion, truncation of high-order digits
        "+ZZZ,ZZZ,ZZZ.99 | 194.00          | '+        194.00'",
        "+ZZZ,ZZZ,ZZZ.99 | 2020            | '+      2,020.00'",
        "+ZZZ,ZZZ,ZZZ.99 | 0               | '+           .00'",
        "+ZZZ,ZZZ,ZZZ.99 | 0.5             | '+           .50'",
        "+ZZZ,ZZZ,ZZZ.99 | -1234.56        | '-      1,234.56'",
        "+ZZZ,ZZZ,ZZZ.99 | 9876543210.999  | '+876,543,210.99'",
        "+ZZZ,ZZZ,ZZZ.99 | -0.001          | '+           .00'",
        "-ZZZ,ZZ9.99     | 12.3            | '      12.30'",
        "-ZZZ,ZZ9.99     | -12.3           | '-     12.30'",
        "ZZZ9.99-        | -5              | '   5.00-'",
        "ZZZ9.99+        | 5               | '   5.00+'",
        "Z(4)9           | 42              | '   42'",
        "ZZZZ            | 0               | '    '",
        "ZZ.ZZ           | 0               | '     '",
        "9(3)            | 1234            | '234'",
        "++++9.99        | 12.5            | '  +12.50'",
        "----9.99        | -12.5           | '  -12.50'",
        "----9.99        | 12.5            | '   12.50'",
        "+++,++9         | 1234            | ' +1,234'",
        "99/99/99        | 123124          | '12/31/24'",
        "999B99          | 12345           | '123 45'",
    })
    void formats(String picture, String value, String expected) {
        assertThat(NumericEdited.format(new BigDecimal(value), picture)).isEqualTo(expected);
    }

    @org.junit.jupiter.api.Test
    void digitsAreAsciiWhateverTheDefaultLocale() {
        java.util.Locale saved = java.util.Locale.getDefault();
        try {
            java.util.Locale.setDefault(java.util.Locale.forLanguageTag("ar-EG-u-nu-arab"));
            assertThat(NumericEdited.format(new BigDecimal("194.00"), "+ZZZ,ZZZ,ZZZ.99")).isEqualTo("+        194.00");
        } finally {
            java.util.Locale.setDefault(saved);
        }
    }
}
