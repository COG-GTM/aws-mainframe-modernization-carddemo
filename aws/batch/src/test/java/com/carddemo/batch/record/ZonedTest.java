package com.carddemo.batch.record;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import java.math.BigDecimal;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class ZonedTest {

    @ParameterizedTest
    @CsvSource({
        "0000007154D, 715.44",
        "0000006330{, 633.00",
        "0000000050}, -5.00",
        "0000001234R, -123.49",
        "0000000000{, 0.00",
    })
    void parsesOverpunchedSignedFields(String field, String expected) {
        assertThat(Zoned.parseSigned(field, 2)).isEqualByComparingTo(expected);
    }

    @Test
    void formatsRoundTrip() {
        for (String v : new String[] {"715.44", "-5.00", "0.00", "-123.49", "999999999.99"}) {
            String f = Zoned.formatSigned(new BigDecimal(v), 9, 2);
            assertThat(f).hasSize(11);
            assertThat(Zoned.parseSigned(f, 2)).isEqualByComparingTo(v);
        }
        assertThat(Zoned.formatSigned(new BigDecimal("-5.00"), 9, 2)).isEqualTo("0000000050}");
    }

    @Test
    void rejectsNonNumeric() {
        assertThatThrownBy(() -> Zoned.parseSigned("00000A0000{", 2)).isInstanceOf(IllegalArgumentException.class);
    }
}
