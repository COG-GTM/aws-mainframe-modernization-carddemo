package com.carddemo.runtime.cics;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/** CSSTRPFY YYYY-STORE-PFKEY. */
class PfKeysTest {

    @ParameterizedTest
    @CsvSource(value = {"ENTER,ENTER", "CLEAR,CLEAR", "PA1,PA1  ", "PA2,PA2  ", "PF1,PFK01", "PF3,PFK03",
            "PF12,PFK12", "PF13,PFK01", "PF15,PFK03", "PF24,PFK12", "PA3,XXXXX"}, ignoreLeadingAndTrailingWhitespace = false)
    void mapsAndFolds(AidKey aid, String expected) {
        assertThat(PfKeys.storePfKey(aid, "XXXXX")).isEqualTo(expected);
    }
}
