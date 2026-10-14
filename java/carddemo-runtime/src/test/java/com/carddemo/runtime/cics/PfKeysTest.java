package com.carddemo.runtime.cics;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

/** CSSTRPFY YYYY-STORE-PFKEY. */
class PfKeysTest {

    @ParameterizedTest
    @CsvSource({"ENTER,ENTER", "CLEAR,CLEAR", "PA1,'PA1  '", "PA2,'PA2  '", "PF1,PFK01", "PF3,PFK03", "PF7,PFK07",
            "PF8,PFK08", "PF10,PFK10", "PF12,PFK12", "PF13,PFK01", "PF15,PFK03", "PF19,PFK07", "PF20,PFK08",
            "PF24,PFK12"})
    void mapsAidToCcardAid(AidKey aid, String expected) {
        assertThat(PfKeys.storePfKey(aid, "     ")).isEqualTo(expected);
    }

    @Test
    void pa3HasNoWhenAndLeavesTheFieldUnchanged() {
        assertThat(PfKeys.storePfKey(AidKey.PA3, "PFK05")).isEqualTo("PFK05");
    }
}
