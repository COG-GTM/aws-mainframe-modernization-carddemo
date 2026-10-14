package com.carddemo.runtime.cics;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

/** CSSTRPFY YYYY-STORE-PFKEY lines 4-71. */
class PfKeysTest {

    @Test
    void mapsEveryAidLikeTheCopybook() {
        assertThat(PfKeys.storePfKey(AidKey.ENTER)).isEqualTo("ENTER");
        assertThat(PfKeys.storePfKey(AidKey.CLEAR)).isEqualTo("CLEAR");
        assertThat(PfKeys.storePfKey(AidKey.PA1)).isEqualTo("PA1  ");
        assertThat(PfKeys.storePfKey(AidKey.PA2)).isEqualTo("PA2  ");
        assertThat(PfKeys.storePfKey(AidKey.PA3)).isNull();
        assertThat(PfKeys.storePfKey(AidKey.PF1)).isEqualTo("PFK01");
        assertThat(PfKeys.storePfKey(AidKey.PF3)).isEqualTo("PFK03");
        assertThat(PfKeys.storePfKey(AidKey.PF12)).isEqualTo("PFK12");
        // PF13-PF24 fold onto PFK01-PFK12
        assertThat(PfKeys.storePfKey(AidKey.PF13)).isEqualTo("PFK01");
        assertThat(PfKeys.storePfKey(AidKey.PF15)).isEqualTo("PFK03");
        assertThat(PfKeys.storePfKey(AidKey.PF24)).isEqualTo("PFK12");
    }
}
