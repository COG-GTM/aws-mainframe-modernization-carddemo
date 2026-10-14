package com.carddemo.runtime.cics;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

/** CSSTRPFY YYYY-STORE-PFKEY. */
class PfKeysTest {

    @Test
    void mapsEveryAid() {
        assertThat(PfKeys.storePfKey(AidKey.ENTER)).isEqualTo("ENTER");
        assertThat(PfKeys.storePfKey(AidKey.CLEAR)).isEqualTo("CLEAR");
        assertThat(PfKeys.storePfKey(AidKey.PA1)).isEqualTo("PA1  ");
        assertThat(PfKeys.storePfKey(AidKey.PA2)).isEqualTo("PA2  ");
        assertThat(PfKeys.storePfKey(AidKey.PF1)).isEqualTo("PFK01");
        assertThat(PfKeys.storePfKey(AidKey.PF12)).isEqualTo("PFK12");
        // PF13-24 fold onto PF1-12
        assertThat(PfKeys.storePfKey(AidKey.PF13)).isEqualTo("PFK01");
        assertThat(PfKeys.storePfKey(AidKey.PF15)).isEqualTo("PFK03");
        assertThat(PfKeys.storePfKey(AidKey.PF24)).isEqualTo("PFK12");
    }

    @Test
    void pa3LeavesTheFieldUnchanged() {
        assertThat(PfKeys.storePfKey(AidKey.PA3)).isEqualTo("     ");
        assertThat(PfKeys.storePfKey(AidKey.PA3, "PFK07")).isEqualTo("PFK07");
    }
}
