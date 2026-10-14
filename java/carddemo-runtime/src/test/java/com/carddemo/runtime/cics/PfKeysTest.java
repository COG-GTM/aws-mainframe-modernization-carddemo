package com.carddemo.runtime.cics;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

/** CSSTRPFY YYYY-STORE-PFKEY. */
class PfKeysTest {

    @Test
    void mapsEveryAid() {
        assertThat(PfKeys.storePfKey(AidKey.ENTER, "x")).isEqualTo("ENTER");
        assertThat(PfKeys.storePfKey(AidKey.CLEAR, "x")).isEqualTo("CLEAR");
        assertThat(PfKeys.storePfKey(AidKey.PA1, "x")).isEqualTo("PA1  ");
        assertThat(PfKeys.storePfKey(AidKey.PA2, "x")).isEqualTo("PA2  ");
        assertThat(PfKeys.storePfKey(AidKey.PF1, "x")).isEqualTo("PFK01");
        assertThat(PfKeys.storePfKey(AidKey.PF12, "x")).isEqualTo("PFK12");
    }

    @Test
    void foldsPf13To24AndLeavesPa3Unchanged() {
        assertThat(PfKeys.storePfKey(AidKey.PF13, "x")).isEqualTo("PFK01");
        assertThat(PfKeys.storePfKey(AidKey.PF15, "x")).isEqualTo("PFK03");
        assertThat(PfKeys.storePfKey(AidKey.PF24, "x")).isEqualTo("PFK12");
        assertThat(PfKeys.storePfKey(AidKey.PA3, "ENTER")).isEqualTo("ENTER");
    }
}
